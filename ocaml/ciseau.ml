(* TODOs:
 *  - implement redo command, and do n times
 *  - properly append terminal buffer bitblit with end-of-line padding, and end-of-file padding
 *      then fix recenter view to really adjust to middle when at end-of-file
 *  - add selection of current word (with highlight), go to next selection, search function
 *  - add next/prev number
 *  - color up the cursor and active line
 *  - finish implementing terminal save and restore by restoring cursor position
 *)


(* remappings *)

let alen = Array.length ;;
let blen = Bytes.length ;;
let slen = String.length ;;


module Utils = struct

  let inc x = x + 1 ;;
  let dec x = x - 1 ;;

  type 'a either = Left of 'a | Right of exn ;;

  let try_to action =
    try let x = action () in Left x
    with e -> Right e
  ;;

  let try_finally action cleanup =
    let rez = try_to action in
    cleanup () ;
    match rez with
    | Left success  -> success
    | Right error   -> raise error
  ;;

  (* fp utils *)

  let (>>) f g x = g (f x) ;;
  let (<<) g f x = g (f x) ;;

  (* string utils *)

  let padding l s = (String.make (l - (slen s)) ' ') ;;
  let postpad l s = s ^ (padding l s) ;;
  let prepad l s = (padding l s) ^ s ;;

  let string_of_char c = String.make 1 c ;;

  let truncate l s =
    if slen s > l then String.sub s 0 l else s ;;

  let is_empty s = (slen s = 0) ;;

  (* char utils *)

  let is_space      chr = (chr = ' ') || (chr = '\t') || (chr = '\r') || (chr = '\n') ;;
  let is_letter     chr = (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z')) ;;
  let is_digit      chr = ('0' <= chr) && (chr <= '9') ;;
  let is_alphanum   chr = (is_digit chr) || (is_letter chr) ;;
  let is_printable  chr = (' ' <= chr) && (chr <= '~') ;;

  (* IO utils *)

  exception IOError ;;
  exception IOUnexpectedByteNumber of int ;;

  (* replacement for input_char which considers 0 as Enf_of_file *)
  let next_char =
    (* WARN not thread safe *)
    let buffer = Bytes.make 1 'z' in
    let rec one_byte_reader () =
      match Unix.read Unix.stdin buffer 0 1 with
      | 1   -> Bytes.get buffer 0 |> Char.code
      | 0   -> one_byte_reader ()     (* timeout *)
      | -1  -> raise IOError          (* TODO: errno *)
      | n   -> raise (IOUnexpectedByteNumber n)  (* cannot happen since we ask for 1 byte only *)
    in one_byte_reader ;;

  let write fd buffer len =
    match Unix.write fd buffer 0 len with
    | n when n = len  -> ()
    | -1              -> raise IOError
    | n               -> raise (IOUnexpectedByteNumber n)
  ;;

  let write_string fd s =
    let buffer = (Bytes.of_string s) in
    write fd buffer (blen buffer)
  ;;

  let do_with_input_file chan fn =
    let action () = fn chan in
    let cleanup () = close_in chan in
    try_finally action cleanup

  let slurp f =
    let rec loop lines ch = match input_line ch with
    | s                     -> loop (s :: lines) ch
    | exception End_of_file -> List.rev lines
    in
    do_with_input_file (open_in f) (loop []) ;;

  let save f lines = "TODO"

end


module Vec2 = struct

  type t = {
    x : int ;
    y : int ;
  } ;;

  let zero = { x = 0; y = 0 } ;;

  let make (x, y) = { x = x ; y = y } ;;
  let add t1 t2 = { x = t1.x + t2.x ; y = t1.y + t2.y } ;;
  let sub t1 t2 = { x = t1.x - t2.x ; y = t1.y - t2.y } ;;
  let to_string t = (string_of_int t.y) ^ "," ^ (string_of_int t.x) ;;
end


(* Mappings of character codes
 * char code -> char enum
 * char code -> char string repr
 *)
module Keys = struct

  type key_symbol = Unknown
    | Ctrl_c
    | Ctrl_d
    | Ctrl_j
    | Ctrl_k
    | Ctrl_u
    | Ctrl_z
    | Alt_h
    | Alt_j
    | Alt_k
    | Alt_l
    | ArrowUp
    | ArrowDown
    | ArrowRight
    | ArrowLeft
    | Lower_h
    | Lower_j
    | Lower_k
    | Lower_l
    | Lower_w
    | Lower_b
  ;;

  type key = {
    symbol  : key_symbol ;
    repr    : string ;
    code    : int ;
  }

  let unknown_key = {
    symbol  = Unknown ;
    repr    = "unknown" ;
    code    = 0 ;
  }

  let make_key s r c = { symbol = s; repr = r; code = c }

  let code_to_key_table = Array.make 256 unknown_key ;;
  code_to_key_table.(3)   <- make_key Ctrl_c      "Ctrl_c"        3 ;;
  code_to_key_table.(4)   <- make_key Ctrl_d      "Ctrl_d"        4 ;;
  code_to_key_table.(10)  <- make_key Ctrl_j      "Ctrl_j"        10 ;;
  code_to_key_table.(11)  <- make_key Ctrl_k      "Ctrl_k"        11 ;;
  code_to_key_table.(21)  <- make_key Ctrl_u      "Ctrl_u"        21 ;;
  code_to_key_table.(26)  <- make_key Ctrl_z      "Ctrl_z"        26 ;;
  code_to_key_table.(65)  <- make_key ArrowUp     "ArrowUp"       65 ;;
  code_to_key_table.(66)  <- make_key ArrowDown   "ArrowDown"     66 ;;
  code_to_key_table.(67)  <- make_key ArrowRight  "ArrowRight"    67 ;;
  code_to_key_table.(68)  <- make_key ArrowLeft   "ArrowLeft"     68 ;;
  code_to_key_table.(98)  <- make_key Lower_b     "w"             98 ;;
  code_to_key_table.(104) <- make_key Lower_h     "h"             104 ;;
  code_to_key_table.(106) <- make_key Lower_j     "j"             106 ;;
  code_to_key_table.(107) <- make_key Lower_k     "k"             107 ;;
  code_to_key_table.(108) <- make_key Lower_l     "l"             108 ;;
  code_to_key_table.(119) <- make_key Lower_w     "w"             119 ;;
  code_to_key_table.(153) <- make_key Alt_h       "Alt_h"         153 ;;
  code_to_key_table.(134) <- make_key Alt_j       "Alt_j"         134 ;;
  code_to_key_table.(154) <- make_key Alt_k       "Alt_k"         154 ;;
  code_to_key_table.(172) <- make_key Alt_l       "Alt_l"         172 ;;

  let code_to_key code =
    match code_to_key_table.(code) with
    | unknown when unknown.symbol = Unknown -> { unknown with code = code }
    | found                                 -> found
  ;;

end


module Bytevector = struct

  type t = {
    bytes : bytes ;
    len : int ;
  } ;;

  let zero = Char.chr 0 ;;

  let init len = {
    bytes = Bytes.make len zero ;
    len   = 0 ;
  } ;;

  let reset bytevec = { bytevec with len = 0 } ;;

  let scale size = size |> float |> ( *. ) 1.45 |> ceil |> truncate ;;

  let rec next_size needed_size size =
    if needed_size <= size then size else next_size needed_size (scale size) ;;

  let grow new_size bytes = Bytes.extend bytes 0 (new_size - (blen bytes)) ;;

  let ensure_size needed_size bytes =
    let current_size = (blen bytes) in
    if (needed_size <= current_size)
      then bytes
      else grow (next_size needed_size current_size) bytes
  ;;

  let append s t =
    let new_length = (slen s) + t.len in
    let new_bytes = ensure_size new_length t.bytes in
      Bytes.blit_string s 0 new_bytes t.len (slen s) ;
      {
        bytes = new_bytes ;
        len   = new_length ;
      }
  ;;

  let cat t bvec =
    let new_length = t.len + bvec.len in
    let new_bytes = ensure_size new_length t.bytes in
      Bytes.blit bvec.bytes 0 new_bytes t.len bvec.len ;
      {
        bytes = new_bytes ;
        len   = new_length ;
      }
  ;;

  let to_string bvec =
    Bytes.sub_string bvec.bytes 0 bvec.len ;;

  let write fd bytev =
    Utils.write fd bytev.bytes bytev.len ;;

end

(* main module for interacting with the terminal *)
module Term = struct

  external get_terminal_size : unit -> (int * int) = "get_terminal_size" ;;

  open Utils

  (* bypass buffered output to the stdout *FILE, use direct write() instead *)
  let print_string = write_string Unix.stdout ;;
  let print_int = string_of_int >> print_string ;;
  let print_char = string_of_char >> print_string ;;

  (* TODO turn these into proper variant and put inside module *)
  (* TODO variant should look like term_color = Basic of ?? | RGB6 of ?? | Gray of ?? | RGB24b of ?? *)
  let black = 0 ;;
  let red = 1 ;;
  let green = 2 ;;
  let yellow = 3 ;;
  let blue = 4 ;;
  let magenta = 5 ;;
  let cyan = 6 ;;
  let white = 7 ;;


  (* TODO: put in Control Sequences module *)
  let control_sequence_introducer = 27 |> Char.chr |> string_of_char ;;
  let ctrl_start = control_sequence_introducer ^ "[" ;;
  let ctrl_end = control_sequence_introducer ^ "[0m" ;;
  let ctrl_clear = control_sequence_introducer ^ "c" ;;
  let clear () = print_string ctrl_clear ;;
  let newline () = print_string "\r\n" ;;
  let ctrl_cursor_hide = ctrl_start ^ "?25l" ;;
  let ctrl_cursor_show = ctrl_start ^ "?25h" ;;

  let ctrl_switch_offscreen = ctrl_start ^ "?47h" ;;
  let ctrl_switch_mainscreen = ctrl_start ^ "?47l" ;;

  let ctrl_gohome = ctrl_start ^ "H" ;;

  let term_fg c = 30 + c ;;
  let term_bg c = 40 + c ;;
  let term_rgb (r, g, b) = 16 + (36 * r) + (6 * g) + b ;;

  let rec term_print_code_seq = function
  | []      -> ""
  | i :: [] -> (string_of_int i ^ "m" )
  | i :: t  -> (string_of_int i ^ ";" ^ term_print_code_seq t)
  ;;

  let term_make_string codes s =
    ctrl_start ^ (term_print_code_seq codes) ^ s ^ ctrl_end ;;

  let term_print codes s =
    print_string (term_make_string codes s) ;;
  ;;

  let term_with_color fg bg s =
    term_make_string [0 ; term_fg fg ; term_bg bg] s ;;

  let term_print_color fg bg s =
    term_print [0 ; term_fg fg ; term_bg bg] s ;;

  let term_print_color256 fg bg s =
    term_print [38 ; 5 ; fg ; 48 ; 5 ; bg] s ;;

  (* TODO: support hex string like specifications like #ffee44 *)
  let term_print_color24b (fg_r, fg_g, fg_b) (bg_r, bg_g, bg_b) s =
    term_print [38 ; 2 ; fg_r; fg_g; fg_b ; 48 ; 2 ; bg_r; bg_g; bg_b] s ;;

  type terminal = {
    buffer : Bytevector.t ;
  } ;;

  let term_init len = {
    buffer = Bytevector.init len ;
  } ;;

  let term_reset term = {
    buffer = Bytevector.reset term.buffer ;
  }

  let term_append s term = {
    buffer = Bytevector.append s term.buffer;
  } ;;

  let term_clear term =
    term |> term_reset |> term_append ctrl_cursor_hide |> term_append ctrl_gohome ;;

  open Vec2

  let term_set_cursor {x ; y} term =
    (* cursor positions are 1 based in the terminal referential *)
    let (y_pos, x_pos) = (y |> inc |> string_of_int, x |> inc |> string_of_int) in
    let cursor_ctrl_string = ctrl_start ^ y_pos ^ ";" ^ x_pos ^ "H" in
    term_append cursor_ctrl_string term
  ;;

  let term_newline term = term_append "\r\n" term ;;

  let term_flush term =
    term |> term_append ctrl_cursor_show |> (fun bvec -> bvec.buffer) |> Bytevector.write Unix.stdout ;
    term ;;


  let do_with_raw_mode action =
    let open Unix in
    (* because terminal_io is a record of mutable fields, do tcgetattr twice:
       once for restoring later, once for setting the terminal to raw mode *)
    let initial = tcgetattr stdin in
    let want    = tcgetattr stdin in
    (
      want.c_brkint  <- false ;   (* no break *)
      want.c_icrnl   <- false ;   (* no CR to NL *)
      want.c_inpck   <- false ;   (* no parity check *)
      want.c_istrip  <- false ;   (* no strip character *)
      want.c_ixon    <- false ;
      want.c_opost   <- false ;
      want.c_echo    <- false ;
      want.c_icanon  <- false ;
      want.c_isig    <- false ;   (* no INTR, QUIT, SUSP signals *)
      want.c_vmin    <- 0;        (* return each byte one by one, or 0 if timeout *)
      want.c_vtime   <- 100;      (* 100 * 100 ms timeout for reading input *)
                                  (* TODO: how to set a low timeout in order to process async IO results
                                               but not deal with the hassle of End_of_file from input_char ... *)
      want.c_csize   <- 8;        (* 8 bit chars *)

      (* TODO: save cursor position *)
      print_string ctrl_switch_offscreen ;
      tcsetattr stdin TCSAFLUSH want ;
      try_finally action (fun () ->
        tcsetattr stdin TCSAFLUSH initial ;
        print_string ctrl_switch_mainscreen
        (* TODO: restore cursor position *)
      )
    ) ;;

end


(* demo for printing color tables *)
(* TODO: move to separate file *)
module ColorTable = struct

  open Term

  let color_table = [|
    ("black", black, white) ;
    ("red", red, white) ;
    ("green", green, white) ;
    ("yellow", yellow, black) ;
    ("blue", blue, white) ;
    ("magenta", magenta, white) ;
    ("cyan", cyan, white) ;
    ("white", white, black) ;
  |] ;;

  let print_base_color_table () =
    let print_one_color (name, bg, fg) =
      term_print_color bg black name ;
      print_string " -- " ;
      term_print [1 ; term_fg bg ; term_bg black] name ;
      print_string " -- " ;
      term_print_color fg bg name ;
      print_string " -- " ;
      term_print [1 ; term_fg fg ; term_bg bg] name ;
      print_newline ()
    in
    let maxlen = color_table
                  |> Array.map (fun (x, _, _) -> slen x)
                  |> Array.fold_left max 0
    in
    let padded_color_table = Array.map (fun (s, a, b) -> (Utils.postpad maxlen s, a, b)) color_table
    in
    Array.iter print_one_color padded_color_table
  ;;

  let print_256_color_table () =
    (* base colors *)
    for c = 0 to 6 do
      term_print_color256 white c ("  " ^ (string_of_int c) ^ "  ")
    done ;
    term_print_color256 black 7 " 7 " ;
    (* base colors *)
    for c = 8 to 15 do
      term_print_color256 white c (" " ^ (string_of_int c) ^ " ")
    done ;
    print_newline () ;

    for c = 16 to 231 do
      if (c - 16) mod 36 = 0 then print_newline () ;
      term_print_color256 white c (" " ^ (Utils.prepad 3 (string_of_int c)) ^ " ")
    done ;

    (* gray scale *)
    print_newline () ;
    print_newline () ;
    for c = 232 to 255 do
      if c = 244 then print_newline () ;
      term_print_color256 white c (" " ^ (string_of_int c) ^ " ")
    done ;
    print_newline ()
  ;;

  let main () =
    clear () ;
    print_base_color_table () ;
    print_newline () ;
    print_256_color_table () ;
    print_newline () ;
    term_print_color24b (0, 204, 153) (242, 230, 255) " something in 24b colors " ;
    print_newline ()
  ;;

end


(* This represents a file currently edited
 * It contains both file information, and windowing information
 * TODO: to properly support multiple editing views into the same file, I need to split these into two
 * TODO: handle long lines: need to wrap line correctly, but need to detect in advance at creation and track correspondly *)
(* TODO: add line number *)
(* TODO: Some of the first cracks in this reprensentation are already showing up.
         For instance, empty lines are empty strings, but in the file they take characters
         this requires special handling in the editor, because we still need to restore these empty lines at save
         and need to display them.
         The naive version of move_next_word therefore fails on empty lines without special handling.
         Similarly move_next_paragraph requires special cursor advance function, because it must not ignore
         empty line, while move_next_word must absolutely do.
         Furthermore, next word, next line, end-of-line, and so on should be first class concept in this
         representation. *)
module Filebuffer = struct

  open Utils
  open Vec2

  type t = {
      (* TOOD: refactor this into slice *)
      buffer: string array ; (* the file data, line per line *)
      buflen: int ;          (* number of lines in the buffer, maybe less than buffer array length *)

      cursor : Vec2.t ;      (* current position string array: y = index array (rows), x = string array (cols) *)

      view_start : int ;     (* index of first row in view *)
      view_diff  : int;      (* additional rows in the view after the first row = total_rows_in_view - 1 *)
                             (* index of last row in view is view_start + view_diff *)
  }

  let init lines view_h =
    let buffer = Array.of_list lines in {
      buffer        = buffer ;
      buflen        = alen buffer ;
      cursor        = Vec2.zero ;
      view_start    = 0 ;
      view_diff     = view_h - 1;
    }

  let is_current_char_valid t = t.cursor.x < (slen t.buffer.(t.cursor.y)) ;;
  let current_line t = t.buffer.(t.cursor.y)
  let current_char t = String.get (current_line t) t.cursor.x ;;
  let current_cursor t = t.cursor ;;

  let saturate_up length x = min (max (length - 1) 0) x

  let adjust_view t =
    if t.cursor.y < t.view_start then
      { t with
        view_start  = t.cursor.y ;
      }
    else if t.cursor.y > t.view_start + t.view_diff then
      { t with
        view_start  = t.cursor.y - t.view_diff ;
      }
    else t

  let adjust_cursor vec2 t = { t with cursor = vec2 }

  let apply_movement fn t =
    t |> adjust_cursor (fn t) |> adjust_view

  let recenter_view t =
    let new_start = t.cursor.y - t.view_diff / 2 in
    let adjusted_bottom = max new_start 0 in
    let adjusted_top = (saturate_up t.buflen (adjusted_bottom + t.view_diff) - t.view_diff) in
    { t with view_start = adjusted_top }

  (* TODO: regroup movement functions that fit the Ciseau.command type into a specific submodule. *)
  (* move_* commands saturates at 0 and end of line *)
  let move_cursor_left t = {
    x = t.cursor.x |> dec |> max 0 ;
    y = t.cursor.y ;
  }

  let move_cursor_left2 t = {
    x = t.cursor.x |> dec |> max 0 ;
    y = t.cursor.y ;
  }

  let move_cursor_right t = {
    x = t.cursor.x |> inc |> saturate_up (slen t.buffer.(t.cursor.y)) ;
    y = t.cursor.y ;
  }

  let move_n_up n t = {
    x = t.cursor.x ;
    y = t.cursor.y |> fun x -> x - n |> max 0 ;
  }

  let move_n_down n t = {
    x = t.cursor.x ;
    y = t.cursor.y |> (+) n |> saturate_up t.buflen ;
  }

  let move_cursor_up   = move_n_up 1 ;;
  let move_cursor_down = move_n_down 1 ;;
  let move_page_up   t = move_n_up (t.view_diff + 1) t ;;
  let move_page_down t = move_n_down (t.view_diff + 1) t ;;

  let cursor_next_char t =
    (* BUG: infinite loop on file where the matcher never return true *)
    let rec first_non_empty y =
      match y with
      | _ when y = t.buflen           -> first_non_empty 0
      | _ when 0 = slen t.buffer.(y)  -> first_non_empty (y + 1)
      | _                             -> y
    in
      if t.cursor.x + 1 < slen (current_line t)
      then { x = t.cursor.x + 1; y = t.cursor.y}
      else { x = 0; y = first_non_empty (t.cursor.y + 1) } (* skip empty lines *)

  let cursor_prev_char t =
    (* BUG: infinite loop on file where the matcher never return true *)
    let rec last_non_empty y =
      match y with
      | _ when y = -1                 -> last_non_empty (t.buflen - 1)
      | _ when 0 = slen t.buffer.(y)  -> last_non_empty (y - 1)
      | _                             -> y
    in
      if t.cursor.x - 1 > 0
      then { x = t.cursor.x - 1 ; y = t.cursor.y }
      else
        let y' = last_non_empty (t.cursor.y - 1) in
        { x = (slen t.buffer.(y')) - 1 ; y = y'}

  let cursor_next_line t = {
      x = t.cursor.x ;
      y = (t.cursor.y + 1) mod t.buflen ;
    }

  let cursor_prev_line t =
    let y' = if t.cursor.y - 1 >= 0 then t.cursor.y - 1 else t.buflen - 1 in {
      x = t.cursor.x ;
      y = y' ;
    }

  (* TODO: refactor to remove the use of adjust cursor *)
  let rec cursor_move_while u f t =
    if f t
    then t |> adjust_cursor (u t) |> cursor_move_while u f
    else t

  let move_next_word t =
    t |> cursor_move_while cursor_next_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_next_char (current_char >> is_alphanum)
      |> cursor_move_while cursor_next_char (current_char >> is_alphanum >> not)
      |> current_cursor

  (* BUG: when starting from an empty line, the first previous word is skipped and the cursor goes to the second previous word *)
  let move_prev_word t =
    t |> cursor_move_while cursor_prev_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_prev_char (current_char >> is_alphanum)
      |> cursor_move_while cursor_prev_char (current_char >> is_alphanum >> not)
      |> cursor_move_while cursor_prev_char (current_char >> is_alphanum)
      |> cursor_next_char

  (* BUG when wrapping over the end of a file, last paragraph and first paragraph are see as one paragraph only *)
  let move_next_paragraph t =
    t |> cursor_move_while cursor_next_line (current_line >> is_empty)
      |> cursor_move_while cursor_next_line (current_line >> is_empty >> not)
      |> cursor_move_while cursor_next_line (current_line >> is_empty)
      |> current_cursor

  let move_prev_paragraph t =
    t |> cursor_move_while cursor_prev_line (current_line >> is_empty)
      |> cursor_move_while cursor_prev_line (current_line >> is_empty >> not)
      |> cursor_move_while cursor_prev_line (current_line >> is_empty)
      |> current_cursor

  let move_line_start t = { x = 0 ; y = t.cursor.y } ;;
  let move_line_end t   = { x = max 0 ((slen t.buffer.(t.cursor.y)) - 1) ; y = t.cursor.y } ;;
  let move_file_start t = { x = t.cursor.x ; y = 0 } ;;
  let move_file_end t   = { x = t.cursor.x ; y = t.buflen - 1 } ;;

  let cursor_position t = Vec2.make (t.cursor.x, t.cursor.y) ;;
  let cursor_position_relative_to_view t = Vec2.make (t.cursor.x, t.cursor.y - t.view_start) ;;
  let file_length_string t = (string_of_int t.buflen) ^ "L" ;;

  let apply_view_frustrum t =
    let rec loop i accum =
      if i < t.view_start
      then accum
      else loop (i - 1) (t.buffer.(i) :: accum)
    in
      loop (t.view_start + t.view_diff) []

end


module Ciseau = struct

  open Utils

  type editor = {
    term : Term.terminal ;
    width : int;
    height : int;
    running : bool ;

    file : string ;
    filebuffer : Filebuffer.t ;
    view_offset : Vec2.t ;

    header : string ;
    user_input : string ;

    (* TODO: add every X a full stats collection for printing total footprint *)
    (* TODO: keep a rolling buffer of alloc diff per frame and show quantiles *)
    (* TODO: export stats to some log files for doing more offline statistics *)
    gc_stats : Gc.stat ;
    gc_stats_diff : float * float ;

    timestamp           : float ;
    last_input_duration : float ;
    last_cycle_duration : float ;
  } ;;


  let init file : editor =
    let (term_rows, term_cols) = Term.get_terminal_size () in
    let lines = slurp file in
    {
      term            = Term.term_init 0x1000 ;
      width           = term_cols ;
      height          = term_rows ;
      running         = true ;

      file            = file ;
      filebuffer      = Filebuffer.init lines (term_rows - 3) ;
      view_offset     = Vec2.make (0, 1);

      header          = (Sys.getcwd ()) ^ "/" ^ file ;
      user_input      = "" ;

      gc_stats = Gc.quick_stat () ;
      gc_stats_diff = (0., 0.) ;

      timestamp           = Sys.time() ;
      last_input_duration = 0. ;
      last_cycle_duration = 0. ;
    } ;;


  (* Represents an editor command *)
  type command = Noop
               | Stop
               | Move of (Filebuffer.t -> Vec2.t)
               | View of (Filebuffer.t -> Filebuffer.t)


  let apply_command command editor =
    match command with
    | Noop    -> editor
    | Stop    -> { editor with running = false }
    | Move fn -> { editor with filebuffer = Filebuffer.apply_movement fn editor.filebuffer }
    | View fn -> { editor with filebuffer = fn editor.filebuffer }


  let update_stats now input_duration editor =
    let open Gc in
    (* let _ = Bytes.make (1024 * 1024) '0' in (* DEBUG uncomment me to pressure the gc *) *)
    let new_gc_stats = quick_stat () in
    let minor_diff = new_gc_stats.minor_words -. editor.gc_stats.minor_words in
    let major_diff = new_gc_stats.major_words -. editor.gc_stats.major_words in
    { editor with
      gc_stats            = new_gc_stats ;
      gc_stats_diff       = (minor_diff, major_diff) ;
      timestamp           = now ;
      last_input_duration = input_duration ;
      last_cycle_duration = now -. editor.timestamp ;
    } ;;

  let word_byte_size = float (Sys.word_size / 8)

  let format_memory_counter word_count = match word_count /. word_byte_size with
    | x when x < 1024.          -> Printf.sprintf "%.2fB" x
    | x when x < 1024. *. 1024. -> Printf.sprintf "%.2fkB" (x /. 1024.)
    | x                         -> Printf.sprintf "%.2fMB" (x /. 1024. /. 1024.)

  let format_memory_counters (minor, major) =
    Printf.sprintf "%s/%s" (format_memory_counter major)
                           (format_memory_counter minor)

  (* TODO: add number of gc collections *)
  let format_memory_stats editor =
    let open Gc in
    Printf.sprintf "  mem total = (%s)  mem delta = (%s)  gc = (%d,%d)"
      (format_memory_counters (editor.gc_stats.minor_words, editor.gc_stats.major_words))
      (format_memory_counters editor.gc_stats_diff)
      editor.gc_stats.major_collections
      editor.gc_stats.minor_collections

  let format_time_stats editor =
    Printf.sprintf "  time = %.3f ms" (1000. *. (editor.last_cycle_duration -. editor.last_input_duration))

  (* one line for header, one line for status, one line for user input *)
  let usage_screen_height editor = editor.height - 3 ;;

  (* TODO: this function should fill remaining vertical spaces with newlines *)
  let rec print_file_buffer padder max_len lines term = match (max_len, lines) with
  | (0, _)      ->  Term.term_newline term
  | (_, [])     ->  Term.term_newline term
  | (n, h :: t) ->  term |> Term.term_newline
                         (* PERF: instead of string concat, make Term auto add the necessary number of spaces *)
                         |> Term.term_append (padder h)
                         |> print_file_buffer padder (n - 1) t
  ;;

  let pad_line editor = postpad editor.width ;;

  let window_size editor =
    "(" ^ (string_of_int editor.width) ^ " x " ^ (string_of_int editor.height) ^ ")" ;;

  (* TODO HUD other metadata: file dirty bit, other tabs *)
  let show_header editor term =
    let s = editor.header
          ^ "  " ^ (Filebuffer.file_length_string editor.filebuffer)
          ^ "  " ^ (editor.filebuffer |> Filebuffer.cursor_position |> Vec2.to_string)
    in let
      prettified_s = s |> pad_line editor |> Term.term_with_color Term.black Term.yellow
    in
      Term.term_append prettified_s term
    ;;

  let show_status editor term =
    let s = "Ciseau stats: win = " ^ (window_size editor) ^ (format_memory_stats editor) ^ (format_time_stats editor)
          |> pad_line editor
          |> Term.term_with_color Term.black Term.white in
    Term.term_append s term ;;

  (* TODO HUD show current mode and pending command *)
  let show_user_input editor term =
    Term.term_append (pad_line editor editor.user_input) term ;;

  let refresh_screen editor =
    let new_term = editor.term |> Term.term_clear
                               |> show_header editor
                               (* TODO: show line numbers *)
                               |> print_file_buffer (pad_line editor) (usage_screen_height editor)
                                                    (Filebuffer.apply_view_frustrum editor.filebuffer)
                               |> show_status editor
                               |> Term.term_newline
                               |> show_user_input editor
                               |> Term.term_set_cursor
                                    (editor.filebuffer |> Filebuffer.cursor_position_relative_to_view |> Vec2.add editor.view_offset)
                               |> Term.term_flush
    in
      { editor with term = new_term }
  ;;

  let key_to_command = function
    | Keys.Ctrl_c       -> Stop
    | Keys.Ctrl_d       -> Move Filebuffer.move_page_down
    | Keys.Ctrl_j       -> Move Filebuffer.move_next_paragraph
    | Keys.Ctrl_k       -> Move Filebuffer.move_prev_paragraph
    | Keys.Ctrl_u       -> Move Filebuffer.move_page_up
    | Keys.Ctrl_z       -> View Filebuffer.recenter_view
    | Keys.Alt_k        -> Move Filebuffer.move_file_start
    | Keys.Alt_j        -> Move Filebuffer.move_file_end
    | Keys.Alt_l        -> Move Filebuffer.move_line_end
    | Keys.Alt_h        -> Move Filebuffer.move_line_start
    | Keys.ArrowUp      -> Move Filebuffer.move_cursor_up
    | Keys.ArrowDown    -> Move Filebuffer.move_cursor_down
    | Keys.ArrowRight   -> Move Filebuffer.move_cursor_right
    | Keys.ArrowLeft    -> Move Filebuffer.move_cursor_left
    | Keys.Lower_k      -> Move Filebuffer.move_cursor_up
    | Keys.Lower_j      -> Move Filebuffer.move_cursor_down
    | Keys.Lower_l      -> Move Filebuffer.move_cursor_right
    | Keys.Lower_h      -> Move Filebuffer.move_cursor_left
    | Keys.Lower_w      -> Move Filebuffer.move_next_word
    | Keys.Lower_b      -> Move Filebuffer.move_prev_word
    | Keys.Unknown      -> Noop (* ignore for now *)

  let process_key = key_to_command >> apply_command

  let make_user_input key editor =
    let new_head = key.Keys.repr ^ "(" ^ (string_of_int key.Keys.code) ^ ")" in
    let new_user_input = new_head ^ " " ^ editor.user_input in
      { editor with
        user_input = truncate editor.width new_user_input ;
      } ;;

  let process_events editor =
    let before = Sys.time () in
    let key = () |> next_char |> Keys.code_to_key in
    let after = Sys.time () in
      editor |> process_key key.Keys.symbol
             |> make_user_input key
             |> update_stats (Sys.time ()) (after -. before)
    ;;

  let rec loop editor =
    if editor.running then
      editor |> refresh_screen |> process_events |> loop
    ;;

  let run_loop editor () = loop editor ;;

  let main () =
    (if alen Sys.argv > 1 then Sys.argv.(1) else __FILE__)
      |> init
      |> run_loop
      |> Term.do_with_raw_mode
  ;;

end

let () =
  Ciseau.main () ;;
