(* TODOs:
 *  - implement terminal save and restore:
 *      use $ tput smcup before, and $ tput rmcup after
 *      however, this doe snot save cursor position
 *)


(* remappings *)

let length = String.length ;;
let a_map = Array.map ;;
let a_fold = Array.fold_left ;;
let a_iter = Array.iter ;;

let l_iter = List.iter ;;


module IO = struct

  exception Timeout ;;
  exception IOError ;;
  exception ReadTooMuch of int ;;
  exception WroteNotEnough of int ;;

  (* replacement for input_char which considers 0 as Enf_of_file *)
  let next_char =
    (* WARN not thread safe *)
    let buffer = Bytes.make 1 'z' in
    let rec one_byte_reader () =
      match Unix.read Unix.stdin buffer 0 1 with
      | 1   -> Bytes.get buffer 0 |> Char.code
      | 0   -> one_byte_reader ()     (* timeout *)
      | -1  -> raise IOError          (* TODO: errno *)
      | n   -> raise (ReadTooMuch n)  (* cannot happen since we ask for 1 byte only *)
    in one_byte_reader ;;

  let write fd buffer len =
    match Unix.write fd buffer 0 len with
    | n when n = len  -> ()
    | -1              -> raise IOError
    | n               -> raise (WroteNotEnough n)
  ;;

  let write_string fd s =
    let buffer = (Bytes.of_string s) in
    write fd buffer (Bytes.length buffer)
  ;;

  (* TODO: exception handling *)
  let do_with_input_file chan fn =
    let r = fn chan in (
      close_in chan ;
      r
    ) ;;

  let do_with_output_file chan fn =
    let r = fn chan in (
      close_out chan ;
      r
    ) ;;

  let slurp f =
    let rec loop lines ch = match input_line ch with
    | s -> loop (s :: lines) ch
    | exception End_of_file -> List.rev lines
    in
    do_with_input_file (open_in f) (loop []) ;;

  let save f lines = "todo"

end


module Vec2 = struct

  type t = {
    x : int ;
    y : int ;
  } ;;

  let make (x, y) = { x = x ; y = y } ;;
  let add t1 t2 = { x = t1.x + t2.x ; y = t1.y + t2.y } ;;
  let sub t1 t2 = { x = t1.x - t2.x ; y = t1.y - t2.y } ;;
end


module Utils = struct

  type 'a either = Left of 'a | Right of exn ;;

  let try_to action =
    try let x = action () in Left x
    with e -> Right e
  ;;

  let try_finally action cleanup =
    let rez = try_to action in
    cleanup () ;
    match rez with
    | Left success -> success
    | Right error -> raise error
  ;;

  (* fp utils *)

  let (>>) f g x = g (f x) ;;
  let (<<) g f x = g (f x) ;;

  (* string utils *)

  let padding l s = (String.make (l - (length s)) ' ') ;;
  let postpad l s = s ^ (padding l s) ;;
  let prepad l s = (padding l s) ^ s ;;

  let string_of_char c = String.make 1 c ;;

  let truncate l s =
    if String.length s > l then String.sub s 0 l else s ;;

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

  let grow new_size bytes = Bytes.extend bytes 0 (new_size - (Bytes.length bytes)) ;;

  let ensure_size needed_size bytes =
    let current_size = (Bytes.length bytes) in
    if (needed_size <= current_size)
      then bytes
      else grow (next_size needed_size current_size) bytes
  ;;

  let append s t =
    let new_length = (String.length s) + t.len in
    let new_bytes = ensure_size new_length t.bytes in
      Bytes.blit_string s 0 new_bytes t.len (String.length s) ;
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
    IO.write fd bytev.bytes bytev.len ;;

end

(* main module for interacting with the terminal *)
module Term = struct

  external get_terminal_size : unit -> (int * int) = "get_terminal_size" ;;

  open Utils

  (* bypass buffered output to the stdout *FILE, use direct write() instead *)
  let print_string = IO.write_string Unix.stdout ;;
  let print_int = string_of_int >> print_string ;;
  let print_char = string_of_char >> print_string ;;

  (* TODO turn these into proper enum and put inside module *)
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

  let term_fg c = 30 + c ;;
  let term_bg c = 40 + c ;;
  let term_rgb (r, g, b) = 16 + (36 * r) + (6 * g) + b ;;

  let rec term_print_code_seq = function
  | []      -> ()
  | i :: [] -> (print_int i ; print_char 'm' )
  | i :: t  -> (print_int i ; print_char ';' ; term_print_code_seq t)
  ;;

  let term_print codes s =
    print_string ctrl_start ;
    term_print_code_seq codes ;
    print_string s ;
    print_string ctrl_end
  ;;

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
    term |> term_reset |> term_append ctrl_cursor_show |> term_append ctrl_clear ;;

  open Vec2

  let term_set_cursor {x ; y ; _ } term =
    let cursor_ctrl_string = (ctrl_start ^ (string_of_int y) ^ ";" ^ (string_of_int x) ^ "H") in
    term_append cursor_ctrl_string term
  ;;

  let term_newline term = term_append "\r\n" term ;;

  let term_flush term =
    term |> term_append ctrl_cursor_show |> (fun bvec -> bvec.buffer) |> Bytevector.write Unix.stdout ;
    term ;;

(* avoid warning #40 *)
open Unix

  let do_with_raw_mode action =
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
      (* for the time being, we let SIGINT kill the program *)
      (* want.c_isig    <- false ;   (* no INTR, QUIT, SUSP signals *) *)
      want.c_vmin    <- 0;        (* return each byte one by one, or 0 if timeout *)
      want.c_vtime   <- 100;      (* 100 * 100 ms timeout for reading input *)
                                  (* TODO: how to set a low timeout in order to process async IO results
                                               but not deal with the hassle of End_of_file from input_char ... *)
      want.c_csize   <- 8;        (* 8 bit chars *)

      tcsetattr stdin TCSAFLUSH want ;
      Utils.try_finally action (fun () -> tcsetattr stdin TCSAFLUSH initial)
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
                  |> a_map (fun (x, _, _) -> length x)
                  |> a_fold max 0
    in
    let padded_color_table = a_map (fun (s, a, b) -> (Utils.postpad maxlen s, a, b)) color_table
    in
    a_iter print_one_color padded_color_table
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


(* TODO: move to separate file *)
module RawModeExperiment = struct

  let rec loop () =
    let code = IO.next_char () in
    let c = Char.chr code in (
    (* if Char.code c != 27 (* Escape *) *)
    (* then ( *)
      Term.print_char c ;
      Term.print_string " " ;
      Term.print_int (Char.code c) ;
      Term.newline ()
      ; loop ()
    (* ) *)
    ) ;;

  let action () =
    (* clear () ; *)
    Term.print_string "hello raw terminal" ;
    Term.newline () ;
    (* not flushed here *)
    loop ()
  ;;

  let main () =
    Term.do_with_raw_mode action ;;

end


module Files = struct

  let main () =
    let lines = IO.slurp __FILE__ in
    let println s = (print_string s ; print_newline () ) in
    l_iter println lines ;;

end


(* This represents a file currently edited
 * It contains both file information, and windowing information
 * TODO: to properly support multiple editing views into the same file, I need to split these into two
 * TODO: handle long lines: need to wrap line correctly, but need to detect in advance at creation and track correspondly *)
(* TODO: add line number *)
module Filebuffer = struct

  type t = {
      (* TOOD: refactor this into slice *)
      buffer: string array ; (* the file data, line per line *)
      buflen: int ;          (* number of lines in the buffer, maybe less than buffer array length *)

      cursor_y : int ;       (* index in the array *)
      cursor_x : int ;       (* index in the string *)

      view_start : int ;     (* index of first row in view *)
      view_diff  : int;      (* additional rows in the view after the first row = total_rows_in_view - 1 *)
                             (* index of last row in view is view_start + view_diff *)
  } ;;

  let init lines view_h =
    let buffer = Array.of_list lines in {
      buffer        = buffer ;
      buflen        = Array.length buffer ;
      cursor_y      = 0 ;
      cursor_x      = 0 ;
      view_start    = 0 ;
      view_diff     = view_h - 1;
    } ;;

  let inc x = x + 1 ;;
  let dec x = x - 1 ;;

  let adjust_view t =
    if t.cursor_y < t.view_start then
      { t with
        view_start  = t.cursor_y ;
      }
    else if t.cursor_y > t.view_start + t.view_diff then
      { t with
        view_start  = t.cursor_y - t.view_diff ;
      }
    else t

  let move_cursor_left t  = adjust_view { t with cursor_x = t.cursor_x |> dec |> max 0 ; } ;;
  let move_cursor_right t = adjust_view { t with cursor_x = t.cursor_x |> inc |> min ((length t.buffer.(t.cursor_y)) - 1) ; } ;;
  let move_cursor_up t    = adjust_view { t with cursor_y = t.cursor_y |> dec |> max 0 ; } ;;
  let move_cursor_down t  = adjust_view { t with cursor_y = t.cursor_y |> inc |> min (t.buflen - 1) ; } ;;

  (* TODO: introduce a proper {x: y:} record for terminal position instead of keeping the order of fields in the head ... *)
  let cursor_position_relative_to_view t = Vec2.make (t.cursor_x, t.cursor_y - t.view_start) ;;

  let apply_view_frustrum t =
    let rec loop i accum =
      if i < t.view_start
      then accum
      else loop (i - 1) (t.buffer.(i) :: accum)
    in
      loop (t.view_start + t.view_diff) []
    ;;

end


module CiseauPrototype = struct

  (* TODO: cursor position, window size *)
  type editor = {
    term : Term.terminal ;
    width : int;
    height : int;
    running : bool ;

    file : string ;
    filebuffer : Filebuffer.t ;
    view_offset : Vec2.t ;

    header : string ;
    status : string ;
    user_input : string ;
  } ;;

  let default_status = "Ciseau editor v0  " ;;

  let init file : editor =
    let (term_rows, term_cols) = Term.get_terminal_size () in
    let lines = IO.slurp file in
    {
      term            = Term.term_init 0x1000 ;
      width           = term_cols ;
      height          = term_rows ;
      running         = true ;

      file            = file ;
      filebuffer      = Filebuffer.init lines (term_rows - 3) ;
      view_offset     = Vec2.make (0, 2);

      header          = file ^ " @ " ^ (Sys.getcwd ()) ;
      status          = default_status ;
      user_input      = "" ;
    } ;;

  (* one line for header, one line for status, one line for user input *)
  let usage_screen_height editor = editor.height - 3 ;;

  (* TODO: this function should fill remaining vertical spaces with newlines *)
  let rec print_file_buffer max_len lines term = match (max_len, lines) with
  | (0, _)      ->  Term.term_newline term
  | (_, [])     ->  Term.term_newline term
  | (n, h :: t) ->  term |> Term.term_newline |> Term.term_append h |> print_file_buffer (n - 1) t
  ;;

  let window_size editor =
    "(" ^ (string_of_int editor.width) ^ " x " ^ (string_of_int editor.height) ^ ")" ;;

  (* TODO HUD use color for header bar and for status *)
  (* TODO HUD Other header metadata: cursor position in the file, file length, file dirty bit *)
  let refresh_screen editor =
    let new_term = editor.term |> Term.term_clear
                               |> Term.term_append editor.header
                               |> print_file_buffer (usage_screen_height editor)
                                                    (Filebuffer.apply_view_frustrum editor.filebuffer)
                               |> Term.term_append (editor.status ^ (window_size editor))
                               |> Term.term_newline
                               |> Term.term_append editor.user_input
                               (* TODO: there is a off by -1 error in the horizontal position of the cursor ??? *)
                               (* TODO: setting cursor position causes flickering *)
                               |> Term.term_set_cursor
                                    (editor.filebuffer |> Filebuffer.cursor_position_relative_to_view |> Vec2.add editor.view_offset)
                               |> Term.term_flush
    in
      { editor with term = new_term }
  ;;

  let process_key keycode editor =
    (* TODO use table to map keycodes to character enum table *)
    match keycode with
    | 65 (* up arrow *)     -> { editor with filebuffer = Filebuffer.move_cursor_up     editor.filebuffer }
    | 66 (* down arrow *)   -> { editor with filebuffer = Filebuffer.move_cursor_down   editor.filebuffer }
    | 67 (* right arrow *)  -> { editor with filebuffer = Filebuffer.move_cursor_right  editor.filebuffer }
    | 68 (* left arrow *)   -> { editor with filebuffer = Filebuffer.move_cursor_left   editor.filebuffer }
    | _ -> editor (* ignore for now *)
  ;;

  let make_user_input keycode editor =
    let keychar = (keycode |> Char.chr |> Utils.string_of_char) in
    let new_head = keychar ^ "(" ^ (string_of_int keycode) ^ ")" in
    let new_user_input = new_head ^ " " ^ editor.user_input in
      { editor with
        user_input = Utils.truncate editor.width new_user_input ;
      } ;;

  let update_status editor = editor ;;

  let process_events editor =
    let keycode = IO.next_char () in
      editor |> process_key keycode
             |> make_user_input keycode
             |> update_status
    ;;

  let rec loop editor =
    if editor.running then
      editor |> refresh_screen |> process_events |> loop
    ;;

  let run_loop editor () = loop editor ;;

  let main () =
    (* Sys.argv.(1) *)
    __FILE__ |> init |> run_loop |> Term.do_with_raw_mode
  ;;

end

let () =
  (* ColorTable.main () ; *)
  (* RawModeExperiment.main () *)
  (* Files.main () *)
  CiseauPrototype.main ()
  (* () *)
;;
