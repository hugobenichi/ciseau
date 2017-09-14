(* TODOs:
 *  - refactor some of the new rendering code, especially the return values
 *  - handle line wrapping
 *
 *  - implement redo command
 *  - add selection of current word (with highlight), go to next selection, search function
 *  - finish implementing terminal save and restore by restoring cursor position
 *)

(* remappings *)

let alen = Array.length ;;
let blen = Bytes.length ;;
let slen = String.length ;;


module Utils = struct

  let inc x = x + 1 ;;
  let dec x = x - 1 ;;

  let try_finally action cleanup =
    let rez =
      try Ok (action ()) with e -> Error e
    in
      cleanup () ;
      match rez with
      | Ok success  -> success
      | Error error -> raise error

  let (>>) f g x = g (f x)

  let string_of_char c = String.make 1 c

  let truncate l s =
    if slen s > l then String.sub s 0 l else s

  let is_empty s = (slen s = 0)

  let is_space      chr = (chr = ' ') || (chr = '\t') || (chr = '\r') || (chr = '\n') ;;
  let is_letter     chr = (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z')) ;;
  let is_digit      chr = ('0' <= chr) && (chr <= '9') ;;
  let is_alphanum   chr = (is_digit chr) || (is_letter chr) ;;
  let is_printable  chr = (' ' <= chr) && (chr <= '~') ;;

  exception IOError

  (* replacement for input_char which considers 0 as Enf_of_file *)
  let next_char =
    (* WARN not thread safe *)
    let buffer = Bytes.make 1 'z' in
    let rec one_byte_reader () =
      match Unix.read Unix.stdin buffer 0 1 with
      | 1   -> Bytes.get buffer 0 |> Char.code
      | 0   -> one_byte_reader ()     (* timeout *)
      | _   -> raise IOError
    in one_byte_reader

  let write fd buffer len =
    let n = Unix.write fd buffer 0 len in
    if n <> len then raise IOError

  let write_string fd s =
    let n = Unix.write_substring fd s 0 (slen s) in
    if n <> slen s then raise IOError

  let slurp f =
    let rec loop lines ch =
      match input_line ch with
      | s                     -> loop (s :: lines) ch
      | exception End_of_file -> List.rev lines
    in
    let ch = open_in f in
    let action () = loop [] ch in
    let cleanup () = close_in ch in
    try_finally action cleanup

end


module Vec2 = struct

  type t = {
    x : int ;
    y : int ;
  }

  let zero = { x = 0; y = 0 }

  let make x y = { x = x ; y = y } ;;
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
                  | Digit_0
                  | Digit_1
                  | Digit_2
                  | Digit_3
                  | Digit_4
                  | Digit_5
                  | Digit_6
                  | Digit_7
                  | Digit_8
                  | Digit_9
                  | Backslash

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
  code_to_key_table.(48)  <- make_key Digit_0     "0"             48 ;;
  code_to_key_table.(49)  <- make_key Digit_1     "1"             49 ;;
  code_to_key_table.(50)  <- make_key Digit_2     "2"             50 ;;
  code_to_key_table.(51)  <- make_key Digit_3     "3"             51 ;;
  code_to_key_table.(52)  <- make_key Digit_4     "4"             52 ;;
  code_to_key_table.(53)  <- make_key Digit_5     "5"             53 ;;
  code_to_key_table.(54)  <- make_key Digit_6     "6"             54 ;;
  code_to_key_table.(55)  <- make_key Digit_7     "7"             55 ;;
  code_to_key_table.(56)  <- make_key Digit_8     "8"             56 ;;
  code_to_key_table.(57)  <- make_key Digit_9     "9"             57 ;;
  code_to_key_table.(92)  <- make_key Backslash   "\\"            92 ;;

  let code_to_key code =
    match code_to_key_table.(code) with
    | unknown when unknown.symbol = Unknown -> { unknown with code = code }
    | found                                 -> found

end


module Bytevector = struct

  type t = {
    bytes : bytes ;
    len : int ;
  }

  let zero = Char.chr 0

  let init len = {
    bytes = Bytes.make len zero ;
    len   = 0 ;
  }

  let reset bytevec = { bytevec with len = 0 }

  let scale size = size |> float |> ( *. ) 1.45 |> ceil |> truncate

  let rec next_size needed_size size =
    if needed_size <= size then size else next_size needed_size (scale size)

  let grow new_size bytes = Bytes.extend bytes 0 (new_size - (blen bytes))

  let ensure_size needed_size bytes =
    let current_size = (blen bytes) in
    if (needed_size <= current_size)
      then bytes
      else grow (next_size needed_size current_size) bytes

  let grow added_length t =
    let new_length = added_length + t.len in
    let new_bytes = ensure_size new_length t.bytes
    in {
      bytes = new_bytes ;
      len   = new_length ;
    }

  let append s t =
    let sl = slen s in
    let t' = grow sl t in
      Bytes.blit_string s 0 t'.bytes t.len sl ;
      t'

  let append_bytes srcbytes srcoffset len t =
    let t' = grow len t in
      Bytes.blit srcbytes srcoffset t'.bytes t.len len ;
      t'

  let cat bvec t =
    let t' = grow bvec.len t in
      Bytes.blit bvec.bytes 0 t'.bytes t.len bvec.len ;
      t'

  let write fd t =
    Utils.write fd t.bytes t.len

  let to_string t =
    Bytes.sub_string t.bytes 0 t.len

end

(* main module for interacting with the terminal *)
module Term = struct

  module Color = struct

    type base = Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenta
              | Cyan
              | White

    let base_code = function
      | Black   -> 0
      | Red     -> 1
      | Green   -> 2
      | Yellow  -> 3
      | Blue    -> 4
      | Magenta -> 5
      | Cyan    -> 6
      | White   -> 7

    let bold_code = function
      | Black   -> 8
      | Red     -> 9
      | Green   -> 10
      | Yellow  -> 11
      | Blue    -> 12
      | Magenta -> 13
      | Cyan    -> 14
      | White   -> 15

    type t = Normal of base
           | Bold of base
           | Gray of int
           | RGB216 of int * int * int
           (* | RGB24b of int * int * int *) (* TODO: support 24b colors, support rgb hex string *)

    let color_control_code = function
      | Normal c        -> base_code c
      | Bold c          -> bold_code c
      | Gray g          -> 232 + g                  (* TODO: clamp to [0,23] *)
      | RGB216 (r,g,b)  -> 16 + 36 * r + 6 * g + b  (* TODO: clamp to [0, 5] ^ 3 *)

    let color_control_string fg bg =
      Printf.sprintf "38;5;%d;48;5;%dm" (color_control_code fg) (color_control_code bg)

    let black   = Normal Black ;;
    let red     = Normal Red ;;
    let green   = Normal Green ;;
    let yellow  = Normal Yellow ;;
    let blue    = Normal Blue ;;
    let magenta = Normal Magenta ;;
    let cyan    = Normal Cyan ;;
    let white   = Normal White ;;

  end

  module Control = struct
    let escape                = 27 |> Char.chr |> Utils.string_of_char ;;
    let start                 = escape ^ "[" ;;
    let finish                = escape ^ "[0m" ;;
    let clear                 = escape ^ "c" ;;
    let newline               = "\r\n"  ;;
    let cursor_hide           = start ^ "?25l" ;;
    let cursor_show           = start ^ "?25h" ;;
    let switch_offscreen      = start ^ "?47h" ;;
    let switch_mainscreen     = start ^ "?47l" ;;
    let gohome                = start ^ "H" ;;

    let cursor_offset = Vec2.make 1 1

    (* ANSI escape codes weirdness: cursor positions are 1 based in the terminal referential *)
    let cursor_set vec2 =
      let open Vec2 in
      let {x = x ; y = y} = Vec2.add cursor_offset vec2 in
      start ^ (Printf.sprintf "%d;%dH" y x)
  end

  external get_terminal_size : unit -> (int * int) = "get_terminal_size"

  open Utils

  (* bypass buffered output to the stdout *FILE, use direct write() instead *)
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
      write_string Unix.stdout Control.switch_offscreen ;
      tcsetattr stdin TCSAFLUSH want ;
      try_finally action (fun () ->
        tcsetattr stdin TCSAFLUSH initial ;
        write_string Unix.stdout Control.switch_mainscreen
        (* TODO: restore cursor position *)
      )
    )

end


module CompositionBuffer = struct

  let default_fg    = Term.Color.white ;;
  let default_bg    = Term.Color.black ;;
  let default_z     = 0 ;;
  let default_text  = ' ' ;;

  let size_for vec2 = vec2.Vec2.x * vec2.Vec2.y

  type t = {
    text        : Bytes.t ;
    fg_colors   : Term.Color.t array ;
    bg_colors   : Term.Color.t array ;
    z_index     : int array ;
    len         : int ;
    window      : Vec2.t ;
  }

  let init vec2 =
    let len = size_for vec2 in {
      text        = Bytes.make len default_text ;
      fg_colors   = Array.make len default_fg ;
      bg_colors   = Array.make len default_bg ;
      z_index     = Array.make len default_z ;
      len         = len ;
      window      = vec2 ;
    }

  let resize vec2 t =
    let new_len = size_for vec2 in
    if new_len <= blen t.text
    then {
      t with len = new_len
    }
    else init vec2

  let clear t =
    Bytes.fill t.text 0 t.len default_text ;
    Array.fill t.fg_colors 0 t.len default_fg ;
    Array.fill t.bg_colors 0 t.len default_bg ;
    Array.fill t.z_index 0 t.len default_z

  let colors_at t offset = (t.fg_colors.(offset), t.bg_colors.(offset))

  let next_contiguous_color_section t start =
    let colors_to_match = colors_at t start in
    let rec loop stop =
      if stop < t.len && (colors_at t stop) = colors_to_match
      then loop (stop + 1)
      else stop
    in loop (start + 1)

  let render_section (fg_color, bg_color) start stop t bvec =
    (* append lines one at a time starting from start offset, ending at stop offset *)
    let open Vec2 in
    let next_line_len t start stop =
      min (t.window.Vec2.x - (start mod t.window.Vec2.x)) (stop - start)
    in
    let append_newline_if_needed t position bvec =
      let is_end_of_line        = (position mod t.window.x) = 0 in
      let is_not_end_of_buffer  = position < t.len in (* Do not append newline at the very end *)
      if is_end_of_line && is_not_end_of_buffer
      then Bytevector.append Term.Control.newline bvec
      else bvec
    in
    let rec loop start stop bvec =
      if start < stop
      then
        let len = next_line_len t start stop in
        bvec
             |> Bytevector.append Term.Control.start
             |> Bytevector.append (Term.Color.color_control_string fg_color bg_color)
             |> Bytevector.append_bytes t.text start len
             |> Bytevector.append Term.Control.finish
             (* Last newline need to be appened *after* the terminating control command for colors *)
             |> append_newline_if_needed t (start + len)
             |> loop (start + len) stop
      else
        bvec
    in
     bvec
          |> loop start stop

  let render t bvec =
    let rec loop start bvec =
      if start < t.len
      then
        let stop = next_contiguous_color_section t start in
        bvec |> render_section (colors_at t start) start stop t
             |> loop stop
      else
        bvec
    in
      loop 0 bvec

  let vec2_to_offset t vec2 =
    vec2.Vec2.y * t.window.Vec2.x + vec2.Vec2.x

  let offset_to_vec2 t offset =
    Vec2.make (offset mod t.window.Vec2.x) (offset / t.window.Vec2.x)

  (* TODO: define an Area type and use Area instead of vec2 *)
  (* TODO: strip the string from \r\n, do tab expansion *)
  let draw vec2 s t =
    let open Vec2 in
    let start = vec2_to_offset t vec2 in
    if start < t.len then
      let len = slen s in
      let maxlen = t.len - start in
      let stoplen = min len maxlen in
      Bytes.blit_string s 0 t.text start stoplen

  (* TODO: define an Area type and use Area instead of vec2 + len *)
  (* TODO: consider changing this api to take start : vec2 + stop : vec2 as a first step towards teh Area type *)
  let set_color vec2 len fg bg t =
    let start = vec2_to_offset t vec2 in
    if start < t.len then
      let maxlen = t.len - start in
      let stoplen = min len maxlen in
      Array.fill t.fg_colors start stoplen fg ;
      Array.fill t.bg_colors start stoplen bg

  (* TODO: define types for bounding box, area and wrapping mode, blending mode for color, ... *)

  (* TODO: delete this test once CompositionBuffer is enhanced with Area type based apis *)
  let test () =
    let cb = init (Vec2.make 20 10) in ignore (
      draw (Vec2.make 0 0) "hello world" cb ;
      draw (Vec2.make 14 1) "foobar" cb ;
      draw (Vec2.make 0 2) "this is 20 char long" cb ;
      draw (Vec2.make 3 3) "hello world blablabalblabalbalbalablalabalbal" cb ;
      draw (Vec2.make 0 9) "left" cb ;
      draw (Vec2.make 15 9) "right" cb ;
      set_color (Vec2.make 4 0) 8 Term.Color.black Term.Color.white cb ;
      set_color (Vec2.make 0 1) 2 Term.Color.white Term.Color.red cb ;
      set_color (Vec2.make 16 2) 3 Term.Color.white Term.Color.red cb ;
      set_color (Vec2.make 0 3) 20 Term.Color.white Term.Color.red cb ;
      set_color (Vec2.make 12 4) 8 Term.Color.white Term.Color.red cb ;
      set_color (Vec2.make 16 5) 8 Term.Color.white Term.Color.red cb ;

      Bytevector.init 1000 |> render cb
                           |> Bytevector.write Unix.stdout
    )
end


module Screen = struct

  open Utils

  type t = {
    size                : Vec2.t ;
    cursor_position     : Vec2.t ;
    render_buffer       : Bytevector.t ;
    composition_buffer  : CompositionBuffer.t ;

    (* In the future, this will have several composition_buffers linked to different file buffer.
     * Composition buffer overlap, tiling and rendering will be managed here *)
  }

  let init vec2 = {
    size                = vec2 ;
    cursor_position     = Vec2.zero ;
    render_buffer       = Bytevector.init 0x1000 ;
    composition_buffer  = CompositionBuffer.init vec2 ;
  }

  let line_size_vec screen =
    Vec2.make screen.size.Vec2.x 0

  let shift_left vec2 =
    Vec2.make 0 vec2.Vec2.y

  let line_offset =
    Vec2.make 0 1

  let line_up vec2 =
    Vec2.sub vec2 line_offset

  let line_down vec2 =
    Vec2.add vec2 line_offset

  let last_line screen =
    screen.size |> line_up

  let last_last_line screen =
    screen.size |> line_up |> line_up

  (* TODO: turn into screen -> () once clear is deleted *)
  let reset screen =
    CompositionBuffer.clear screen.composition_buffer ;
    {
      size                = screen.size ;
      cursor_position     = screen.cursor_position ;
      render_buffer       = Bytevector.reset screen.render_buffer ;
      composition_buffer  = screen.composition_buffer ;
    }

  (* Write a string to the screen starting at the given position.
   * If the string is longer then the remaining space on the line, then wraps the string to the next line.
   * Returns a position at the end of the string written, including wrapping *)
  let write_string screen s vec2 =
    let cb = screen.composition_buffer  in
    CompositionBuffer.draw vec2 s cb ;
    s |> slen |> CompositionBuffer.offset_to_vec2 cb

  (* Set the foreground and background colors of a given segment of the screen delimited by the given start
   * and end positions. *)
  let color_segment fg bg start stop screen =
    let cb = screen.composition_buffer  in
    let start_p = CompositionBuffer.vec2_to_offset cb start in
    let stop_p  = CompositionBuffer.vec2_to_offset cb stop in
    let len = stop_p - start_p in
    if len > 0 then
      CompositionBuffer.set_color start len fg bg cb

  (* Set the foreground and background colors of the line pointed to by the current position. *)
  (* TODO: is this really useful ? I probably might not need it *)
  let color_line fg bg vec2 screen =
    let start = shift_left vec2 in
    let stop = Vec2.add start (line_size_vec screen) in
    color_segment fg bg start stop screen

(* to set the cursor, I need to save the position and then add it after ! *)
  let cursor_set vec2 screen = {
    size                = screen.size ;
    cursor_position     = vec2 ;
    render_buffer       = screen.render_buffer ;
    composition_buffer  = screen.composition_buffer ;
  }

  (* Render the screen to the backing terminal device *)
  let render screen =
    let buffer' =
      screen.render_buffer  |> Bytevector.reset
                            |> Bytevector.append Term.Control.cursor_hide
                            |> Bytevector.append Term.Control.gohome
                            |> CompositionBuffer.render screen.composition_buffer
                            |> Bytevector.append (Term.Control.cursor_set screen.cursor_position)
                            |> Bytevector.append Term.Control.cursor_show
    in
      Bytevector.write Unix.stdout buffer' ;
      { screen with render_buffer = buffer' }

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

  type numbering_mode = Absolute | CursorRelative

  type t = {
      buffer: string array ; (* the file data, line per line *)
      buflen: int ;          (* number of lines in the buffer, maybe less than buffer array length *)

      cursor : Vec2.t ;      (* current position string array: y = index array (rows), x = string array (cols) *)

      view_start : int ;     (* index of first row in view *)
      view_diff  : int ;     (* additional rows in the view after the first row = total_rows_in_view - 1 *)
                             (* index of last row in view is view_start + view_diff *)

      line_number_m : numbering_mode ;
  }

  let init lines view_h =
    let buffer = Array.of_list lines in {
      buffer        = buffer ;
      buflen        = alen buffer ;
      cursor        = Vec2.zero ;
      view_start    = 0 ;
      view_diff     = view_h - 1;
      line_number_m = CursorRelative ;
    }

  let is_current_char_valid t = t.cursor.x < (slen t.buffer.(t.cursor.y)) ;;
  let current_line t = t.buffer.(t.cursor.y)
  let current_char t = String.get (current_line t) t.cursor.x ;;
  let cursor t = t.cursor ;;

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

  let swap_line_number_mode t =
    let new_mode = match t.line_number_m with
    | Absolute        -> CursorRelative
    | CursorRelative  -> Absolute
    in { t with line_number_m = new_mode }

  let recenter_view t =
    let new_start = t.cursor.y - t.view_diff / 2 in
    { t with view_start = max new_start 0 }

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

  (* BUG ? '_' is considered to be a word separation *)
  let move_next_word t =
    t |> cursor_move_while cursor_next_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_next_char (current_char >> is_alphanum)
      |> cursor_move_while cursor_next_char (current_char >> is_alphanum >> not)
      |> cursor

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
      |> cursor

  let move_prev_paragraph t =
    t |> cursor_move_while cursor_prev_line (current_line >> is_empty)
      |> cursor_move_while cursor_prev_line (current_line >> is_empty >> not)
      |> cursor_move_while cursor_prev_line (current_line >> is_empty)
      |> cursor

  let move_line_start t = { x = 0 ; y = t.cursor.y } ;;
  let move_line_end t   = { x = max 0 ((slen t.buffer.(t.cursor.y)) - 1) ; y = t.cursor.y } ;;
  let move_file_start t = { x = t.cursor.x ; y = 0 } ;;
  let move_file_end t   = { x = t.cursor.x ; y = t.buflen - 1 } ;;

  let cursor_relative_to_view t = Vec2.sub t.cursor { x = 0; y = t.view_start } ;;
  let file_length_string t = (string_of_int t.buflen) ^ "L" ;;

  (* Represents the result of projecting a line of text inside a drawing view rectangle *)
  type line_info = End | Line of {
    text        : string ;
    number      : int ;
    fg_color    : Term.Color.t ;
    bg_color    : Term.Color.t ;
  }

  type projected_view = {
    lines         : line_info array ;
  }

  let apply_view_frustrum t =
    let offset = match t.line_number_m with
    | Absolute        -> 1 ;
    | CursorRelative  -> -t.cursor.y
    in let get_line idx =
      let i = idx + t.view_start in
      let bg = if (i = t.cursor.y) then (Term.Color.Gray 4) else Term.Color.black in
      if i < t.buflen
      then Line {
        text        = t.buffer.(i) ;
        number      = i + offset ;
        fg_color    = Term.Color.white ;
        bg_color    = bg ;
      }
      else End
    in {
      lines = Array.init (t.view_diff + 1) get_line ;
    }

end


module Ciseau = struct

  open Utils

  type pending_command_atom = Digit of int

  type pending_command = None
                       | Number of int list

  (* Represents an editor command *)
  type command = Noop
               | Stop
               | Move of (Filebuffer.t -> Vec2.t)
               | View of (Filebuffer.t -> Filebuffer.t)
               | Pending of pending_command_atom

  let max_repetition = 10000

  let enqueue_digit d = function
    | None      -> Number [d]
    | Number ds -> Number (d :: ds)

  let dequeue_digits ds =
    let rec loop acc = function
      | []      -> acc
      | d :: t  -> loop (acc * 10 + d) t
    in min (loop 0 (List.rev ds)) max_repetition

  let pending_command_to_string = function
    | None      -> ""
    | Number ds -> Printf.sprintf "Repetition(%d) " (dequeue_digits ds)

  type editor = {
    screen : Screen.t ;
    (* Fold width and height into Screen.t *)
    width : int;
    height : int;
    running : bool ;

    file : string ;
    filebuffer : Filebuffer.t ;
    view_offset : Vec2.t ;

    header : string ;
    user_input : string ;

    pending_input : pending_command ;

    (* TODO: add every X a full stats collection for printing total footprint *)
    (* TODO: keep a rolling buffer of alloc diff per frame and show quantiles *)
    (* TODO: export stats to some log files for doing more offline statistics *)
    gc_stats : Gc.stat ;
    gc_stats_diff : float * float ;

    timestamp           : float ;
    last_input_duration : float ;
    last_cycle_duration : float ;
  }


  let init file : editor =
    let (term_rows, term_cols) = Term.get_terminal_size () in
    let term_dim = Vec2.make term_cols term_rows in
    let lines = slurp file in
    {
      screen          = Screen.init term_dim ;
      width           = term_cols ;
      height          = term_rows ;
      running         = true ;

      file            = file ;
      filebuffer      = Filebuffer.init lines (term_rows - 3) ;   (* 3 lines for header, status, input *)
      view_offset     = Vec2.make 5 1 ; (* +5 for line numbers, +1 for header *)

      header          = (Sys.getcwd ()) ^ "/" ^ file ;
      user_input      = "" ;

      pending_input   = None;

      gc_stats        = Gc.quick_stat () ;
      gc_stats_diff   = (0., 0.) ;

      timestamp           = Sys.time() ;
      last_input_duration = 0. ;
      last_cycle_duration = 0. ;
    }

  let queue_pending_command editor = function
    | Digit n -> { editor with pending_input = enqueue_digit n editor.pending_input }

  let apply_command command editor =
    match command with
    | Noop    -> editor
    | Stop    -> { editor with running = false }
    | Move fn -> { editor with filebuffer = Filebuffer.apply_movement fn editor.filebuffer }
    | View fn -> { editor with filebuffer = fn editor.filebuffer }
      (* cannot happen ?? *)
    | Pending ((Digit n) as d)  -> queue_pending_command editor d

  let apply_command_with_repetition n command editor =
    match command with
    | Noop    -> editor
    | Stop    -> { editor with running = false }
    | View fn -> { editor with filebuffer = fn editor.filebuffer }
    | Move fn ->
      let rec loop n fb =
        if (n > 0) then loop (n - 1) (Filebuffer.apply_movement fn fb) else fb
      in {
        editor with
        filebuffer    = loop n editor.filebuffer ;
        pending_input = None ;
      }
    | Pending ((Digit n) as d)  -> queue_pending_command editor d

  let update_stats now input_duration editor =
    let open Gc in
    (* let _ = Bytes.make (1024 * 512) '0' in (* DEBUG uncomment me to pressure the gc *) *)
    let new_gc_stats = quick_stat () in
    let minor_diff = new_gc_stats.minor_words -. editor.gc_stats.minor_words in
    let major_diff = new_gc_stats.major_words -. editor.gc_stats.major_words in
    { editor with
      gc_stats            = new_gc_stats ;
      gc_stats_diff       = (minor_diff, major_diff) ;
      timestamp           = now ;
      last_input_duration = input_duration ;
      last_cycle_duration = now -. editor.timestamp ;
    }

  let word_byte_size = float (Sys.word_size / 8)

  let format_memory_counter word_count = match word_count *. word_byte_size with
    | x when x < 1024.          -> Printf.sprintf "%.2fB" x
    | x when x < 1024. *. 1024. -> Printf.sprintf "%.2fkB" (x /. 1024.)
    | x                         -> Printf.sprintf "%.2fMB" (x /. 1024. /. 1024.)

  let format_memory_counters (minor, major) =
    Printf.sprintf "%s/%s" (format_memory_counter major)
                           (format_memory_counter minor)

  let format_memory_stats editor =
    let open Gc in
    Printf.sprintf "  mem total = (%s)  mem delta = (%s)  gc = (%d,%d)"
      (format_memory_counters (editor.gc_stats.minor_words, editor.gc_stats.major_words))
      (format_memory_counters editor.gc_stats_diff)
      editor.gc_stats.major_collections
      editor.gc_stats.minor_collections

  let format_time_stats editor =
    Printf.sprintf "  time = %.3f ms" (1000. *. (editor.last_cycle_duration -. editor.last_input_duration))

  let window_size editor =
    "(" ^ (string_of_int editor.width) ^ " x " ^ (string_of_int editor.height) ^ ")"

  let show_header editor screen vec2 =
    let s = editor.header
          ^ "  " ^ (Filebuffer.file_length_string editor.filebuffer)
          ^ "  " ^ (editor.filebuffer |> Filebuffer.cursor |> Vec2.to_string)
    in
      Screen.write_string screen s vec2 |> ignore ;
      Screen.color_line Term.Color.black Term.Color.yellow vec2 screen

  let show_status editor screen vec2 =
    (* BUG: fix this -1 offset issue *)
    let vec2' = Screen.line_up vec2 in
    let s = "Ciseau stats: win = "
          ^ (window_size editor)
          ^ (format_memory_stats editor)
          ^ (format_time_stats editor)
    in
      Screen.write_string screen s vec2' |> ignore ;
      Screen.color_line Term.Color.black Term.Color.white vec2 screen

  let show_user_input editor screen vec2 =
    (* BUG: fix this -1 offset issue *)
    let vec2' = Screen.line_up vec2 in
    let s =  editor.user_input
    in
      Screen.write_string screen s vec2' |> ignore

  let print_file_buffer line_length filebuffer screen =
    (* TODO: colors *)
    (* TODO: derive padding, x_offset, y_offset better by simply using a nested screen *)
    let open Filebuffer in
    let padding = 4 in
    let x_offset = padding + 1 in
    let y_offset = 1 in
    let print_line y = function
      | Line info ->  let y' = y + y_offset in
                      let line_start = Vec2.make x_offset y' in
                      let number_start = Vec2.make 0 y' in
                      let line = truncate line_length info.text in
                      let number = Printf.sprintf "%4d" info.number in
                      Screen.write_string screen number number_start |> ignore ;
                      Screen.write_string screen line line_start |> ignore ;
                      Screen.color_line info.fg_color info.bg_color line_start screen ;
                      Screen.color_segment Term.Color.green Term.Color.black number_start line_start screen
                      (* TODO: handle line wrapping by passing down the returned
                       *       vec2 end point to the next line *)
      | End       ->  ()
    in
    Array.iteri print_line (apply_view_frustrum filebuffer).lines

  let default_fill_screen y_start y_stop screen =
    for y = y_start to y_stop do
      let start = Vec2.make 0 y in
      let stop  = Vec2.make 1 y in
      Screen.write_string screen "~" start |> ignore ;
      Screen.color_segment Term.Color.blue Term.Color.black start stop screen
    done

  (* BUGS:  - color_line and write_string are offsetted by 1, except on the first line !
   *        - the last line is not visible *)
  let refresh_screen editor =
    let screen' = Screen.reset editor.screen in (
      show_header editor screen' Vec2.zero ;
      show_status editor screen' (Screen.last_last_line screen') ;
      show_user_input editor screen' (Screen.last_line screen') ;
      default_fill_screen 1 (editor.height - 3) screen' ;
      print_file_buffer (editor.width - editor.view_offset.Vec2.x) editor.filebuffer screen' ;
      let screen'' =
        screen' |> Screen.cursor_set (editor.filebuffer |> Filebuffer.cursor_relative_to_view
                                                        |> Vec2.add editor.view_offset)
                |> Screen.render
      in { editor with screen = screen'' }
    )


  let key_to_command = function
    | Keys.Ctrl_c       -> Stop
    | Keys.Backslash    -> View Filebuffer.swap_line_number_mode
    | Keys.Ctrl_z       -> View Filebuffer.recenter_view
    | Keys.Ctrl_d       -> Move Filebuffer.move_page_down
    | Keys.Ctrl_j       -> Move Filebuffer.move_next_paragraph
    | Keys.Ctrl_k       -> Move Filebuffer.move_prev_paragraph
    | Keys.Ctrl_u       -> Move Filebuffer.move_page_up
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
    | Keys.Digit_0      -> Pending (Digit 0)
    | Keys.Digit_1      -> Pending (Digit 1)
    | Keys.Digit_2      -> Pending (Digit 2)
    | Keys.Digit_3      -> Pending (Digit 3)
    | Keys.Digit_4      -> Pending (Digit 4)
    | Keys.Digit_5      -> Pending (Digit 5)
    | Keys.Digit_6      -> Pending (Digit 6)
    | Keys.Digit_7      -> Pending (Digit 7)
    | Keys.Digit_8      -> Pending (Digit 8)
    | Keys.Digit_9      -> Pending (Digit 9)
    | Keys.Unknown      -> Noop (* ignore for now *)

  let process_command editor =
    match editor.pending_input with
    | None          -> apply_command
    | Number digits -> apply_command_with_repetition (max 1 (dequeue_digits digits))

  let process_key editor = key_to_command >> (process_command editor)

  (* TODO: replace by a proper history of previous inputs *)
  let make_user_input key editor =
    let new_head = key.Keys.repr ^ "(" ^ (string_of_int key.Keys.code) ^ ")" in
    let new_user_input = (pending_command_to_string editor.pending_input)
                       ^  new_head
                       ^ " " ^ editor.user_input
    in {
      editor with
      user_input = truncate editor.width new_user_input ;
    }

  let process_events editor =
    let before = Sys.time () in
    let key = () |> next_char |> Keys.code_to_key in
    let after = Sys.time () in
      editor |> process_key editor key.Keys.symbol
             |> make_user_input key
             |> update_stats (Sys.time ()) (after -. before)

  let rec loop editor =
    if editor.running then
      editor |> refresh_screen |> process_events |> loop

  let run_loop editor () = loop editor

  let main () =
    (if alen Sys.argv > 1 then Sys.argv.(1) else __FILE__)
      |> init
      |> run_loop
      |> Term.do_with_raw_mode

end

let () =
  Ciseau.main ()
