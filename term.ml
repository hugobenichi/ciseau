open Util

let kDRAW_SCREEN = true
let kFAIL_ON_GEOMETRY_ASSERTS = true

external get_terminal_size : unit -> (int * int) = "get_terminal_size"
let terminal_dimensions  =
  get_terminal_size >> apply_tup2 Vec.mk_v2

let stdout_write_string s =
  let l = slen s in
  let n = Unix.write_substring Unix.stdout s 0 l in
  if l <> n then fail ("sdtout write failed for " ^ s)

(* Used for restoring terminal state at program exit *)
let terminal_initial = Unix.tcgetattr Unix.stdin

let terminal_restore () =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH terminal_initial ;
  stdout_write_string "\027[?1000l" ; (* mouse event off *)
  stdout_write_string "\027[?1002l" ; (* mouse tracking off *)
  stdout_write_string "\027[?1004l" ; (* switch focus event off *)
  stdout_write_string "\027[?47l" ;   (* switch back to main screen *)
  stdout_write_string "\027[u"        (* cursor restore *)

let terminal_set_raw () =
  let want = Unix.tcgetattr Unix.stdin in
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
  want.c_vtime   <- 1;        (* 1 * 100 ms timeout for reading input *)
  want.c_csize   <- 8;        (* 8 bit chars *)
  stdout_write_string "\027[s" ;      (* cursor save *)
  stdout_write_string "\027[?47h" ;   (* switch offscreen *)
  stdout_write_string "\027[?1000h" ; (* mouse event on *)
  stdout_write_string "\027[?1002h" ; (* mouse tracking on *)
  (* stdout_write_string "\027[?1004h" ; *) (* switch focus event off *)
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH want

module Keys = struct

  type click =
      Left
    | Right
    | Middle
    | Release

  type key =
      Key of char
    | Click of Vec.vec2 * click         (* esc[M + mod + mouse position *)
    | Escape_Z                          (* esc[Z: shift + tab *)
    | ArrowUp                           (* esc[A *)
    | ArrowDown                         (* esc[B *)
    | ArrowRight                        (* esc[C *)
    | ArrowLeft                         (* esc[D *)
    | EINTR                             (* usually happen when terminal is resized *)

  let key_to_string =
    function
      | Click (v, Left)                 -> "ClickLeft" ^ (Vec.v2_string v)
      | Click (v, Right)                -> "ClickRight" ^ (Vec.v2_string v)
      | Click (v, Middle)               -> "ClickMiddle" ^ (Vec.v2_string v)
      | Click (v, Release)              -> "ClickRelease" ^ (Vec.v2_string v)
      | Escape_Z                        -> "Escape_z"
      | ArrowUp                         -> "ArrowUp"
      | ArrowDown                       -> "ArrowDown"
      | ArrowRight                      -> "ArrowRight"
      | ArrowLeft                       -> "ArrowLeft"
      | EINTR                           -> "Interrupt"
      | Key '\x00'                      -> "^@"
      | Key '\x01'                      -> "^a"
      | Key '\x02'                      -> "^b"
      | Key '\x03'                      -> "^c"
      | Key '\x04'                      -> "^d"
      | Key '\x05'                      -> "^e"
      | Key '\x06'                      -> "^f"
      | Key '\x07'                      -> "^g"
      | Key '\x08'                      -> "^h"
      | Key '\x09'                      -> "^i"
      | Key '\x0a'                      -> "^j"
      | Key '\x0b'                      -> "^k"
      | Key '\x0c'                      -> "^l"
      | Key '\x0d'                      -> "^m"
      | Key '\x0e'                      -> "^n"
      | Key '\x0f'                      -> "^o"
      | Key '\x10'                      -> "^p"
      | Key '\x11'                      -> "^q"
      | Key '\x12'                      -> "^r"
      | Key '\x13'                      -> "^s"
      | Key '\x14'                      -> "^t"
      | Key '\x15'                      -> "^u"
      | Key '\x16'                      -> "^v"
      | Key '\x17'                      -> "^w"
      | Key '\x18'                      -> "^x"
      | Key '\x19'                      -> "^y"
      | Key '\x1a'                      -> "^z"
      | Key '\x1b'                      -> "^["
      | Key '\x1c'                      -> "^\\"
      | Key '\x1d'                      -> "^]"
      | Key '\x1e'                      -> "^^"
      | Key '\x1f'                      -> "^_"
      | Key '\x20'                      -> "space"
      | Key '\x7f'                      -> "del"
      | Key '\''                        -> "'"
      | Key k                           ->  Char.escaped k

  (* x10 mouse click reader *)
  let mouse_x10_reader buffer =
    let convert_click_type c =
      c |> (land) 3 (* Ignore modifier keys and keep the 2 LSBs only *)
        |> (function
          | 0   ->  Left
          | 1   ->  Middle
          | 2   ->  Right
          | 3   ->  Release
          | cb  ->  fail "mouse_x10_reader bug")
    in
    let adjust_coordinate c =
      let c' = c - 33 in if c' < 0 then c' + 255 else c'
    in
    let c = Bytes.get buffer 0 |> Char.code |> convert_click_type in
    let x = Bytes.get buffer 1 |> Char.code |> adjust_coordinate in
    let y = Bytes.get buffer 2 |> Char.code |> adjust_coordinate in
    Click (Vec.mk_v2 x y, c)

  (* Terminal input needs to be read 3 bytes at a time to detect escape sequences *)
  let input_buffer_len = 3

  (* Buffer inputs across next_key calls in order to correcty segment escape key codes *)
  type input_parser = {
    input_fd          : Unix.file_descr ;
    buffer            : Bytes.t ;
    mutable cursor    : int ;    (* indicate if there is some pending input from last read *)
    mutable lastread  : int ;
  }

  let rec read_next_key input_parser () =
    let buffer = input_parser.buffer in
    (* First, consume any pending input that did not match a sequence *)
    if input_parser.cursor > 0 then
      let c = Bytes.get buffer input_parser.cursor in
      input_parser.cursor <- (input_parser.cursor + 1) mod input_parser.lastread ;
      Key c
    else
    (* Else read available data *)
    match
      Unix.read input_parser.input_fd buffer 0 input_buffer_len
    with
      (* interrupt: probably a screen resize event *)
      | exception Unix.Unix_error (Unix.EINTR, _, _)
            -> EINTR
      (* timeout: retry *)
      | 0   ->
              read_next_key input_parser ()
      (* one normal key *)
      | 1   -> Key (Bytes.get buffer 0)
      (* escape sequences *)
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'Z' -> Escape_Z
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'A' -> ArrowUp
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'B' -> ArrowDown
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'C' -> ArrowRight
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'D' -> ArrowLeft
      (* mouse click *)
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'M'
            -> Unix.read input_parser.input_fd buffer 0 input_buffer_len |> ignore ;
               (* TODO: add support for other modes: xterm-262, ... *)
               mouse_x10_reader buffer
      (* This happens when typing CTRL + [ followed by another key: just buffer the input. *)
      | n  ->
          input_parser.cursor <- 1 ;
          input_parser.lastread <- n ;
          Key (Bytes.get buffer 0)

  let make_next_key_fn input_fd =
    {
      input_fd  = input_fd ;
      buffer    = Bytes.make input_buffer_len '\000' ;
      cursor    = 0 ;
      lastread  = 0;
    } |> read_next_key

  let get_next_key = make_next_key_fn Unix.stdin

end

module Color = struct

  type color  = Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenta
              | Cyan
              | White
              | Bold_Black
              | Bold_Red
              | Bold_Green
              | Bold_Yellow
              | Bold_Blue
              | Bold_Magenta
              | Bold_Cyan
              | Bold_White
              | RGB216 of int * int * int
              | Gray of int

  type color_layer = Foreground | Background

  let color_code_raw =
    function
      | Black           -> 0
      | Red             -> 1
      | Green           -> 2
      | Yellow          -> 3
      | Blue            -> 4
      | Magenta         -> 5
      | Cyan            -> 6
      | White           -> 7
      | Bold_Black      -> 8
      | Bold_Red        -> 9
      | Bold_Green      -> 10
      | Bold_Yellow     -> 11
      | Bold_Blue       -> 12
      | Bold_Magenta    -> 13
      | Bold_Cyan       -> 14
      | Bold_White      -> 15
      | Gray g          -> assert_that (0 <= g && g < 24) ;
                           232 + g
      | RGB216 (r,g,b)  -> assert_that (0 <= r && r < 6) ;
                           assert_that (0 <= g && g < 6) ;
                           assert_that (0 <= b && b < 6) ;
                           16 + 36 * r + 6 * g + b

  let color_code_fg =  color_code_raw
  let color_code_bg =  color_code_raw >> ((+) 256)

  type color_cell = {
    fg : color ;
    bg : color ;
  }

  let fg_color_control_strings = Array.init 256 (Printf.sprintf "38;5;%d")
  let bg_color_control_strings = Array.init 256 (Printf.sprintf ";48;5;%dm")

  let color_control_strings = Array.make (2 * 256) ""

  let _ =
    for i = 0 to 255 do
      Arrays.array_set color_control_strings (i)       (Printf.sprintf "38;5;%d" i) ;
      Arrays.array_set color_control_strings (i + 256) (Printf.sprintf ";48;5;%dm" i)
    done

  let color_code_to_string = Arrays.array_get color_control_strings
end

module Framebuffer = struct
  open Util.Arrays

  let clamp a b x =
    let x' = x |> max a |> min b in
    if kFAIL_ON_GEOMETRY_ASSERTS && x' <> x then fail (Printf.sprintf "%d was not in [%d, %d]" x a b) ;
    x'

  let clampv vlim v =
    let x = clamp 0 (Vec.x vlim) (Vec.x v) in
    let y = clamp 0 (Vec.y vlim) (Vec.y v) in
    Vec.mk_v2 x y

  let v11 = Vec.mk_v2 1 1

  let buffer = Buffer.create 4096

  (* TODO: this should be platform specific *)
  let newline = "\r\n"

  let default_text            = ' '
  let default_fg_color_code   = Color.color_code_fg Color.White
  let default_bg_color_code   = Color.color_code_bg (Color.Gray 2)

  type t = {
    text                      : Bytes.t ;
    fg_colors                 : int array ;
    bg_colors                 : int array ;
    len                       : int ;
    window                    : Vec.vec2 ;
    mutable cursor            : Vec.vec2 ;
  }

  let mk_framebuffer v =
    let len = Vec.area v
    in {
      text        = Bytes.make len default_text ;
      fg_colors   = Array.make len default_fg_color_code ;
      bg_colors   = Array.make len default_bg_color_code ;
      len         = len ;
      window      = v ;
      cursor      = Vec.zero ;
    }

  let framebuffer_size { window } = window

  let clear t =
    Bytes.fill t.text 0 t.len default_text ;
    Array.fill t.fg_colors 0 t.len default_fg_color_code ;
    Array.fill t.bg_colors 0 t.len default_fg_color_code

  let clear_rect t rect =
    let startv = rect |> Rec.rect_offset |> clampv (Vec.sub t.window v11) in (* startv must be strictly inside window *)
    let stopv  = rect |> Rec.rect_end |> clampv t.window in
    for y = (Vec.y startv) to (Vec.y stopv) - 1 do
      let offset = y * (Vec.x t.window) + (Vec.x startv) in
      let len = (Vec.x stopv) - (Vec.x startv) in
      Bytes.fill t.text offset len default_text ;
      Array.fill t.fg_colors offset len default_fg_color_code ;
      Array.fill t.bg_colors offset len default_fg_color_code
    done

  let clear_line t ~x:x_raw ~y:y_raw ~len:len_raw =
    let wx = Vec.x t.window in
    let wy = Vec.y t.window in
    let x = clamp 0 (wx - 1) x_raw in (* x and y must be strictly inside t.window *)
    let y = clamp 0 (wy - 1) y_raw in
    let len = clamp 0 (wx - x) len_raw in
    let offset = y * wx + x in
    Bytes.fill t.text offset len default_text ;
    Array.fill t.fg_colors offset len default_fg_color_code ;
    Array.fill t.bg_colors offset len default_fg_color_code

  let render framebuffer =
    Buffer.clear buffer ;
    (* Do not clear the screen with \027c as it causes flickering *)
    Buffer.add_string buffer "\027[?25l" ;    (* hide cursor *)
    Buffer.add_string buffer "\027[H" ;       (* go home *)
    (* Push lines one by one, one color segment at a time *)
    let linestop = ref (Vec.x framebuffer.window) in
    let start = ref 0 in
    let len = ref 0 in
    let fg = ref 0 in
    let bg = ref 0 in
    while !start < framebuffer.len do
      if !len = 0 then (
        fg := array_get framebuffer.fg_colors !start ;
        bg := array_get framebuffer.bg_colors !start
      ) ;
      incr len ;
      let stop = !start + !len in
      (* Push a color segment if: 1) end of line, 2) color switch *)
      let should_draw_line =
        if stop = framebuffer.len then (
          (* End of last line, do no append new line, do not read colors *)
          true
        ) else if stop > !linestop then (
          (* End of line, also put new line control characters for previous line *)
          Buffer.add_string buffer newline ;
          linestop += (Vec.x framebuffer.window) ;
          true
        ) else
          (* Otherwise, just check colors *)
          !fg <> (array_get framebuffer.fg_colors stop) || !bg <> (array_get framebuffer.bg_colors stop)
      in
      if should_draw_line then (
        Buffer.add_string buffer "\027[" ;
        Buffer.add_string buffer (Color.color_code_to_string !fg) ;
        Buffer.add_string buffer (Color.color_code_to_string !bg) ;
        Buffer.add_subbytes buffer framebuffer.text !start !len ;
        Buffer.add_string buffer "\027[0m" ;
        start += !len ;
        len := 0
      )
    done ;
    (* cursor position. ANSI terminal weirdness: cursor positions start at 1, not 0. *)
    Buffer.add_string buffer "\027[" ;
    (* PERF: make a add_number function *)
    Buffer.add_string buffer (string_of_int ((Vec.y framebuffer.cursor) + 1)) ;
    Buffer.add_char buffer ';' ;
    Buffer.add_string buffer (string_of_int ((Vec.x framebuffer.cursor) + 1)) ;
    Buffer.add_char buffer 'H' ;
    Buffer.add_string buffer "\027[?25h" ; (* show cursor *)
    (* and finally, push to terminal *)
    if kDRAW_SCREEN then (
      Buffer.output_buffer stdout buffer ;
      flush stdout
    )

  let put_color_rect t { Color.fg ; Color.bg } rect =
    let startv = rect |> Rec.rect_offset |> clampv (Vec.sub t.window v11) in (* startv must be strictly inside window *)
    let stopv  = rect |> Rec.rect_end |> clampv t.window in
    for y = (Vec.y startv) to (Vec.y stopv) - 1 do
      let offset = y * (Vec.x t.window) + (Vec.x startv) in
      let len = (Vec.x stopv) - (Vec.x startv) in
      fg |> Color.color_code_fg |> array_fill t.fg_colors offset len ;
      bg |> Color.color_code_bg |> array_fill t.bg_colors offset len
    done

  let put_cursor t cursor =
    let cursor' = clampv (Vec.sub t.window v11) cursor in
    if cursor' = cursor
      then t.cursor <- cursor

  let put_line t ~x:x_raw ~y:y_raw ?offset:(offset_raw=0) ?len:(len_raw=0-1) s =
    let offset = clamp 0 (slen s) offset_raw in
    let len = if len_raw < 0 then slen s else len_raw in
    let wx = Vec.x t.window in
    let wy = Vec.y t.window in
    let x = clamp 0 (wx - 1) x_raw in (* x and y must be strictly inside t.window *)
    let y = clamp 0 (wy - 1) y_raw in
    bytes_blit_string s offset t.text (y + wx + x) (min len (wx - x))
end

module Source = struct
  type t = {
    origin                : Util.Vec.vec2 ;
    size                  : Util.Vec.vec2 ;
    cursors               : Util.Vec.vec2 list ;
    lineno                : int ;
    get_line_length       : int -> int ;
    fill_line_by_segment  : lineno:int -> lineoffset:int -> byteoffset:int -> segmentlength:int -> Bytes.t -> unit ;
  }

  let draw_line framebuffer source y lineno =
    (* TODO: draw lineno and "..." on wrapped lines *)
    let open Framebuffer in
    let linelen = source.get_line_length lineno in
    let bx = Vec.x framebuffer.window in
    let wx = Vec.x source.size in
    let nsegments = linelen / wx + (if linelen mod wx = 0 then 0 else 1) in
    for seg = 0 to nsegments - 1 do
      source.fill_line_by_segment
        ~lineno:lineno
        ~lineoffset:(seg * wx)
        ~byteoffset:(((Vec.y source.origin) + y) * bx + (Vec.x source.origin))
        ~segmentlength:wx
        framebuffer.Framebuffer.text
    done ;
    y + nsegments

  let draw_source framebuffer source =
    (* TODO: add "cursor anchor mode" *)
    let rec loop framebuffer source y lineno =
      if y < (Vec.y source.size)
        then
          let y' = draw_line framebuffer source y lineno in
          loop framebuffer source y' (lineno + 1)
    in
    loop framebuffer source 0 source.lineno

  let draw_sources framebuffer =
    List.iter (draw_source framebuffer)
    (* TODO: draw cursors *)
    (* TODO: put colors *)

end
