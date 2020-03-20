open Util

let kTERM_ESCAPE = true
let kCHANGE_TERM = true
let kDRAW_SCREEN = true
let kFORCE_TERM_DIM = Some (Vec.mk_v2 60 40)
let kFAIL_ON_GEOMETRY_ASSERTS = false

external get_terminal_size : unit -> (int * int) = "get_terminal_size"
let terminal_dimensions  =
  match kFORCE_TERM_DIM with
  | None          -> get_terminal_size >> apply_tup2 Vec.mk_v2
  | Some fake_dim -> fun () -> fake_dim

let term_escape = if kTERM_ESCAPE then "\027[" else "ESC["
let term_escape_len = slen term_escape

let write_escape s =
  Unix.write_substring Unix.stdout term_escape 0 term_escape_len |> ignore ;
  Unix.write_substring Unix.stdout s 0 (slen s) |> ignore

let buffer_add_escape buffer =
  Buffer.add_string buffer term_escape

(* Used for restoring terminal state at program exit *)
let terminal_initial = Unix.tcgetattr Unix.stdin

let terminal_restore () =
  if kCHANGE_TERM then begin
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH terminal_initial ;
  write_escape "?1000l" ; (* mouse event off *)
  write_escape "?1002l" ; (* mouse tracking off *)
  write_escape "?1004l" ; (* switch focus event off *)
  write_escape "?47l" ;   (* switch back to main screen *)
  write_escape "u"        (* cursor restore *)
  end

let terminal_set_raw () =
  if kCHANGE_TERM then begin
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
  write_escape "s" ;          (* cursor save *)
  write_escape "?47h" ;       (* switch offscreen *)
  write_escape "?1000h" ;     (* mouse event on *)
  write_escape "?1002h" ;     (* mouse tracking on *)
  (* write_escape "?1004h" ; *) (* switch focus event off *)
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH want
  end

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

  let color_control_strings = Array.make (2 * 256) ""
  let _ =
    for i = 0 to 255 do
      Arrays.array_set color_control_strings (i)       (Printf.sprintf "38;5;%d" i) ;
      Arrays.array_set color_control_strings (i + 256) (Printf.sprintf ";48;5;%dm" i)
    done

  let color_code_to_string = Arrays.array_get color_control_strings
end


let clamp a b x =
  let x' = x |> max a |> min b in
  if kFAIL_ON_GEOMETRY_ASSERTS && x' <> x then fail (Printf.sprintf "%d was not in [%d, %d]" x a b) ;
  x'

let clampv vlim v =
  let x = clamp 0 (Vec.x vlim) (Vec.x v) in
  let y = clamp 0 (Vec.y vlim) (Vec.y v) in
  Vec.mk_v2 x y

let v11 = Vec.mk_v2 1 1


module Framebuffer = struct
  open Util.Arrays

  let buffer = Buffer.create 4096

  (* TODO: this should be platform specific *)
  let newline = "\r\n"

  let default_text            = ' '
  (* TODO: move these to constant/config module somewhere else *)
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

  let debug_text t =
    let k = ref 0 in
    for y = 0 to (Vec.y t.window) - 1 do
      for x = 0 to (Vec.x t.window) - 1 do
        let xy = (x + y) mod 16 in
        let c =
          if xy < 10
            then (Char.code '0') + xy
            else (Char.code 'a') + xy - 10
        in
        Bytes.set t.text !k (Char.chr c) ;
        k += 1
      done
    done

  let debug_color t =
    let k = ref 0 in
    for y = 0 to (Vec.y t.window) - 1 do
      for x = 0 to (Vec.x t.window) - 1 do
        array_set t.bg_colors !k (!k mod 256) ;
        k += 1
      done
    done

  let clear t =
    Bytes.fill t.text 0 t.len default_text ;
    Array.fill t.fg_colors 0 t.len default_fg_color_code ;
    Array.fill t.bg_colors 0 t.len default_bg_color_code

  let clear_rect t rect =
    let startv = rect |> Rec.rect_offset |> clampv (Vec.sub t.window v11) in (* startv must be strictly inside window *)
    let stopv  = rect |> Rec.rect_end |> clampv t.window in
    for y = (Vec.y startv) to (Vec.y stopv) - 1 do
      let offset = y * (Vec.x t.window) + (Vec.x startv) in
      let len = (Vec.x stopv) - (Vec.x startv) in
      Bytes.fill t.text offset len default_text ;
      Array.fill t.fg_colors offset len default_fg_color_code ;
      Array.fill t.bg_colors offset len default_bg_color_code
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
    Array.fill t.bg_colors offset len default_bg_color_code

  let render framebuffer =
    Buffer.clear buffer ;
    (* Do not clear with \027c to avoid flickering *)
    if kTERM_ESCAPE = false then begin
      Buffer.add_string buffer newline ;
      Buffer.add_string buffer "--- NEW FRAME ---" ;
      Buffer.add_string buffer newline
    end ;
    buffer_add_escape buffer ; Buffer.add_string buffer "?25l" ;    (* hide cursor *)
    buffer_add_escape buffer ; Buffer.add_string buffer "H" ;       (* go home *)
    let xstop = (Vec.x framebuffer.window) - 1 in
    let ystop = (Vec.y framebuffer.window) - 1 in
    let k = ref 0 in
    for y = 0 to ystop do
      let k' = ref !k in
      let x = ref 0 in
      while !x <= xstop do
        let fg = array_get framebuffer.fg_colors !k in
        let bg = array_get framebuffer.bg_colors !k in
        let same_color = ref true in
        buffer_add_escape buffer ;
        Buffer.add_string buffer (Color.color_code_to_string fg) ;
        Buffer.add_string buffer (Color.color_code_to_string bg) ;
        while !x <= xstop && !same_color do
          k' += 1 ;
          x += 1 ;
          same_color := !x <= xstop &&
            (fg = array_get framebuffer.fg_colors !k') &&
            (bg = array_get framebuffer.bg_colors !k') ;
        done ;
        Buffer.add_subbytes buffer framebuffer.text !k (!k' - !k) ;
        buffer_add_escape buffer ;
        Buffer.add_string buffer "0m" ;
        k := !k' ;
      done ;
      if y < ystop then
        Buffer.add_string buffer newline
    done ;
    (* cursor position. ANSI terminal weirdness: cursor positions start at 1, not 0. *)
    buffer_add_escape buffer ;
    (* PERF: make a add_number function *)
    Buffer.add_string buffer (string_of_int ((Vec.y framebuffer.cursor) + 1)) ;
    Buffer.add_char buffer ';' ;
    Buffer.add_string buffer (string_of_int ((Vec.x framebuffer.cursor) + 1)) ;
    Buffer.add_char buffer 'H' ;
    buffer_add_escape buffer ;
    Buffer.add_string buffer "?25h" ; (* show cursor *)
    (* and finally, push to terminal *)
    if kDRAW_SCREEN then (
      Buffer.output_buffer stdout buffer ;
      flush stdout
    )

  let put_color_proto window color_array color_code origin size =
    let startv = origin |> clampv (Vec.sub window v11) in (* startv must be strictly inside window *)
    let stopv  = (Vec.add origin size) |> clampv window in
    let segment_len = (Vec.x stopv) - (Vec.x startv) in
    for y = (Vec.y startv) to (Vec.y stopv) - 1 do
      array_fill color_array (y * (Vec.x window) + (Vec.x startv)) segment_len color_code
    done

  let put_fg_color t color = put_color_proto t.window t.fg_colors (Color.color_code_fg color)
  let put_bg_color t color = put_color_proto t.window t.bg_colors (Color.color_code_bg color)

  let put_color_rect t { Color.fg ; Color.bg } rect =
    let origin = Rec.rect_offset rect in
    let size  = Rec.rect_size rect in
    put_fg_color t fg origin size ;
    put_bg_color t bg origin size

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
    bytes_blit_string s offset t.text (y * wx + x) (min len (wx - x))
end

module Source = struct
  open Util
  open Framebuffer

  type fill_line_by_segment_t = lineno:int -> lineoffset:int -> byteoffset:int -> segmentlength:int -> Bytes.t -> unit

  type options_t = {
    wrap_lines                : bool ;
    show_lineno               : bool ;
    current_line_highlight    : bool ;
    current_colm_highlight    : bool ;
    (* TODO:
    relative_lineno           : bool ;
    *)
  }

  type t = {
    origin                : Vec.vec2 ;
    size                  : Vec.vec2 ;
    cursors               : Vec.vec2 list ;
    lineno                : int ;
    lineno_stop           : int ;
    line_len              : int -> int ;
    fill_line             : fill_line_by_segment_t ;
    options               : options_t ;
  }

  (* TODO: move to Constants module somewhere else *)
  let wrapped_line_continuation = " ..."
  let lineno_color = Color.Green
  let default_options = {
    wrap_lines                = true ;
    show_lineno               = true ;
    current_line_highlight    = true ;
    current_colm_highlight    = true ;
  }

  let draw_line framebuffer origin size line_len fill_line lineno y =
    (* TODO: draw lineno and "..." on wrapped lines *)
    let bx = Vec.x framebuffer.window in
    let ox = Vec.x origin in
    let oy = Vec.y origin in
    let wx = Vec.x size in
    let wy = Vec.y size in
    let seg = ref 0 in
    let left = ref (line_len lineno) in
    if !left = 0
      then 1
      else
    let lineoffset = ref 0 in
    let byteoffset = ref ((oy + y) * bx + ox) in
    while 0 < !left && y + !seg < wy do
      fill_line
        ~lineno:lineno
        ~lineoffset:!lineoffset
        ~byteoffset:!byteoffset
        ~segmentlength:(min wx !left)
        framebuffer.text ;
      seg += 1 ;
      left -= wx ;
      lineoffset += wx ;
      byteoffset += bx ;
    done ;
    !seg

  let draw_source framebuffer { origin ; size ; lineno ; lineno_stop ; line_len ; fill_line ; options } =
    (* TODO: draw cursors *)
    (* TODO: put colors *)
    let origin = origin |> clampv (Vec.sub framebuffer.window v11) in
    (* TODO: add "cursor anchor mode" *)
    (* compute horizontal offset for showing lineno *)
    let text_dx =
      if options.show_lineno
        then lineno_stop |> (+) 1 (* lineno display starts at 1 *)
                         |> string_of_int
                         |> slen
                         |> max (slen wrapped_line_continuation)
                         |> (+) 1 (* margin *)
        else 0
    in
    let text_origin = Vec.add origin (Vec.mk_v2 text_dx 0) in
    let text_size = size |> clampv (Vec.sub framebuffer.window text_origin) in
    let y = ref 0 in
    let linenor = ref lineno in
    while !y < (Vec.y text_size) && !linenor < lineno_stop do
      (* BUG: clamping of origin does not take into account the text_origin text_dx, which can cause
       * crashes when drawing sufficiently on the right. draw_line must skip blits in that case *)
      let text_size =
        if options.wrap_lines
          then text_size
          else Vec.mk_v2 (Vec.x text_size) (!y + 1) (* force 1 line max *)
      in
      let dy =
        draw_line framebuffer text_origin text_size line_len fill_line !linenor !y
      in
      (* put lineno *)
      if options.show_lineno then begin
        let lineno_string = string_of_int (!linenor + 1) in (* lineno display starts at 1 *)
        Framebuffer.put_line
          framebuffer
          ~x:((Vec.x text_origin) - (slen lineno_string) - 1) (* right aligned *)
          ~y:((Vec.y origin) + !y)
          lineno_string ;
        if options.wrap_lines then
          for y' = 1 to dy - 1 do
            Framebuffer.put_line
              framebuffer
              ~x:(Vec.x origin)
              ~y:((Vec.y origin) + !y + y')
              wrapped_line_continuation
          done
      end ;
      y += dy ;
      linenor += 1
    done ;
    if options.show_lineno then begin
      Framebuffer.put_fg_color framebuffer lineno_color origin (Vec.mk_v2 text_dx (Vec.y size)) ;
      Framebuffer.put_bg_color framebuffer Color.Black origin (Vec.mk_v2 text_dx (Vec.y size))
    end

  (* TODO: add named arguments to bytes_blit_string and just uses these here as well *)
  let fill_line_by_segment_from_string_array strings ~lineno:lineno ~lineoffset:lineoffset ~byteoffset:byteoffset ~segmentlength:segmentlength bytes =
    Arrays.bytes_blit_string (Arrays.array_get strings lineno) lineoffset bytes byteoffset segmentlength

  let string_array_to_source origin size lineno strings = {
    origin ;
    size ;
    lineno ;
    lineno_stop   = alen strings ;
    cursors       = [] ; (* TODO *)
    line_len      = Arrays.array_get strings >> slen ;
    fill_line     = fill_line_by_segment_from_string_array strings ;
    options       = default_options ;
  }

end

let lorem_ipsum = [|
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor";
  "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation";
  "ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in";
  "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.";
  "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
  "";
  "Curabitur pretium tincidunt lacus. Nulla gravida orci a odio. Nullam varius, turpis";
  "et commodo pharetra, est eros bibendum elit, nec luctus magna felis sollicitudin mauris.";
  "Integer in mauris eu nibh euismod gravida. Duis ac tellus et risus vulputate vehicula.";
  "Donec lobortis risus a elit. Etiam tempor. Ut ullamcorper, ligula eu tempor congue, eros";
  "est euismod turpis, id tincidunt sapien risus a quam. Maecenas fermentum consequat mi.";
  "Donec fermentum. Pellentesque malesuada nulla a mi.";
  "Duis sapien sem, aliquet nec, commodo eget, consequat quis, neque.";
  "Aliquam faucibus, elit ut dictum aliquet, felis nisl adipiscing sapien, sed malesuada diam lacus eget erat.";
  "Cras mollis scelerisque nunc. Nullam arcu. Aliquam consequat.";
  "Curabitur augue lorem, dapibus quis, laoreet et, pretium ac, nisi.";
  "Aenean magna nisl, mollis quis, molestie eu, feugiat in, orci. In hac habitasse platea dictumst.";
|]


let smoke_test () =
  (* Register SIGWINCH handler to react on terminal resize events *)
  let handler sig_n = () in
  let sigwinch_code = 28 in
  Sys.set_signal sigwinch_code (Sys.Signal_handle handler) ;
  (* Prepare terminal *)
  try
    terminal_set_raw () ;
    let term_dim = terminal_dimensions () in
    let framebuffer = Framebuffer.mk_framebuffer term_dim in
    let origin = ref Vec.zero in
    let size = Vec.mk_v2 30 40 in
    let running = ref true in
let color_square_origin = ref (* Vec.zero *) (Vec.mk_v2 1 0) in
let color_square_len = 2 in (* TODO: make this resizable *)
    while !running do
      Framebuffer.clear framebuffer ;
      let source = Source.string_array_to_source !origin size 0 lorem_ipsum in
      Source.draw_source framebuffer source ;
      Framebuffer.put_bg_color framebuffer Color.Blue (Vec.mk_v2 5 5) (Vec.mk_v2 10 10) ;
      (*
      Framebuffer.put_fg_color framebuffer Color.Red !color_square_origin (Vec.mk_v2 color_square_len color_square_len) ;
      Framebuffer.put_bg_color framebuffer Color.Red !color_square_origin (Vec.mk_v2 color_square_len color_square_len) ;
      Framebuffer.debug_color framebuffer ;
      Framebuffer.debug_text framebuffer ;
      *)
      Framebuffer.render framebuffer ;
      let target = origin in
      Keys.get_next_key () |>
        begin function
          | ArrowUp     -> target := Vec.sub !target (Vec.mk_v2 0 1)
          | ArrowDown   -> target := Vec.add !target (Vec.mk_v2 0 1)
          | ArrowRight  -> target := Vec.add !target (Vec.mk_v2 1 0)
          | ArrowLeft   -> target := Vec.sub !target (Vec.mk_v2 1 0)
          | Key '\x03'  -> running := false
          | _           -> ()
        end ;
      target := clampv (Vec.sub framebuffer.window v11) !target
    done ;
    terminal_restore ()
  with
    e ->  terminal_restore () ;
          Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
          Printexc.print_backtrace stdout

let () =
  smoke_test () ;
  exit 0

(*
 * BUGS: - crash when show_lineno = true and drawing text beyond the right limit due to lineno x offset
 * NEXT: - draw cursors
 *       - add frame options
 *       - turn source into a enum type StringArray, Line, Generic, ...,
 *)
