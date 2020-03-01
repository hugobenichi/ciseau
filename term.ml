open Util

let kDRAW_SCREEN = true

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
  open Util.Vec

  type click =
      Left
    | Right
    | Middle

  type key =
      Key of char
    | Click of vec2 * click (* esc[M + mod + mouse position *)
    | ClickRelease of vec2  (* esc[M + mod + mouse position *)
    | Escape_Z              (* esc[Z: shift + tab *)
    | ArrowUp               (* esc[A *)
    | ArrowDown             (* esc[B *)
    | ArrowRight            (* esc[C *)
    | ArrowLeft             (* esc[D *)
    | EINTR                 (* usually happen when terminal is resized *)

  let descr_of =
    function
      | Click ({x ; y}, Left)           ->  Printf.sprintf "ClickLeft(%d,%d)" x y
      | Click ({x ; y}, Right)          ->  Printf.sprintf "ClickRight(%d,%d)" x y
      | Click ({x ; y}, Middle)         ->  Printf.sprintf "ClickMiddle(%d,%d)" x y
      | ClickRelease {x ; y}            ->  Printf.sprintf "ClickRelease(%d,%d)" x y
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

  (* Terminal input needs to be read 3 bytes at a time to detect escape sequences *)
  let input_buffer_len = 3

  (* Buffer inputs across next_key calls in order to correclty segment escape key codes *)
  type input_buffer = {
    buffer : Bytes.t ;
    mutable cursor : int ;    (* indicate if there is some pending input from last read *)
    mutable lastread : int ;
  }

  let rec next_key input_buffer () =
    let { buffer ; cursor ; lastread } = input_buffer in
    if cursor > 0 then
      let c = Bytes.get buffer cursor in
      input_buffer.cursor <- (cursor + 1) mod lastread ;
      Key c
    else
    match
      Unix.read Unix.stdin buffer 0 input_buffer_len
    with
      (* interrupt: probably a screen resize event *)
      | exception Unix.Unix_error (Unix.EINTR, _, _)
            -> EINTR
      (* timeout: retry *)
      | 0   ->
              next_key input_buffer ()
      (* one normal key *)
      | 1   -> Key (Bytes.get buffer 0)
      (* escape sequences *)
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'Z'
            -> Escape_Z
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'A'
            -> ArrowUp
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'B'
            -> ArrowDown
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'C'
            -> ArrowRight
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'D'
            -> ArrowLeft
      (* mouse click *)
      | 3 when Bytes.get buffer 1 = '[' && Bytes.get buffer 2 = 'M'
            ->
              Unix.read Unix.stdin buffer 0 input_buffer_len |> ignore ;
              (* x10 mouse click mode. *)
              (* TODO: add support for other modes: xterm-262, ... *)
              let x10_position_reader c =
                let c' = (Char.code c) - 33 in
                if c' < 0 then c' + 255 else c'
              in
              let cx = Bytes.get buffer 1 |> x10_position_reader in
              let cy = Bytes.get buffer 2 |> x10_position_reader in
              Bytes.get buffer 0
                |> Char.code
                |> (land) 3 (* Ignore modifier keys *)
                |> (function
                  | 0   ->  Click (mk_v2 cx cy, Left)
                  | 1   ->  Click (mk_v2 cx cy, Middle)
                  | 2   ->  Click (mk_v2 cx cy, Right)
                  | 3   ->  ClickRelease (mk_v2 cx cy)
                  | cb  ->  fail (Printf.sprintf "unexpected mouse event %d,%d,%d" cb cx cy))
      (* This happens when typing CTRL + [ followed by another key *)
      | n  ->
          input_buffer.cursor <- 1 ;
          input_buffer.lastread <- n ;
          Key (Bytes.get buffer 0)

  let make_next_key_fn () =
    {
      buffer    = Bytes.make input_buffer_len '\000' ;
      cursor    = 0 ;
      lastread  = 0;
    } |> next_key

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

  let color_code_bg =  color_code_raw >> ((+) 256)

  type color_cell = {
    fg : color ;
    bg : color ;
  }

  let color_code =
    function
      | Foreground -> color_code_raw
      | Background -> color_code_bg

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
  open Util.Vec
  open Util.Rec

  let buffer = Buffer.create 4096

  (* TODO: this should be platform specific *)
  let newline = "\r\n"

  module Default = struct
    let fg_color_code    = Color.color_code Color.Foreground Color.White
    let bg_color_code    = Color.color_code Color.Background (Color.Gray 2)
    let z     = 0
    let text  = ' '
  end

  type t = {
    text                      : Bytes.t ;
    line_lengths              : int array ;
    fg_colors                 : int array ;
    bg_colors                 : int array ;
    z_index                   : int array ;
    len                       : int ;
    window                    : vec2 ;
    mutable cursor            : vec2 ;
  }

  let mk_framebuffer v2 =
    let len = v2.x * v2.y
    in {
      text        = Bytes.make len Default.text ;
      line_lengths = Array.make v2.x 0 ; (* CLEANUP: rename me *)
      fg_colors   = Array.make len Default.fg_color_code ;
      bg_colors   = Array.make len Default.bg_color_code ;
      z_index     = Array.make len Default.z ;
      len         = len ;
      window      = v2 ;
      cursor      = v2_zero ;
    }

  let framebuffer_size  { window } = window

  let default_fill_len    = 8192
  let default_fg_colors   = Array.make default_fill_len Default.fg_color_code
  let default_bg_colors   = Array.make default_fill_len Default.bg_color_code
  let default_line_length = Array.make 256 0

  let fill_fg_color t offset len color =
    color
      |> Color.color_code Color.Foreground
      |> array_fill t.fg_colors offset len

  let fill_bg_color t offset len color =
    color
      |> Color.color_code Color.Background
      |> array_fill t.bg_colors offset len

  let clear t =
    (*
    let rec loop t offset remaining =
      if remaining > 0 then (
        let len = min remaining default_fill_len in
        array_blit default_fg_colors 0 t.fg_colors offset len ;
        array_blit default_bg_colors 0 t.bg_colors offset len ;
        (* TODO: also clear z_index if I ever start using it *)
        loop t (offset + len) (remaining - len)
      )
    in
      loop t 0 t.len ;
      *)
      Bytes.fill t.text 0 t.len Default.text ;
      Array.fill t.fg_colors 0 t.len Default.fg_color_code ;
      Array.fill t.bg_colors 0 t.len Default.fg_color_code ;
      ()
      (*
      array_blit default_line_length 0 t.line_lengths 0 t.window.y
      *)

  let clear_rect t rect =
    assert_rect_inside t.window rect ;
    let len = rect_w rect in
    for y = (rect_y rect) to (rect_y_end rect) - 1 do
      let offset = y * t.window.x + (rect_x rect) in
      Bytes.fill t.text offset len Default.text ;
      array_blit default_fg_colors 0 t.fg_colors offset len ;
      array_blit default_bg_colors 0 t.bg_colors offset len ;
    done

  let clear_line t ~x:x ~y:y ~len:len =
    assert_that (0 <= y) ;
    assert_that (y < t.window.y) ;
    let offset = y * t.window.x + x in
    Bytes.fill t.text offset len Default.text ;
    array_blit default_fg_colors 0 t.fg_colors offset len ;
    array_blit default_bg_colors 0 t.bg_colors offset len

  let render framebuffer =
    (* Prep buffer *)
    Buffer.clear buffer ;
    (* Do not clear the screen with \027c as it causes flickering *)
    Buffer.add_string buffer "\027[?25l" ;    (* cursor hide *)
    Buffer.add_string buffer "\027[H" ;       (* go home *)

    (* Push lines one by one, one color segment at a time *)
    let linestop = ref framebuffer.window.x in
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
          linestop += framebuffer.window.x ;
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
    Buffer.add_string buffer (string_of_int (framebuffer.cursor.y + 1)) ;
    Buffer.add_string buffer ";" ;
    Buffer.add_string buffer (string_of_int (framebuffer.cursor.x + 1)) ;
    Buffer.add_string buffer "H" ;
    Buffer.add_string buffer "\027[?25h" ; (* show cursor *)

    (* and finally, push to terminal *)
    if kDRAW_SCREEN then (
      Buffer.output_buffer stdout buffer ;
      flush stdout
    )

  let update_line_end t x y =
    ()
    (*
    if (array_get t.line_lengths y) < x then
      array_set t.line_lengths y x
      *)

  let to_offset window x y =
    assert_that (x <= window.x) ;
    assert_that (y <= window.y) ;
    y * window.x + x

  let put_color_rect t { Color.fg ; Color.bg } rect =
    (* Clip rectangle vertically to framebuffer's window *)
    let x_start = max 0 (rect_x rect) in
    let y_start = max 0 (rect_y rect) in
    let x_end   = min t.window.x (rect_x_end rect) in
    let y_end   = min (t.window.y - 1) (rect_y_end rect) in
    let len     = x_end - x_start in
    for y = y_start to y_end do
      let offset = to_offset t.window x_start y in
      fill_fg_color t offset len fg ;
      fill_bg_color t offset len bg ;
      update_line_end t x_end y
    done

  let put_cursor t cursor =
    assert_that (is_v2_inside t.window cursor) ;
    t.cursor <- cursor

  let put_line framebuffer ~x:x ~y:y ?offset:(offset=0) ?len:(len=0-1) s =
    let bytes_offset = to_offset framebuffer.window x y in
    let linelen = if len < 0 then slen s else len in
    let blitlen = min linelen (framebuffer.window.x - x) in
    (*
    if not (x + blitlen <= framebuffer.window.x) then
      fail  (Printf.sprintf "x:%d + blitlen:%d was not leq than framebuffer.window.x:%d" x blitlen framebuffer.window.x);
      *)
    update_line_end framebuffer (x + blitlen) y ;
    bytes_blit_string s offset framebuffer.text bytes_offset blitlen

  (* TODO: eliminate this once remains of Overflow mdoe is gone and Fileview draws directly to a screen
   * Blit content of framebuffer 'src' into a rectangle 'src_rect' of framebuffer 'dst'.
   * Copy cursor in 'dst' if 'copy_cursor' is true. *)
  let put_framebuffer dst dst_rect src =
    assert_rect_inside dst.window dst_rect ;
    assert_that (src.window.y >= rect_h dst_rect) ;

    let w_dst = rect_w dst_rect in
    let x_dst = ref (rect_x dst_rect) in
    let y_dst = ref (rect_y dst_rect) in

    let x_src = ref 0 in
    let y_src = ref 0 in
    let x_src_stop = ref 0 in

    while !y_dst < (rect_y_end dst_rect) do
      let o_dst = to_offset dst.window !x_dst !y_dst in
      let o_src = to_offset src.window !x_src !y_src in

      if 0 = !x_src then
        x_src_stop := array_get src.line_lengths !y_src ;

      let len = w_dst in

      bytes_blit src.text o_src dst.text o_dst len ;
      array_blit src.fg_colors o_src dst.fg_colors o_dst len ;
      array_blit src.bg_colors o_src dst.bg_colors o_dst len ;

      incr y_dst;
      incr y_src
    done

end
