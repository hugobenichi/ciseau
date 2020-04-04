open Util

let kTERM_ESCAPE = true
let kCHANGE_TERM = true
let kDRAW_SCREEN = true
let kFORCE_TERM_DIM = Some (Vec.mk_v2 60 40)
let kFAIL_ON_GEOMETRY_ASSERTS = false
let kDRAW_TERMINAL_CURSOR = false

let opt_wrap_lines              = Config.define_option "wrap_lines"               bool_of_string true
let opt_show_lineno             = Config.define_option "show_lineno"              bool_of_string true
let opt_current_line_highlight  = Config.define_option "highlight_cursor_line"    bool_of_string true
let opt_current_colm_highlight  = Config.define_option "highlight_cursor_column"  bool_of_string true
let opt_relative_lineno         = Config.define_option "show_relative_lineno"     bool_of_string true

external get_terminal_size : unit -> (int * int) = "get_terminal_size"
let terminal_dimensions  =
  match kFORCE_TERM_DIM with
  | None          -> get_terminal_size >> apply_tup2 Vec.mk_v2
  | Some fake_dim -> fun () -> fake_dim

let term_escape = if kTERM_ESCAPE then "\027[" else "ESC["
let term_escape_len = slen term_escape

let with_escape s =
  Unix.write_substring Unix.stdout term_escape 0 term_escape_len |> ignore ;
  Unix.write_substring Unix.stdout s 0 (slen s) |> ignore

(* Used for restoring terminal state at program exit *)
let terminal_initial = Unix.tcgetattr Unix.stdin

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
    with_escape "s" ;           (* cursor save *)
    with_escape "?25l" ;        (* hide cursor *)
    with_escape "?47h" ;        (* switch offscreen *)
    with_escape "?1000h" ;      (* mouse event on *)
    with_escape "?1002h" ;      (* mouse tracking on *)
    (* with_escape "?1004h" ; *) (* switch focus event off *)
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH want
  end

let terminal_restore () =
  if kCHANGE_TERM then begin
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH terminal_initial ;
    with_escape "?1000l" ;      (* mouse event off *)
    with_escape "?1002l" ;      (* mouse tracking off *)
    with_escape "?1004l" ;      (* switch focus event off *)
    with_escape "?47l" ;        (* switch back to main screen *)
    with_escape "?25h" ;        (* show cursor *)
    with_escape "u"             (* cursor restore *)
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

  (* TODO: introduce a Notacolor variant instead of this hack ! *)
  let notacolor = Gray (-1)

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
      | Gray -1         -> -1
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

  let buffer_add_escape buffer =
    Buffer.add_string buffer term_escape

  let buffer = Buffer.create 4096

  (* TODO: this should be platform specific *)
  let newline = "\r\n"

  let default_text            = ' '
  (* TODO: move these to constant/config module somewhere else *)
  let default_fg_color_code   = Color.color_code_fg Color.White
  let default_bg_color_code   = Color.color_code_bg (Color.Gray 2)

  (* Cursor colors, listed by decreasing precedence order:
   *  active:     main cursor of the active source.
   *  primary:    main cursor of any other source.
   *  secondary:  any other cursor.
   *)
  let default_cursor_color_active     = Color.Bold_Red
  let default_cursor_color_primary    = Color.Magenta
  let default_cursor_color_secondary  = Color.Red

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
    if kDRAW_TERMINAL_CURSOR then begin
      (* cursor position. ANSI terminal weirdness: cursor positions start at 1, not 0. *)
      buffer_add_escape buffer ;
      (* PERF: make a add_number function *)
      Buffer.add_string buffer (string_of_int ((Vec.y framebuffer.cursor) + 1)) ;
      Buffer.add_char buffer ';' ;
      Buffer.add_string buffer (string_of_int ((Vec.x framebuffer.cursor) + 1)) ;
      Buffer.add_char buffer 'H' ;
      buffer_add_escape buffer ;
      Buffer.add_string buffer "?25h" (* show cursor *)
    end ;
    (* and finally, push to terminal *)
    if kDRAW_SCREEN then (
      Buffer.output_buffer stdout buffer ;
      flush stdout
    )

  let put_color_proto window color_array color_code origin size =
    if 0 <= color_code then
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

  let put_line t ~x:x_raw ~y:y ?offset:(offset_raw=0) ?len:(len_raw=0-1) s =
    let wx = Vec.x t.window in
    let wy = Vec.y t.window in
    if x_raw < wx && 0 <= y && y < wy then
      let offset = clamp 0 (slen s) offset_raw in
      let len = if len_raw < 0 then slen s else len_raw in
      (*BUG: if x is less than 0, I must adjust len and offset to take into account that *)
      let x = clamp 0 (wx - 1) x_raw in (* x and y must be strictly inside t.window *)
      bytes_blit_string s offset t.text (y * wx + x) (min len (wx - x))

  let put_frame t ?wire:(wire=false) ?fg:(fg=Color.notacolor) ?bg:(bg=Color.notacolor) origin size =
    let wx = Vec.x t.window in
    let wy = Vec.y t.window in
    let x = Vec.x origin in
    let y = Vec.y origin in
    let dx = Vec.x size in
    let dy = Vec.y size in
    if x <= wx && y <= wy then begin
      let offset_topleft      = wx * y + x in
      let offset_bottomleft   = offset_topleft + dy * wx in
      let show_top_edge = 0 <= y in
      let show_left_edge = 0 <= x in
      let show_right_edge = x + dx < wx in
      let show_bottom_edge = y + dy < wy in
      let fgcode = Color.color_code_fg fg in
      let bgcode = Color.color_code_bg bg in
      let max_dx = min dx (wx - x) + if show_right_edge then 1 else 0 in
      if show_top_edge then begin
        let offscreen_offset = (max 0 x) - x in
        let offset_topleft = offset_topleft + offscreen_offset in
        let max_dx = max_dx - offscreen_offset in
        if wire then bytes_fill t.text offset_topleft max_dx '-' ;
        if 0 <= fgcode then array_fill t.fg_colors offset_topleft max_dx fgcode ;
        if 0 <= bgcode then array_fill t.bg_colors offset_topleft max_dx bgcode
      end ;
      if show_bottom_edge then begin
        let offscreen_offset = (max 0 x) - x in
        let offset_bottomleft = offset_bottomleft + offscreen_offset in
        let max_dx = max_dx - offscreen_offset in
        if wire then bytes_fill t.text offset_bottomleft max_dx '-' ;
        if 0 <= fgcode then array_fill t.fg_colors offset_bottomleft max_dx fgcode ;
        if 0 <= bgcode then array_fill t.bg_colors offset_bottomleft max_dx bgcode ;
      end ;
      let min_y = max 0 y in
      let max_y = min wy (dy + y) in
      if show_left_edge then
        for y' = min_y to max_y - 1 do
          let offset = wx * y' + x in
          if wire then Bytes.set t.text offset '|' ;
          if 0 <= fgcode then array_set t.fg_colors offset fgcode ;
          if 0 <= bgcode then array_set t.bg_colors offset bgcode
        done ;
      if show_right_edge then
        for y' = min_y to max_y - 1 do
          let offset = wx * y' + x + dx in
          if wire then Bytes.set t.text offset '|' ;
          if 0 <= fgcode then array_set t.fg_colors offset fgcode ;
          if 0 <= bgcode then array_set t.bg_colors offset bgcode
        done ;
      if wire then begin
        if show_top_edge && show_left_edge then
          Bytes.set t.text offset_topleft '+' ;
        if show_top_edge && show_right_edge then
          Bytes.set t.text (offset_topleft + dx) '+' ;
        if show_bottom_edge && show_left_edge then
          Bytes.set t.text offset_bottomleft '+' ;
        if show_bottom_edge && show_right_edge then
          Bytes.set t.text (offset_bottomleft + dx) '+'
      end
    end

  (* TODO: stash cursor positions and colors in a separate datastruct and blit them last
   *  caveat: does not work well with overlapping layers. Maybe multiple layers just
   *  requires multiple fbs ? *)
  let put_cursor t ?primary:(primary=false) ?active:(active=false) cursor =
    let cursor' = clampv (Vec.sub t.window v11) cursor in
    if cursor' = cursor then
      let color =
        match (active, primary) with
          | (true, _)       -> default_cursor_color_active
          | (false, true)   -> default_cursor_color_primary
          | (false, false)  -> default_cursor_color_secondary
      in
      put_bg_color t color cursor v11

end

module Source = struct
  open Util
  open Framebuffer

  type fill_line_by_segment_t = lineno:int -> lineoffset:int -> byteoffset:int -> segmentlength:int -> Bytes.t -> unit

  type 's source_tc = {
    lineno_stop               : 's -> int ;
    line_len                  : 's -> int -> int ;
    line_fill_by_segment      : 's -> lineno:int -> lineoffset:int -> byteoffset:int -> segmentlength:int -> Bytes.t -> unit ;
  }

  type options_t = {
    wrap_lines                : bool ;
    show_lineno               : bool ;
    current_line_highlight    : bool ;
    current_colm_highlight    : bool ;
    relative_lineno           : bool ;
  }

  type 's t = {
    origin                : Vec.vec2 ;
    size                  : Vec.vec2 ;
    cursor                : Vec.vec2 ;
    lineno                : int ;
    source                : 's ;
    ops                   : 's source_tc ;
    options               : options_t ;
  }

  (* TODO: move to Constants module somewhere else *)
  let wrapped_line_continuation = " ..."
  let lineno_color = Color.Green
  let cursor_highlight_background = Color.Gray 4
  let cursor_highlight_lineno = Color.Yellow
  let default_options = {
    wrap_lines                = Config.get_option opt_wrap_lines ;
    show_lineno               = Config.get_option opt_show_lineno ;
    current_line_highlight    = Config.get_option opt_current_line_highlight ;
    current_colm_highlight    = Config.get_option opt_current_colm_highlight ;
    relative_lineno           = Config.get_option opt_relative_lineno ;
  }

  let draw_line framebuffer origin size source ops lineno y =
    let bx = Vec.x framebuffer.window in
    let ox = Vec.x origin in
    let oy = Vec.y origin in
    let wx = Vec.x size in
    let wy = Vec.y size in
    let seg = ref 0 in
    let left = ref (ops.line_len source lineno) in
    if !left = 0
      then 1
      else
    let lineoffset = ref 0 in
    let byteoffset = ref ((oy + y) * bx + ox) in
    while 0 < !left && y + !seg < wy do
      if ox < bx then (* origin clamping does not take into account horizal dx when showing lineno *)
        ops.line_fill_by_segment
          source
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

  let draw_source framebuffer { origin ; size ; cursor ; lineno ; source ; ops ; options } =
    let origin = origin |> clampv (Vec.sub framebuffer.window v11) in
    (* TODO: add "cursor anchor mode" *)
    (* compute horizontal offset for showing lineno *)
    let lineno_stop = ops.lineno_stop source in
    let text_dx =
      if options.show_lineno
        then
          (if options.relative_lineno then (Vec.y size) + 1 (* minus sign *) else lineno_stop)
            |> (+) 1 (* lineno display starts at 1 *)
            |> string_of_int
            |> slen
            |> max (slen wrapped_line_continuation)
            |> (+) 1 (* margin *)
        else 0
    in
    let lineno_size = Vec.mk_v2 text_dx 0 in
    let text_origin = Vec.add origin lineno_size in
    let text_size = (Vec.sub size lineno_size) |> clampv (Vec.sub framebuffer.window text_origin) in
    let y = ref 0 in
    let linenor = ref lineno in
    let cursor_y = ref (-1) in
    while !y < (Vec.y text_size) && !linenor < lineno_stop do
      let text_size =
        if options.wrap_lines
          then text_size
          else Vec.mk_v2 (Vec.x text_size) (!y + 1) (* force 1 line max *)
      in
      let dy =
        draw_line framebuffer text_origin text_size source ops !linenor !y
      in
      (* put lineno *)
      if options.show_lineno then begin
        let lineno_offset =
          if options.relative_lineno
            then -(Vec.y cursor)
            else 1  (* absolute lineno starts at 1 *)
        in
        let lineno_string = string_of_int (!linenor + lineno_offset) in
        Framebuffer.put_line
          framebuffer
          ~x:((Vec.x text_origin) - (slen lineno_string) - 1) (* right aligned, with 1 char margin *)
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
      if Vec.y cursor = !linenor then
        cursor_y := !y ;
      y += dy ;
      linenor += 1
    done ;
    if options.show_lineno then begin
      Framebuffer.put_fg_color framebuffer lineno_color origin (Vec.mk_v2 text_dx (Vec.y size)) ;
      Framebuffer.put_bg_color framebuffer Color.Black origin (Vec.mk_v2 text_dx (Vec.y size))
    end ;
    (* Show cursor. Must go after most other coloring *)
    let cursor_y_correction = Vec.mk_v2 0 !cursor_y in
    if 0 <= !cursor_y then begin
      if options.current_line_highlight then
        Framebuffer.put_bg_color
          framebuffer
          cursor_highlight_background
          (Vec.add origin cursor_y_correction)
          (Vec.mk_v2 (Vec.x size) 1) ;
      if options.current_colm_highlight then
        (* TODO: consider skipping wrapped segments of lines *)
        Framebuffer.put_bg_color
          framebuffer
          cursor_highlight_background
          (Vec.add text_origin (Vec.mk_v2 (Vec.x cursor) 0)) (* TODO: add horizontal offset from text_view_origin when text_view_origin is added *)
          (Vec.mk_v2 1 (Vec.y text_size)) ;
      if options.show_lineno then (* must go after normal lineno coloring *)
        Framebuffer.put_fg_color
          framebuffer
          cursor_highlight_lineno
          (Vec.add origin cursor_y_correction)
          (Vec.mk_v2 text_dx 1)
    end ;
    let cursor_p = Vec.add (Vec.add cursor text_origin) cursor_y_correction in
    Framebuffer.put_cursor framebuffer ~active:true cursor_p ;
    Framebuffer.put_cursor framebuffer ~primary:true (Vec.add cursor_p v11) ;
    Framebuffer.put_cursor framebuffer (Vec.add (Vec.add cursor_p v11) v11)

  (* TODO: add named arguments to bytes_blit_string and just uses these here as well *)
  let fill_line_by_segment_from_string_array strings ~lineno:lineno ~lineoffset:lineoffset ~byteoffset:byteoffset ~segmentlength:segmentlength bytes =
    Arrays.bytes_blit_string (Arrays.array_get strings lineno) lineoffset bytes byteoffset segmentlength

  let string_array_typeclass = {
    lineno_stop               = alen ;
    line_len                  = Arrays.array_get >>> slen ;
    line_fill_by_segment      = fill_line_by_segment_from_string_array ;
  }

  let string_array_to_source origin size cursor lineno source = {
    origin  ;
    size    ;
    cursor  ;
    lineno  ;
    source  ;
    ops     = string_array_typeclass ;
    options = default_options ;
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
    let cursor = ref Vec.zero in
    let origin = ref v11 in
    let size = Vec.mk_v2 30 40 in
    let running = ref true in
    let color_square_origin = ref (* Vec.zero *) (Vec.mk_v2 1 0) in
    let color_square_len = 2 in (* TODO: make this resizable *)
    while !running do
      Framebuffer.clear framebuffer ;
      Framebuffer.put_bg_color framebuffer Color.Blue (Vec.mk_v2 5 5) (Vec.mk_v2 10 10) ;
      (*
      Framebuffer.put_fg_color framebuffer Color.Red !color_square_origin (Vec.mk_v2 color_square_len color_square_len) ;
      Framebuffer.put_bg_color framebuffer Color.Red !color_square_origin (Vec.mk_v2 color_square_len color_square_len) ;
      *)
      let source = Source.string_array_to_source !origin size !cursor 0 lorem_ipsum in
      Source.draw_source framebuffer source ;
      Framebuffer.put_frame ~wire:true ~bg:Color.White ~fg:Color.Cyan framebuffer (Vec.sub !origin v11) (Vec.add size v11) ;
      (*
      Framebuffer.put_frame ~wire:true ~bg:Color.White ~fg:Color.Cyan framebuffer (Vec.sub !origin (Vec.mk_v2 5 5)) (Vec.mk_v2 10 10) ;
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
          | Key 'h'     -> cursor := Vec.sub !cursor (Vec.mk_v2 1 0)
          | Key 'j'     -> cursor := Vec.add !cursor (Vec.mk_v2 0 1)
          | Key 'k'     -> cursor := Vec.sub !cursor (Vec.mk_v2 0 1)
          | Key 'l'     -> cursor := Vec.add !cursor (Vec.mk_v2 1 0)
          | Key '\x03'  -> running := false
          | _           -> ()
        end ;
      target := clampv (Vec.sub framebuffer.window v11) !target ;
      cursor := clampv (Vec.mk_v2 20 (Arrays.astop lorem_ipsum)) !cursor
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
 * NEXT: - add text_view_origin
 *       - draw secondary cursors
 *       - draw colors ?
 * BUG: - in line wrapping mode, cursor y computation is incorrect
 *)
