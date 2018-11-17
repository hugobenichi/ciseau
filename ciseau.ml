let starttime = Sys.time ()

let logs = open_out "/tmp/ciseau.log"

(* Debugging flags *)
let kLOG_STATS    = true
let kDRAW_SCREEN  = true
let kDEBUG        = false

module CommonCombinators = struct
  let id x          = x
  let const a b     = a
  let flip f a b    = f b a
  let psi f g a1 a2 = f (g a1) (g a2)
  let (>>) f g x    = g (f x)
end


open CommonCombinators


module OptionCombinators = struct
  let some x = Some x

  let fmap fn =
    function
      | None    -> None
      | Some a  -> fn a

  let map fn =
    fmap (fn >> some)

  let get_or b =
    function
      | None    -> b
      | Some a  -> a
end


module Error = struct

  exception E of string

  let _ =
    Printexc.record_backtrace true ;
    Printexc.register_printer
      (function
        | E msg ->  Some msg
        | _     ->  None)
end

let fail msg = raise (Error.E msg)

let assert_that ?msg:(m="failed assert") condition = if not condition then fail m


let output_int f    = string_of_int >> output_string f
let output_float f  = string_of_float >> output_string f

let (+=) r x = (r := !r + x)

let string_of_char c = String.make 1 c

let is_space      chr = (chr = ' ') || (chr = '\t') || (chr = '\r') || (chr = '\n')
let is_letter     chr = (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z'))
let is_digit      chr = ('0' <= chr) && (chr <= '9')
let is_alphanum   chr = (is_digit chr) || (is_letter chr)
let is_printable  chr = (' ' <= chr) && (chr <= '~')

let write fd buffer len =
  let n = Unix.write fd buffer 0 len in
  if n <> len
    then fail "fd write failed"


(* Wrappers around common Array/String/Bytes operations to get useful backtraces *)
module ArrayOperations = struct

  let alen = Array.length
  let blen = Bytes.length
  let slen = String.length

  let astop ary = (alen ary) - 1

  let check_bounds src_offset src_len =
    if src_offset < 0 || src_len <= src_offset
      then fail (Printf.sprintf "set/get out of bounds: offset %d for range 0..%d" src_offset src_len)

  let check_range range_len src_offset src_len =
    if range_len < 0 || src_offset < 0 || src_len < src_offset + range_len
      then fail (Printf.sprintf "invalid blit/fill for range %d..%d+%d on len=%d item" src_offset src_offset range_len src_len)

  let string_at s i =
    check_bounds i (slen s) ;
    String.get s i

  let array_get a i =
    check_bounds i (alen a) ;
    Array.get a i

  let array_set a i x =
    check_bounds i (alen a) ;
    Array.set a i x

  let array_fill dst dst_o len value =
    check_range len dst_o (alen dst) ;
    Array.fill dst dst_o len value

  let array_blit src src_o dst dst_o len =
    check_range len src_o (alen src) ;
    check_range len dst_o (alen dst) ;
    Array.blit src src_o dst dst_o len

  let bytes_blit src src_o dst dst_o len =
    check_range len src_o (blen src) ;
    check_range len dst_o (blen dst) ;
    Bytes.blit src src_o dst dst_o len

  let bytes_blit_string src src_o dst dst_o len =
    check_range len src_o (slen src) ;
    check_range len dst_o (blen dst) ;
    Bytes.blit_string src src_o dst dst_o len

  let array_rev a =
    let l = alen a in
    for i = 0 to (l - 1) / 2 do
      let t = a.(i) in
      a.(i)         <- a.(l - i - 1) ;
      a.(l - i - 1) <- t
    done

  let array_add a_in e =
    let l = alen a_in in
    let a_out = Array.make (l + 1) e in
    array_blit a_in 0 a_out 0 l ;
    a_out

  let array_swap a i j =
    let t = a.(i) in
    a.(i) <- a.(j) ;
    a.(j) <- t

  let array_find fn a =
    let rec loop fn a i =
      if i = alen a
        then -1
        else (if fn a.(i)
          then i
          else loop fn a (i + 1))
    in
      loop fn a 0
end


open ArrayOperations


module Arraybuffer = struct
  type 'a t = {
    data : 'a array ;
    next : int ;
  }

  let empty () = {
    data = [||] ;
    next = 0 ;
  }

  let reserve n zero_elem = {
    data = Array.make n zero_elem ;
    next = 0 ;
  }

  let to_array { data ; next } =
    Array.sub data 0 next

  let grow e data =
    let len = alen data in
    let len' = max 10 (2 * len) in
    let data' = Array.make len' e in
    array_blit data 0 data' 0 len ;
    data'

  let append { data ; next } elem =
    let r = {
      data  = if next < alen data
                then data
                else grow elem data ;
      next  = next + 1 ;
    }
    in
      array_set r.data next elem ;
      r
end


module Vec2 = struct

  type v2 = {
    x : int ;
    y : int ;
  }

  let mk_v2 x y = {
    x = x ;
    y = y ;
  }

  let v2_zero       = mk_v2 0 0
  let v2_add t1 t2  = mk_v2 (t1.x + t2.x) (t1.y + t2.y)
  let v2_sub t1 t2  = mk_v2 (t1.x - t2.x) (t1.y - t2.y)

  let (<+>) t1 t2 = v2_add t1 t2
  let (<->) t1 t2 = v2_sub t1 t2

  (* Check if second v2 argument is inside the implicit rectangle woth topleft (0,0)
   * and first v2 argument as bottomright corner. *)
  let is_v2_inside { x = xlim ; y = ylim } { x ; y } =
    (0 <= x) && (0 <= y) && (x <= xlim) && (y <= ylim)

  let is_v2_outside { x = xlim ; y = ylim } { x ; y } =
    (x < 0) || (y < 0) || (x > xlim) || (y > ylim)

  let assert_v2_inside box_v2 v2 =
    if is_v2_outside box_v2 v2
      then fail (Printf.sprintf "(%d,%d) out of bound of (%d,%d)" v2.x v2.y box_v2.x box_v2.y)
end


open Vec2


module Rect = struct

  type rect = {
    x0  : int ;
    y0  : int ;
    x1  : int ;
    y1  : int ;
    w   : int ;
    h   : int ;
  }

  let mk_rect tl_x tl_y br_x br_y = {
    x0  = tl_x ;
    y0  = tl_y ;
    x1  = br_x ;
    y1  = br_y ;
    w   = br_x - tl_x ;
    h   = br_y - tl_y ;
  }

  let rect_size   { w ; h}      = mk_v2 w h
  let rect_offset { x0 ; y0 }   = mk_v2 x0 y0
  let rect_end    { x1 ; y1 }   = mk_v2 x1 y1
  let rect_x      { x0 }        = x0
  let rect_y      { y0 }        = y0
  let rect_x_end  { x1 }        = x1
  let rect_y_end  { y1 }        = y1
  let rect_w      { w }         = w
  let rect_h      { h }         = h

  let rect_mv { x ; y } {x0 ; y0 ; x1 ; y1 } =
    mk_rect (x + x0) (y + y0) (x + x1) (y + y1)

  let assert_rect_inside bounds r =
    r |> rect_offset  |> assert_v2_inside bounds ;
    r |> rect_end     |> assert_v2_inside bounds

  let rect_to_string { x0 ; y0 ; w ; h } =
    Printf.sprintf "(%d,%d)x%dx%d" x0 y0 w h
end


open Rect


module Token = struct
  (* TODO: flatten with option ?? *)
  type token = {
    start : int ;
    stop  : int ;
  }

  let mk_tok i j = {
    start = i ;
    stop  = j ;
  }

  let token_contains x { start ; stop } =
    start <= x && x < stop
end


module Color = struct

  type color  = (* First 8 ansi colors *)
                Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenta
              | Cyan
              | White
                (* High contract 8 ansi colors *)
              | Bold_Black
              | Bold_Red
              | Bold_Green
              | Bold_Yellow
              | Bold_Blue
              | Bold_Magenta
              | Bold_Cyan
              | Bold_White
                (* Remaining colors from extended 256 colors mode *)
              | RGB216 of int * int * int
              | Gray of int

  let color_control_code =
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

  type color_cell = {
    fg : color ;
    bg : color ;
  }

  let darkgray  = Gray 2

  let white_code      = color_control_code White
  let darkgray_code   = color_control_code (Gray 2)
  let black_code      = color_control_code Black

  let fg_color_control_strings = Array.init 256 (Printf.sprintf "38;5;%d")
  let bg_color_control_strings = Array.init 256 (Printf.sprintf ";48;5;%dm")

end


(* this is a config module for storing all parameters *)
module Config = struct
  open Color

  type colors = {
    operator      : color_cell ;
    structure     : color_cell ;
    string        : color_cell ;
    spacing       : color_cell ;
    numbers       : color_cell ;
    default       : color_cell ;
    cursor_line   : color_cell ;
    current_token : color_cell ;
    selection     : color_cell ;
    line_numbers  : color_cell ;
    focus_header  : color_cell ;
    header        : color_cell ;
    status_normal : color_cell ;
    status_input  : color_cell ;
    user_input    : color_cell ;
    border        : color_cell ;
    no_text       : color_cell ;

    leftright_neighbor  : color_cell ;
    updown_neighbor     : color_cell ;
  }

  type cfg = {
    colors    : colors ;
    page_size : int;
  }

  let default : cfg = {
    colors = {
      operator = {
        fg    = Green ;
        bg    = darkgray ;
      } ;
      structure = {
        fg    = Red ;
        bg    = darkgray ;
      } ;
      string  = {
        fg    = Yellow ;
        bg    = darkgray ;
      } ;
      spacing = {
        fg    = darkgray ;
        bg    = darkgray ;
      } ;
      numbers = {
        fg    = Magenta ;
        bg    = darkgray ;
      } ;
      default = {
        fg    = White ;
        bg    = darkgray ;
      } ;
      cursor_line = {
        fg    = White ;
        bg    = Black ;
      } ;
      current_token = {
        fg    = White ;
        bg    = Color.Gray 4 ;
      } ;
      selection = {
        fg    = White ;
        bg    = Blue ;
      } ;
      leftright_neighbor = {
        fg    = White ;
        bg    = Red ;
      } ;
      updown_neighbor = {
        fg    = White ;
        bg    = Green ;
      } ;
      line_numbers = {
        fg    = Green ;
        bg    = darkgray ;
      } ;
      focus_header = {
        fg    = darkgray ;
        bg    = Yellow ;
      } ;
      header = {
        fg    = darkgray ;
        bg    = Cyan ;
      } ;
      status_normal = {
        fg    = darkgray ;
        bg    = White ;
      } ;
      status_input = {
        fg    = darkgray ;
        bg    = Red ;
      } ;
      user_input = {
        fg    = White ;
        bg    = darkgray ;
      } ;
      border = {
        fg    = White ;
        bg    = White ;
      } ;
      no_text = {
        fg    = Bold_Magenta ;
        bg    = darkgray ;
      }
    } ;
    page_size = 50;
  }
end


module Keys = struct

  type key  = Unknown of char
            | Tab
            | Ctrl_c
            | Ctrl_d
            | Ctrl_j
            | Ctrl_k
            | Ctrl_u
            | Ctrl_z
            | Space
            | SingleQuote
            | Colon
            | SemiColon
            | Equal
            | ArrowUp
            | ArrowDown
            | ArrowRight
            | ArrowLeft
            | Lower_h
            | Lower_j
            | Lower_k
            | Lower_l
            | Lower_b
            | Lower_c
            | Lower_s
            | Lower_v
            | Lower_d
            | Lower_w
            | Lower_x
            | Lower_z
            | Upper_b
            | Upper_w
            | Upper_g
            | Upper_h
            | Upper_j
            | Upper_k
            | Upper_l
            | Upper_m
            | Upper_z
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
            | ParenLeft
            | ParenRight
            | BracketLeft
            | BracketRight
            | BraceLeft
            | BraceRight
            | Pipe
            | Plus
            | Minus
            | Underscore
            | Backspace
            (* Non-key events *)
            | Click of v2
            | ClickRelease of v2
            | Escape_Z      (* esc[Z: shift + tab *)
            | EINTR         (* usually happen when terminal is resized *)


  let code_to_key_table   = Array.init (256) (fun c -> Unknown (Char.chr c))
  let code_to_descr_table = Array.init (256) (fun c -> Printf.sprintf "unknown:%s(%i)" (c |> Char.chr |> Char.escaped) c)

  let add_key (sym, descr, code) =
    array_set code_to_key_table code sym ;
    array_set code_to_descr_table code descr

  let defined_keys = [
    (Tab,             "Tab",          9) ;
    (Ctrl_c,          "Ctrl_c",       3) ;
    (Ctrl_d,          "Ctrl_d",       4) ;
    (Ctrl_j,          "Ctrl_j",       10) ;
    (Ctrl_k,          "Ctrl_k",       11) ;
    (Ctrl_u,          "Ctrl_u",       21) ;
    (Ctrl_z,          "Ctrl_z",       26) ;
    (Space,           "Space",        32) ;
    (SingleQuote,     "'",            39) ;
    (Colon,           ":",            58) ;
    (SemiColon,       ";",            59) ;
    (Equal,           "Equal",        61) ;
    (* BUG: this collides with 'A', 'B', 'C', 'D', Wat the Moon !!! *)
    (ArrowUp,         "ArrowUp",      65) ;
    (ArrowDown,       "ArrowDown",    66) ;
    (ArrowRight,      "ArrowRight",   67) ;
    (ArrowLeft,       "ArrowLeft",    68) ;
    (Upper_b,         "B",            66) ;
    (Upper_w,         "W",            87) ;
    (Upper_g,         "G",            71) ;
    (Upper_h,         "H",            72) ;
    (Upper_j,         "J",            74) ;
    (Upper_k,         "K",            75) ;
    (Upper_l,         "L",            76) ;
    (Upper_m,         "M",            77) ;
    (Upper_z,         "Z",            90) ;
    (Lower_b,         "w",            98) ;
    (Lower_c,         "c",            99) ;
    (Lower_s,         "s",            115) ;
    (Lower_v,         "w",            118) ;
    (Lower_d,         "d",            100) ;
    (Lower_z,         "z",            122) ;
    (Lower_x,         "x",            120) ;
    (Lower_h,         "h",            104) ;
    (Lower_j,         "j",            106) ;
    (Lower_k,         "k",            107) ;
    (Lower_l,         "l",            108) ;
    (Lower_w,         "w",            119) ;
    (Digit_0,         "0",            48) ;
    (Digit_1,         "1",            49) ;
    (Digit_2,         "2",            50) ;
    (Digit_3,         "3",            51) ;
    (Digit_4,         "4",            52) ;
    (Digit_5,         "5",            53) ;
    (Digit_6,         "6",            54) ;
    (Digit_7,         "7",            55) ;
    (Digit_8,         "8",            56) ;
    (Digit_9,         "9",            57) ;
    (Backslash,       "\\",           92) ;
    (ParenLeft,       "(",            40) ;
    (ParenRight,      ")",            41) ;
    (Plus,            "+",            43) ;
    (Minus,           "-",            45) ;
    (BracketLeft,     "[",            91) ;
    (BracketRight,    "]",            93) ;
    (Underscore,      "_",            95) ;
    (Pipe,            "|",            124) ;
    (BraceLeft,       "{",            123) ;
    (BraceRight,      "}",            125) ;
    (Backspace,       "del",          127) ;
    (Escape_Z,        "esc[Z",        254) ;
    (EINTR,           "EINTR",        255) ;
  ]

  let _ = List.iter add_key defined_keys

  let code_to_descr = array_get code_to_descr_table

  let code_of =
    function
      | Tab               -> 9
      | Ctrl_c            -> 3
      | Ctrl_d            -> 4
      | Ctrl_j            -> 10
      | Ctrl_k            -> 11
      | Ctrl_u            -> 21
      | Ctrl_z            -> 26
      | Space             -> 32
      | SingleQuote       -> 39
      | Colon             -> 58
      | SemiColon         -> 59
      | Equal             -> 61
      | ArrowUp           -> 65
      | ArrowDown         -> 66
      | ArrowRight        -> 67
      | ArrowLeft         -> 68
      | Upper_b           -> 66
      | Upper_w           -> 87
      | Upper_g           -> 71
      | Upper_h           -> 72
      | Upper_j           -> 74
      | Upper_k           -> 75
      | Upper_l           -> 76
      | Upper_m           -> 77
      | Upper_z           -> 90
      | Lower_b           -> 98
      | Lower_c           -> 99
      | Lower_s           -> 115
      | Lower_v           -> 118
      | Lower_d           -> 100
      | Lower_z           -> 122
      | Lower_x           -> 120
      | Lower_h           -> 104
      | Lower_j           -> 106
      | Lower_k           -> 107
      | Lower_l           -> 108
      | Lower_w           -> 119
      | Digit_0           -> 48
      | Digit_1           -> 49
      | Digit_2           -> 50
      | Digit_3           -> 51
      | Digit_4           -> 52
      | Digit_5           -> 53
      | Digit_6           -> 54
      | Digit_7           -> 55
      | Digit_8           -> 56
      | Digit_9           -> 57
      | Backslash         -> 92
      | ParenLeft         -> 40
      | ParenRight        -> 41
      | Plus              -> 43
      | Minus             -> 45
      | BracketLeft       -> 91
      | BracketRight      -> 93
      | Underscore        -> 95
      | Pipe              -> 124
      | BraceLeft         -> 123
      | BraceRight        -> 125
      | Backspace         -> 127
      | Click _           -> 252
      | ClickRelease _    -> 253
      | Escape_Z          -> 254
      | EINTR             -> 255
      | Unknown c         -> Char.code c

  let descr_of =
    function
      | Click {x ; y}         ->  Printf.sprintf "Click(%d,%d)" x y
      | ClickRelease {x ; y}  ->  Printf.sprintf "ClickRelease(%d,%d)" x y
      | k                     ->  k |> code_of |> code_to_descr

  let key_to_escape =
    function
      | Upper_z   -> Escape_Z
      | other     -> Unknown (other |> code_of |> Char.chr)

  let code_timeout    = -1  (* when read() returns 0: timeout*)
  let code_interrupt  = -2  (* when read() is interrupted: assume SIGWINCH *)
  let code_escape     = 27  (* starts an escape sequence *)

  (* input_char equivalent with enhancements for terminal stuff. Not thread safe *)
  let read_char =
    let buffer = Bytes.make 1 'z' in
    let reader () =
      match Unix.read Unix.stdin buffer 0 1 with
        | 0   ->  code_timeout
        | 1   ->  Bytes.get buffer 0 |> Char.code
        | _   -> fail "next_char failed"
        | exception Unix.Unix_error (errcode,  fn_name, fn_param) ->
          (match errcode with
          | Unix.EINTR  -> code_interrupt
          | _           -> raise (Unix.Unix_error (errcode, fn_name, fn_param)))
    in
      reader

  let mk_mouse_click cx cy =
    function
      | 0
      | 1
      | 2   ->  Click (mk_v2 cx cy) (* TODO: distinguish buttons *)
      | 3   ->  ClickRelease (mk_v2 cx cy)
      | cb  ->  fail (Printf.sprintf "unexpected mouse event %d,%d,%d" cb cx cy)

  let parse_mouse_click_x10 () =
    let cb = (read_char ()) land 3 in (* Ignore modifier keys *)
    let cx = (read_char ()) - 33 in
    let cy = (read_char ()) - 33 in
    let cx' = if cx < 0 then cx + 255 else cx in
    let cy' = if cy < 0 then cy + 255 else cy in
    mk_mouse_click cx' cy' cb

  let parse_escape_sequence () =
    read_char ()
      |> array_get code_to_key_table
      |> function
          | Upper_z   -> Escape_Z
          | Upper_m   -> parse_mouse_click_x10 ()
                         (* TODO: add support for xterm-262 mode *)
          | other     -> Unknown (other |> code_of |> Char.chr)

  (* Replacement for input_char which considers 0 as Enf_of_file *)
  (* BUG: unmapped keys get all smashed into Unknown, which is not very convenient *)
  let rec next_key () : key =
    read_char ()
      |> function
          | c when c = code_timeout   ->  next_key ()
          | c when c = code_interrupt ->  EINTR
            (* BUG: how to distinquish the user pressing the real Esc key from an escape sequence ? Hacky timeout heuristic ? *)
          | c when c = code_escape    ->  next_key () = BracketLeft |> assert_that ;
                                          parse_escape_sequence ()
          | key   ->  array_get code_to_key_table key

  let rec next_key2 () : key =
    let len = 32 in
    let buffer = Bytes.make len '\000' in
    match
      Unix.read Unix.stdin buffer 0 len
    with
      (* interrupt: probably a screen resize event *)
      | exception Unix.Unix_error (Unix.EINTR, _, _)
            -> EINTR
      (* timeout: retry *)
      | 0   -> next_key2 ()
      (* one normal key *)
      | 1   -> Bytes.get buffer 0 |> Char.code |> array_get code_to_key_table
      (* invalid input *)
      | n when Bytes.get buffer 1 <> '['
            -> fail (Printf.sprintf "unexpected %d byte sequence TODO:print bytes" n)
      (* escape sequences *)
      | _ when Bytes.get buffer 2 = 'Z'
            -> Escape_Z
      (* mouse click *)
      | _ when Bytes.get buffer 2 = 'M'
            -> EINTR (* TODO: parse mouse click !! *)
      | _
            -> Unknown '\000'


end


module ScreenConfiguration = struct

  type orientation = Normal | Mirror
  type layout = Single | Columns | Rows | ColumnMajor | RowMajor

  type t = {
    layout      : layout ;
    orientation : orientation ;
  }

  let mk_config l o = {
    layout      = l ;
    orientation = o ;
  }

  module Configs = struct
    let zero    = mk_config Single  Normal
    let columns = mk_config Columns Normal
    let rows    = mk_config Rows    Normal
  end

  let flip_orientation =
    function
      | Normal -> Mirror
      | Mirror -> Normal

  let cycle_layout_next =
    function
      | Single      -> Columns
      | Columns     -> Rows
      | Rows        -> ColumnMajor
      | ColumnMajor -> RowMajor
      | RowMajor    -> Single

  let cycle_layout_prev =
    function
      | Single      -> RowMajor
      | Columns     -> Single
      | Rows        -> Columns
      | ColumnMajor -> Rows
      | RowMajor    -> ColumnMajor

  let flip_config_orientation { layout ; orientation } = {
    layout      = layout ;
    orientation = flip_orientation orientation ;
  }

  let cycle_config_layout_next { layout ; orientation } = {
    layout      = cycle_layout_next layout ;
    orientation = orientation ;
  }

  let cycle_config_layout_prev { layout ; orientation } = {
    layout      = cycle_layout_prev layout ;
    orientation = orientation ;
  }

  let split l n =
    let a = l / n in
    let r = l mod n in
    let compute_segment i =
      (* For the first 'r' tiles, add a cumulative offset of 1 for using the remainder of 'l / n'.
       * That cumulative offset happens to be min(tile index, remainder) *)
      let k = min i r in
      let l = min (i + 1) r in
      (k + i * a, l + (i + 1) * a)
    in
    Array.init n compute_segment

  let flip_xy_rect r =
    let x = rect_x r in
    let y = rect_y r in
    let w = rect_w r in
    let h = rect_h r in
    mk_rect y x (h + y) (w + x)

  let rec mk_view_ports total_area n_screen =
    function
      | { layout = Single } ->
          Array.make 1 total_area
      | _ when n_screen = 1 ->
          mk_view_ports total_area 1 Configs.zero
      | { layout = Columns ; orientation = Normal } ->
          let xo = rect_x total_area in
          let yo = rect_y total_area in
          split (rect_x_end total_area) n_screen
            |> Array.map (fun (xl, xr) -> mk_rect (xo + xl) yo (xo + xr) (rect_y_end total_area))
      | { layout = Columns ; orientation = Mirror } ->
          let views = mk_view_ports total_area n_screen (mk_config Columns Normal) in
          array_rev views ;
          views
      | { layout = Rows ; orientation } ->
          mk_config Columns orientation
            |> mk_view_ports (flip_xy_rect total_area) n_screen
            |> Array.map flip_xy_rect
      | { layout = ColumnMajor ; orientation } ->
          let halves = mk_view_ports total_area 2 (mk_config Columns orientation) in
          let minors = mk_view_ports (array_get halves 1) (n_screen - 1) Configs.rows in
          let views = Array.make n_screen (array_get halves 0) in
          array_blit minors 0 views 0 (alen minors) ;
          views
      | { layout = RowMajor ; orientation } ->
          mk_config ColumnMajor orientation
            |> mk_view_ports (flip_xy_rect total_area) n_screen
            |> Array.map flip_xy_rect
end


(* main module for interacting with the terminal *)
module Term = struct
  (* TODO: this should be platform specific *)
  let newline                     = "\r\n"

  (* ANSI escape codes weirdness: cursor positions are 1 based in the terminal referential *)
  let cursor_control_string vec2 =
    Printf.sprintf "\027[%d;%dH" (vec2.y + 1) (vec2.x + 1)

  external get_terminal_size : unit -> (int * int) = "get_terminal_size"

  let get_terminal_dimensions () =
    let (term_rows, term_cols) = get_terminal_size () in
    mk_v2 term_cols term_rows

  let do_with_raw_mode action =
    let open Unix in
    let stdout_write_string s =
      if (write_substring stdout s 0 (slen s)) <> slen s then fail "sdtout write failed"
    in
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

      Printf.fprintf logs "enter raw mode %f\n" (Sys.time() -. starttime) ;

      stdout_write_string "\027[s" ; (* cursor save *)
      stdout_write_string "\027[?47h" ; (* switch offscreen *)
      stdout_write_string "\027[?1000h" ; (* mouse event on *)
      stdout_write_string "\027[?1002h" ; (* mouse tracking on *)
      (* stdout_write_string "\027[?1004h" ; *) (* TODO: enable, switch focus event off *)
      tcsetattr stdin TCSAFLUSH want ;

      let cleanup () =
        tcsetattr stdin TCSAFLUSH initial ;
        stdout_write_string "\027[?1000l" ; (* mouse event off *)
        stdout_write_string "\027[?1002l" ; (* mouse tracking off *)
        stdout_write_string "\027[?1004l" ; (* switch focus event off *)
        stdout_write_string "\027[?47l" ; (* switch back to main screen *)
        stdout_write_string "\027[u" (* cursor restore *)
      in
      try
        action () ;
        cleanup () ;
      with
        e ->
          cleanup () ;
          raise e ;
    )

end


module Framebuffer : sig
  type t

  val init_framebuffer  : v2 -> t
  val clear             : t -> unit
  val clear_rect        : t -> rect -> unit
  val clear_line        : t -> x:int -> y:int -> len:int -> unit
  val render            : t -> unit
  val put_color_rect    : t -> Color.color_cell -> rect -> unit
  (* TODO: add a put_color_segment function *)
  val put_cursor        : t -> v2 -> unit
  val put_line          : t -> x:int -> y:int -> ?offset:int -> ?len:int -> string -> unit
  val put_framebuffer   : t -> rect -> t -> unit

end = struct

  let buffer = Buffer.create 4096

  module Default = struct
    let fg    = Color.White
    let bg    = Color.darkgray
    let z     = 0
    let text  = ' '
  end

  type t = {
    text        : Bytes.t ;
    line_lengths : int array ;
    fg_colors   : int array ;
    bg_colors   : int array ;
    z_index     : int array ;
    len         : int ;
    window      : v2 ;

    mutable cursor : v2 ;
  }

  module Rendering = struct
    let colors_equal t i j =
      let open Color in
      (t.fg_colors.(i) = t.fg_colors.(j)) && (t.bg_colors.(i) = t.bg_colors.(j))

    let next_contiguous_color_section t start =
      let rec loop t start stop =
        (* memory opt: all arguments passed explicitly *)
        if stop < t.len && colors_equal t start stop
        then loop t start (stop + 1)
        else stop
      in loop t start (start + 1)

    let next_line_len t start stop =
      min (t.window.x - (start mod t.window.x)) (stop - start)

    let render_section start stop t =
      (* append lines one at a time starting from start offset, ending at stop offset *)
      let append_newline_if_needed t position =
        let is_end_of_line        = (position mod t.window.x) = 0 in
        let is_not_end_of_buffer  = position < t.len in (* Do not append newline at the very end *)
        if is_end_of_line && is_not_end_of_buffer
          then Buffer.add_string buffer Term.newline
      in
      let rec loop t start stop colorbuffer_idx =
        (* memory opt: all arguments passed explicitly *)
        if start < stop
          then
            let len = next_line_len t start stop in
            Buffer.add_string buffer "\027[" ;
            colorbuffer_idx
              |> array_get t.fg_colors
              |> array_get Color.fg_color_control_strings
              |> Buffer.add_string buffer ;
            colorbuffer_idx
              |> array_get t.bg_colors
              |> array_get Color.bg_color_control_strings
              |> Buffer.add_string buffer ;
            Buffer.add_subbytes buffer t.text start len ;
            Buffer.add_string buffer "\027[0m" ; (* finish sequence for colored strings *)
            (* Last newline need to be appened *after* the terminating control command for colors *)
            append_newline_if_needed t (start + len) ;
            loop t (start + len) stop colorbuffer_idx
      in
        loop t start stop start

    let render_all_sections t =
      let rec loop start =
        if start < t.len
          then
            let stop = next_contiguous_color_section t start in
            render_section start stop t;
            loop stop
      in
        loop 0
  end

  let init_framebuffer vec2 =
    let len = vec2.x * vec2.y
    in {
      text        = Bytes.make len Default.text ;
      line_lengths = Array.make vec2.x 0 ; (* CLEANUP: rename me *)
      fg_colors   = Array.make len Color.white_code ;
      bg_colors   = Array.make len Color.darkgray_code ;
      z_index     = Array.make len Default.z ;
      len         = len ;
      window      = vec2 ;
      cursor      = v2_zero ;
    }

  let default_fill_len    = 8192
  let default_fg_colors   = Array.make default_fill_len Color.white_code
  let default_bg_colors   = Array.make default_fill_len Color.darkgray_code
  let default_line_length = Array.make 256 0

  let fill_fg_color t offset len color =
    color
      |> Color.color_control_code
      |> array_fill t.fg_colors offset len

  let fill_bg_color t offset len color =
    color
      |> Color.color_control_code
      |> array_fill t.bg_colors offset len

  let clear t =
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
      Bytes.fill t.text 0 t.len Default.text ;
      array_blit default_line_length 0 t.line_lengths 0 t.window.y

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

  let render frame_buffer =
    Buffer.clear buffer ;
    (* Buffer.add_string buffer "\027c" ; *) (* TODO: delete ? *)
    Buffer.add_string buffer "\027[?25l" ;    (* cursor hide *)
    Buffer.add_string buffer "\027[H" ;       (* go home *)
    Rendering.render_all_sections frame_buffer ;
    Buffer.add_string buffer (Term.cursor_control_string frame_buffer.cursor) ;
    Buffer.add_string buffer "\027[?25h" ; (* show cursor *)
    if kDRAW_SCREEN then (
      Buffer.output_buffer stdout buffer ;
      flush stdout
    )

  let update_line_end t x y =
    if (array_get t.line_lengths y) < x then
      array_set t.line_lengths y x

  let to_offset window x y =
    assert (x <= window.x) ;
    assert (y <= window.y) ;
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
    let blitlen = if len < 0 then slen s else len in
    if not (x + blitlen <= framebuffer.window.x) then
      fail  (Printf.sprintf "x:%d + blitlen:%d was not leq than framebuffer.window.x:%d" x blitlen framebuffer.window.x);
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


(* A common buffer for filling Fileview content just before bliting into a backend rendering framebuffer *)
(* TODO: Not threadsafe !! find better place to put that *)
let fb = ref (Framebuffer.init_framebuffer (mk_v2 300 80))
(* let fb = ref (Framebuffer.init_framebuffer (mk_v2 4000 100)) *)


module Screen : sig
  type t

  val screen_size       : t -> v2
  val screen_window     : t -> rect
  val screen_offset     : t -> v2
  val screen_width      : t -> int
  val screen_height     : t -> int
  val clear             : t -> unit
  val mk_screen         : Framebuffer.t -> rect -> t
  val mk_subscreen      : t -> rect -> t
  val put_color_rect    : t -> Color.color_cell -> rect -> unit
  val put_line          : t -> x:int -> y:int -> ?offset:int -> ?len:int -> string -> unit
  val put_cursor        : t -> v2 -> unit
  val put_framebuffer   : t -> Framebuffer.t -> unit

end = struct

  type t = {
    size            : v2 ;
    window          : rect ;
    frame_buffer    : Framebuffer.t ;
  }

  let screen_size    { size }    = size
  let screen_window  { window }  = window
  let screen_offset  { window }  = rect_offset window
  let screen_width   { window }  = rect_w window
  let screen_height  { window }  = rect_h window

  (* Makes a screen with 'fb' as the backend framebuffer.
   * Passed in rectangle defines the screen absolute coordinates w.r.t the framebuffer *)
  let mk_screen fb rect = {
    size          = rect_size rect ;
    window        = rect ;
    frame_buffer  = fb ;
  }

  let clear t =
    Framebuffer.clear_rect t.frame_buffer t.window

  (* Makes a subscreen from this screen.
   * Passed in rectangle defines the subscreen absolut coordinates w.r.t the parent screen *)
  let mk_subscreen { size ; window ; frame_buffer } rect =
    assert_rect_inside size rect ;
    mk_screen frame_buffer (rect_mv (rect_offset window) rect)

  let put_color_rect screen colors rect =
    let x_start   = max (rect_x rect) 0 in
    let y_start   = max (rect_y rect) 0 in
    let x_end     = min (screen_width screen) (rect_w rect) in
    let y_end     = min (screen_height screen) (rect_h rect) in
    let x_offset  = rect_x screen.window in
    let y_offset  = rect_y screen.window in
    let rect'     = mk_rect (x_offset + x_start) (y_offset + y_start) (x_offset + x_end) (y_offset + y_end) in
    Framebuffer.put_color_rect
      screen.frame_buffer
      colors
      rect'

  let put_line screen ~x:x ~y:y ?offset:(offset=0) ?len:(len=0-1) line =
    let len' = if len < 0 then slen line else len in
    let blitend = min screen.size.x (offset + len') in
    let blitlen = blitend - offset in
    if 0 <= y && y < screen.size.y then (
      Framebuffer.clear_line
        screen.frame_buffer
        ~x:(rect_x screen.window)
        ~y:((rect_y screen.window) + y)
        ~len:(rect_w screen.window) ;
      Framebuffer.put_line
        screen.frame_buffer
        ~x:((rect_x screen.window) + x)
        ~y:((rect_y screen.window) + y)
        ~offset:offset
        ~len:blitlen
        line
    )

  let put_cursor screen pos =
    screen.window
      |> rect_offset
      |> v2_add pos
      |> Framebuffer.put_cursor screen.frame_buffer

  let put_framebuffer screen src =
    Framebuffer.put_framebuffer screen.frame_buffer screen.window src
end


(* Result of a cursor movement step. *)
(* TODO: move back into TextCursor *)
type step = Nomore
          | Continue


(* A 4-directional text cursor: can move up/down one line at a time or left/right one char at a time
 * No constructor specified: Text impls provide their own TextCursor *)
module type TextCursor = sig
  type t

  val x               : t -> int        (* current column index *)
  val y               : t -> int        (* current line index *)
  val pos             : t -> v2         (* current column and line indexes as a vec *)
  val save            : t -> t
  val goto            : ?x:int -> ?y:int -> t -> unit

  val line_get        : t -> string
  val line_is_empty   : t -> bool
  val line_not_empty  : t -> bool
  val line_next       : t -> step
  val line_prev       : t -> step
  val line_first      : t -> unit
  val line_last       : t -> unit

  val char_get        : t -> char       (* TODO: what to do for empty lines ?? *)
  val char_next       : t -> step       (* move to next char, or return Nomore if cursor is at end of line *)
  val char_prev       : t -> step       (* move to previous char, or return Nomore if cursor is at beginning of line *)
  val char_zero       : t -> unit
  val char_first      : t -> unit       (* go to first non-space character *)
  val char_last       : t -> unit

  val prev            : t -> step
  val next            : t -> step

end

(* TextCursor impl for an array of strings *)
module Cursor : sig

  type t
  include TextCursor with type t := t

  val mk_cursor : string array -> int -> int -> t

end = struct

  type t = {
    text          : string array ;
    mutable x     : int ;
    mutable y     : int ;
  }

  let mk_cursor t x y = {
    text  = t ;
    x     = x ;
    y     = y ;
  }

  let x { x } = x

  let y { y } = y

  let pos { x ; y } = mk_v2 x y

  let save { text ; x ; y } = { text ; x ; y }

  let goto_y y cursor =
    let y' = min y ((alen cursor.text) - 1) in
    cursor.y <- y'

  let goto_x x cursor =
    let x' = min x ((slen (array_get cursor.text cursor.y)) - 1) in
    let x'' = max 0 x' in (* empty lines: put cursor on 0 *)
    cursor.x <- x''

  let goto ?x:(x_want = -1) ?y:(y_want = -1) cursor =
    let x_want' = if x_want < 0 then cursor.x else x_want in
    let y_want' = if y_want < 0 then cursor.y else y_want in
    goto_y y_want' cursor ;
    goto_x x_want' cursor

  let line_get { text ; y } = array_get text y

  let line_is_empty cursor = cursor |> line_get |> slen |> (=) 0

  let line_not_empty cursor = cursor |> line_get |> slen |> (<) 0

  let line_next cursor =
    let y' = cursor.y + 1 in
    if y' < alen cursor.text
      then (
        cursor.y <- y' ;
        goto_x cursor.x cursor ;
        Continue
      )
      else Nomore

  let line_prev cursor =
    let y' = cursor.y - 1 in
    if y' >= 0
      then (
        cursor.y <- y' ;
        goto_x cursor.x cursor ;
        Continue
      )
      else Nomore

  let char_get { text ; x ; y } = string_at (array_get text y) x

  let char_next cursor =
    let len = cursor |> line_get |> slen in
    let x' = cursor.x + 1 in
    if x' < len
      then (
        cursor.x <- cursor.x + 1 ;
        Continue
      )
      else Nomore

  let char_prev cursor =
    let len = cursor |> line_get |> slen in
    let x' = cursor.x - 1 in
    if len > 0 && x' >= 0
      then (
        cursor.x <- x' ;
        Continue
      )
      else Nomore

  let char_zero cursor = goto ~x:0 cursor
  let char_last cursor = goto ~x:max_int cursor

  let char_first cursor =
    goto ~x:0 cursor ;
    if not (line_is_empty cursor) then
      while cursor |> char_get |> is_space && char_next cursor = Continue
      do () done

  let line_first cursor = goto ~y:0 cursor
  let line_last cursor = goto ~y:max_int cursor

  let do_if_no_more fn cursor =
    function
      | Nomore    -> fn cursor
      | Continue  -> Continue

  (* These two next functions are ugly ! Isn't there a better way ?? *)

  let line_prev_non_empty cursor =
    if line_prev cursor = Nomore
      then Nomore
      else (
        let step = ref Continue in
        while line_is_empty cursor && !step = Continue do
          step := line_prev cursor
        done ;
        if !step = Continue
          then char_last cursor ;
        !step
      )

  let line_next_non_empty cursor =
    if line_next cursor = Nomore
      then Nomore
      else (
        let step = ref Continue in
        while line_is_empty cursor && !step = Continue do
          step := line_next cursor
        done ;
        if !step = Continue
          then char_zero cursor ;
        !step
      )

  let prev cursor =
    char_prev cursor
      |> do_if_no_more line_prev_non_empty cursor

  let next cursor =
    char_next cursor
      |> do_if_no_more line_next_non_empty cursor


  (* Conversion plan for introducing cursors little by little:
   *  1) add a vec -> cursor and cursor -> vec conversion fns
   *  2) translate little by little every movement module to use cursors internally but return vec as currently
   *  3) change the outer impl
   *)
end


module Filebuffer : sig
  type t

  val init_filebuffer     : string -> t
  val from_lines          : string -> string array -> t
  val header              : t -> string
  val filename            : t -> string
  val file_length         : t -> int
  val search              : t -> string -> rect array
  val cursor              : t -> v2 -> Cursor.t

  (* TODO: migrate fill_framebuffer to text cursor and eliminate these two *)
  val line_at             : t -> int -> string
  val line_length         : t -> int -> int

end = struct
  type t = {
    filename    : string ;
    filepath    : string ;
    header      : string ;
    buffer      : string array ;          (* the file data, line per line *)
    buflen      : int ;                   (* number of lines in buffer, may be less than buffer array length *)
  }

  let cursor { buffer } { x ; y } =
    Cursor.mk_cursor buffer x y

  let read_file f =
    let rec loop lines ch =
      match input_line ch with
      | s                     -> loop (Arraybuffer.append lines s) ch
      | exception End_of_file -> lines
    in
    let ch = open_in f in
    try
      let r = loop (Arraybuffer.reserve 32 "") ch
                |> Arraybuffer.to_array
      in
        close_in ch ;
        r
    with
      e ->
        close_in ch ;
        raise e

  let from_lines file lines =
    let filepath = (Sys.getcwd ()) ^ "/" ^ file in {
      filename      = file ;
      filepath      = filepath ;
      header        = filepath ^ "  " ^ (lines |> alen |> string_of_int) ^ "L " ;
      buffer        = lines ;
      buflen        = alen lines ;
    }

  let init_filebuffer file =
    file |> read_file |> from_lines file

  let line_at { buffer } y = array_get buffer y  (* fully apply function to avoid closure creation *)

  let line_length { buffer } y = array_get buffer y |> slen (* TODO: take into account '\t' *)

  let file_length { buflen } = buflen
  let filename { filename } = filename
  let header { header } = header

  let search filebuffer target =
    let rec loop_in_one_line width r s y xi acc =
      match Str.search_forward r s xi with
        | exception _ -> acc
        | start ->
            let next_start = start + width in
            let found = mk_rect start y (next_start - 1) y in
            loop_in_one_line width r s y next_start (found :: acc)
    in
    let rec loop_file filebuffer width r y acc =
      if y < filebuffer.buflen
        then
          let acc' = loop_in_one_line width r (line_at filebuffer y) y 0 acc in
          loop_file filebuffer width r (y + 1) acc'
        else acc
    in
    loop_file filebuffer (slen target) (Str.regexp_string target) 0 []
        |> List.rev
        |> Array.of_list
end


module Move = struct
  type t  = Left
          | Right
          | Up
          | Down
          | Start
          | End
end


module type TokenFinder = sig
  val next : string -> Token.token option -> Token.token option
end


(* CLEANUP: don't forget to delete me once all movement code is migrated to cursor *)
let cursor_position_bridge fn filebuffer p =
  let c = Filebuffer.cursor filebuffer p in
  fn c ;
  Cursor.pos c


module TokenMovement (T : TokenFinder) : sig
  val movement        : Move.t -> Filebuffer.t -> v2 -> v2
end = struct
  open Token
  open OptionCombinators

  let find_token x line =
    let rec loop line x =
      function
        | None                        -> None
        | Some tok when tok.start > x -> None
        | Some tok when x < tok.stop  -> Some tok
        | tok_opt                     -> tok_opt
                                          |> T.next line
                                          |> loop line x
    in
      loop line x (T.next line None)

  let token_start_to_cursor y { start } = mk_v2 start y
  let token_stop_to_cursor y { stop }   = mk_v2 (stop - 1) y

  let go_token_start cursor =
    Cursor.line_get cursor
      |> find_token (Cursor.x cursor)
      |> function
          | None -> ()
          | Some { start } -> Cursor.goto ~x:start cursor

  let go_token_end cursor =
    Cursor.line_get cursor
      |> find_token (Cursor.x cursor)
      |> function
          | None -> ()
          | Some { stop } -> Cursor.goto ~x:(stop - 1) cursor

  let rec go_token_first cursor =
    Cursor.char_zero cursor ;
    T.next (Cursor.line_get cursor) None
      |> function
        | Some { start } -> Cursor.goto ~x:start cursor
        | None           -> if Cursor.line_next cursor = Continue
                              then go_token_first cursor

  let go_token_right cursor =
    let rec loop line x =
      function
        | None                        -> None
        | Some tok when x < tok.start -> Some tok
        | tok_opt                     -> tok_opt
                                          |> T.next line
                                          |> loop line x
    in
      let line = Cursor.line_get cursor in
      loop line (Cursor.x cursor) (T.next line None)
        |> function
            | Some { start } -> Cursor.goto ~x:start cursor
            | None           -> if Cursor.line_next cursor = Continue
                                  then go_token_first cursor

  let rec go_token_last cursor =
    let rec loop line tok_opt =
      match (tok_opt, T.next line tok_opt) with
        | (None, None)         -> None
        | (tok,  None)         -> tok
        | (_,    next_tok_opt) -> loop line next_tok_opt
    in
      Cursor.char_last cursor ;
      loop (Cursor.line_get cursor) None
        |> function
            | Some { start } -> Cursor.goto ~x:start cursor
            | None           -> if Cursor.line_prev cursor = Continue
                                  then go_token_last cursor

  let go_token_left cursor =
    let rec loop line x tok_opt =
      T.next line tok_opt
        |> function
            | None                                -> None
            | Some tok when token_contains x tok  -> tok_opt
            | next_tok_opt                        -> loop line x next_tok_opt
    in
      loop (Cursor.line_get cursor) (Cursor.x cursor) None
        |> function
            | Some { start } -> Cursor.goto ~x:start cursor
            | None           -> if Cursor.line_prev cursor = Continue
                                  then go_token_last cursor

  let find_token_nth x line =
    let rec loop line x n =
      function
        | None                        -> None
        | Some tok when tok.start > x -> None
        | Some tok when x < tok.stop  -> Some n
        | tok_opt                     -> tok_opt
                                          |> T.next line
                                          |> loop line x (n + 1)
    in
      loop line x 0 (T.next line None)

  let go_token_nth n line =
    let rec loop line n =
      function
        | None                -> None
        | tok_opt when n = 0  -> tok_opt
        | tok_opt             -> loop line (n - 1) (T.next line tok_opt)
    in
      loop line n (T.next line None)

  let go_token_down cursor =
    let rec loop cursor n =
      if Cursor.line_next cursor = Continue
        then
          go_token_nth n (Cursor.line_get cursor)
            |> function
                | Some { start } -> Cursor.goto ~x:start cursor
                | None           -> loop cursor n
    in
      find_token_nth (Cursor.x cursor) (Cursor.line_get cursor)
        |> map (loop cursor)

  let go_token_up cursor =
    let rec loop cursor n =
      if Cursor.line_prev cursor = Continue
        then
          go_token_nth n (Cursor.line_get cursor)
            |> function
              | Some { start } -> Cursor.goto ~x:start cursor
              | None  -> loop cursor n
    in
      find_token_nth (Cursor.x cursor) (Cursor.line_get cursor)
        |> map (loop cursor)

  let movement =
    let open Move in
    function
      | Left    -> cursor_position_bridge go_token_left
      | Right   -> cursor_position_bridge go_token_right
      | Up      -> cursor_position_bridge go_token_up
      | Down    -> cursor_position_bridge go_token_down
      | Start   -> cursor_position_bridge go_token_start
      | End     -> cursor_position_bridge go_token_end
end


type kind = Include | Exclude


module type TokenKind = sig
  val kind_of : char -> kind
end


module BaseTokenFinder (T: TokenKind) : TokenFinder = struct
  open Token

  let rec find_token_start line i =
    if i < slen line && string_at line i |> T.kind_of = Exclude
      then find_token_start line (i + 1)
      else i

  let rec find_token_stop line i =
    if i < slen line && string_at line i |> T.kind_of = Include
      then find_token_stop line (i + 1)
      else i

  let next line tok_opt =
    let i = match tok_opt with
      | None      -> 0
      | Some tok  -> tok.stop
    in
      let start = find_token_start line i in
      let stop  = find_token_stop line start in
      if start = slen line
        then None
        else Some (Token.mk_tok start stop)
end


module BlockFinder = BaseTokenFinder(struct
  let kind_of c =
    if c <> ' ' && is_printable c
      then Include
      else Exclude
end)
module BlockMovement = TokenMovement(BlockFinder)


module DigitFinder = BaseTokenFinder(struct
  let kind_of c =
    if is_digit c
      then Include
      else Exclude
end)
module DigitMovement = TokenMovement(DigitFinder)


module WordFinder = BaseTokenFinder(struct
  let kind_of c =
    if is_alphanum c || c = '_'
      then Include
      else Exclude
end)
module WordMovement = TokenMovement(WordFinder)


module LineMovement = struct

  let go_line_start'  = Cursor.char_zero
  let go_line_end'    = Cursor.char_last
  let go_line_up'     = Cursor.line_prev
  let go_line_down'   = Cursor.line_next

  let go_line_left' cursor =
    Cursor.line_prev cursor |> ignore ;
    Cursor.char_first cursor

  let go_line_right' cursor =
    Cursor.line_next cursor |> ignore ;
    Cursor.char_last cursor

  let go_line_start = cursor_position_bridge go_line_start'
  let go_line_end   = cursor_position_bridge go_line_end'
  let go_line_up    = cursor_position_bridge go_line_up'
  let go_line_down  = cursor_position_bridge go_line_down'
  let go_line_left  = cursor_position_bridge go_line_left'
  let go_line_right = cursor_position_bridge go_line_right'

  let movement =
    let open Move in
    function
      | Left    -> go_line_left
      | Right   -> go_line_right
      | Up      -> go_line_up
      | Down    -> go_line_down
      | Start   -> go_line_start
      | End     -> go_line_end
end


module CharMovement = struct

  let go_char_left = cursor_position_bridge Cursor.char_prev
  let go_char_right = cursor_position_bridge Cursor.char_next

  let noop any cursor = cursor

  let movement =
    let open Move in
    function
      | Start
      | End     -> noop
      | Left    -> go_char_left
      | Right   -> go_char_right
                   (* TODO: consider changing behavior to go to first above/below line with at
                    *       least a length > to cursor.x *)
      | Up      -> LineMovement.go_line_up
      | Down    -> LineMovement.go_line_down
end


module ParagraphMovement = struct

  open Cursor

  let go_while cursor_condition cursor_step_fn cursor =
    let c = ref Continue in
    let m = ref false in
    while !c = Continue && cursor_condition cursor do
      c := cursor_step_fn cursor ;
      m := !m || ( !c = Continue )
    done ;
    !m

  let go_para_start cursor =
    if go_while line_not_empty line_prev cursor
      then line_next cursor |> ignore ;
    char_first cursor

  let go_para_end cursor =
    if go_while line_not_empty line_next cursor
      then line_prev cursor |> ignore ;
    char_last cursor

  let go_para_up cursor =
    go_para_start cursor ;
    line_prev cursor |> ignore ;
    if go_while line_is_empty line_prev cursor
      then go_para_start cursor

  let go_para_down cursor =
    go_para_end cursor ;
    line_next cursor |> ignore ;
    if go_while line_is_empty line_next cursor
      then go_para_start cursor

  let go_para_left cursor =
    go_para_start cursor ;
    line_prev cursor |> ignore ;
    if go_while line_is_empty line_prev cursor
      then go_para_end cursor

  let go_para_right cursor =
    go_para_end cursor ;
    line_next cursor |> ignore ;
    if go_while line_is_empty line_next cursor
      then go_para_end cursor

  let movement =
    let open Move in
    function
      | Start   -> cursor_position_bridge go_para_start
      | End     -> cursor_position_bridge go_para_end
      | Left    -> cursor_position_bridge go_para_left
      | Right   -> cursor_position_bridge go_para_right
      | Up      -> cursor_position_bridge go_para_up
      | Down    -> cursor_position_bridge go_para_down
end


(* TODO: migrate to Cursor *)
module type DelimiterKind = sig
  (* Returns:
   *    +1 for left delimiter: '(', '[', '{'
   *    -1 for right delimiter: ')', ']', '}'
   *     0 for others *)
  val get_kind : char -> int
end


module DelimMovement(K : DelimiterKind) = struct
  open OptionCombinators

  let is_left k = (k = 1)
  let is_right k = (k = -1)

  (* Returns kind score of char at x,y position, or 0 if x,y is not valid *)
  let get_kind_at = Cursor.char_get >> K.get_kind

  (* Go to first 'left' delimiter on the left of current position. *)
  let rec go_first_left cursor skip =
    if not skip && cursor |> get_kind_at |> is_left || Cursor.prev cursor = Nomore
      then ()
      else go_first_left cursor false

  (* Go to first 'left' delimiter on the right of current position *)
  let rec go_first_right cursor skip =
    if not skip && cursor |> get_kind_at |> is_left || Cursor.next cursor = Nomore
      then ()
      else go_first_right cursor false

  (* Go to first 'right' delimiter on the left of current position *)
  let rec go_first_end_left cursor skip =
    if not skip && cursor |> get_kind_at |> is_right || Cursor.prev cursor = Nomore
      then ()
      else go_first_end_left cursor false

  (* Move cursor left until balance is 0 *)
  let rec go_left cursor b =
    let b' = b + (get_kind_at cursor) in
    if b' = 0 || Cursor.prev cursor = Nomore
      then ()
      else go_left cursor b'

  (* Move cursor right until balance is 0 *)
  let rec go_right cursor b =
    let b' = b + (get_kind_at cursor) in
    if b' = 0 || Cursor.next cursor = Nomore
      then ()
      else go_right cursor b'

  let go_delim_left cursor =
    go_first_left cursor true

  let go_delim_end_left cursor =
    go_first_end_left cursor true

  let go_delim_right cursor =
    go_first_right cursor true

  let go_delim_start cursor =
    let k = get_kind_at cursor in
    if not (is_left k)
      then go_left cursor (0 - k - 1)

  let go_delim_end cursor =
    let k = get_kind_at cursor in
    if not (is_right k)
      then go_right cursor (1 - k)

  let go_delim_up cursor =
    go_delim_start cursor ;
    go_delim_end_left cursor ;
    go_delim_start cursor

  let go_delim_down cursor =
    go_delim_start cursor ;
    go_delim_end cursor ;
    go_delim_right cursor

  let movement : Move.t -> Filebuffer.t -> v2 -> v2 =
    let open Move in
    function
      | Left    -> cursor_position_bridge go_delim_left
      | Right   -> cursor_position_bridge go_delim_right
      | Up      -> cursor_position_bridge go_delim_up
      | Down    -> cursor_position_bridge go_delim_down
      | Start   -> cursor_position_bridge go_delim_start
      | End     -> cursor_position_bridge go_delim_end
end


module ParenMovement = DelimMovement(struct
  let get_kind =
    function
    | '('   ->  1
    | ')'   -> -1
    | _     ->  0
end)


module BracketMovement = DelimMovement(struct
  let get_kind =
    function
    | '['   ->  1
    | ']'   -> -1
    | _     ->  0
end)


module BraceMovement = DelimMovement(struct
  let get_kind =
    function
    | '{'   ->  1
    | '}'   -> -1
    | _     ->  0
end)


module MovementContext = struct
  type t = {
    (* TODO: a selected area cannot be a rect all the time when it covers multiple lines *)
    selection : rect array ;
  }
end

(* TODO: migrate to Cursor *)
module SelectionMovement = struct
  open MovementContext

  let is_v2_less_or_equal va vb = (va.y < vb.y) || (va.y = vb.y) && (va.x <= vb.x)
  let is_v2_less          va vb = (va.y < vb.y) || (va.y = vb.y) && (va.x < vb.x)

  let noop any cursor = cursor

  (* PERF: do binary search instead *)
  let selection_prev { selection } any v2 =
    let rec loop s c i =
      if i = alen s || is_v2_less_or_equal c (array_get s i |> rect_offset)
        then (i - 1 + (alen s)) mod alen s (* mod == remainder *)
        else loop s c (i + 1)
    in
    if alen selection = 0
      then v2
      else loop selection v2 0
            |> array_get selection
            |> rect_offset

  let selection_next { selection } any v2 =
    let rec loop s c i =
      if i = alen s || is_v2_less c (array_get s i |> rect_offset)
        then i mod alen s
        else loop s c (i + 1)
    in
    if alen selection = 0
      then v2
      else loop selection v2 0
            |> array_get selection
            |> rect_offset

  let select_current_rect fn { selection } any v2 =
    let rec loop s c i =
      if i = alen s
        then c
        else (
          let r = array_get s i in
          if is_v2_less_or_equal (rect_offset r) c && is_v2_less_or_equal c (rect_end r)
            then fn r
            else loop s c (i + 1))
    in
    if alen selection = 0
      then v2
      else loop selection v2 0

  let selection_start = select_current_rect rect_offset
  let selection_end   = select_current_rect rect_end

  let movement movement_context =
    let open Move in
    function
      | Up
      | Down    -> noop
      | Start   -> selection_start movement_context
      | End     -> selection_end movement_context
      | Left    -> selection_prev movement_context
      | Right   -> selection_next movement_context
end

module Movement (* TODO: formalize module signature *) = struct

  type mode = Blocks
            | Words
            | Digits
            | Lines
            | Chars
            | Paragraphs
            | Parens
            | Brackets
            | Braces
            | Selection

  type movement = Left
                | Right
                | Up
                | Down
                | Start
                | End
                | PageUp
                | PageDown
                | FileStart
                | FileEnd

  let mode_to_string =
    function
      | Blocks        -> "Blocks"
      | Words         -> "Words"
      | Digits        -> "Digits"
      | Lines         -> "Lines"
      | Chars         -> "Chars"
      | Paragraphs    -> "Paragraphs"
      | Parens        -> "Parens"
      | Brackets      -> "Brackets"
      | Braces        -> "Braces"
      | Selection     -> "Selection"

  let movement_to_string =
    function
      | Left          -> "Left"
      | Right         -> "Right"
      | Up            -> "Up"
      | Down          -> "Down"
      | Start         -> "Start"
      | End           -> "End"
      | PageUp        -> "PageUp"
      | PageDown      -> "PageDown"
      | FileStart     -> "FileStart"
      | FileEnd       -> "FileEnd"

  let page_offset = Config.default.page_size

  let move_file_start = Cursor.goto ~x:0 ~y:0
  let move_file_end   = Cursor.line_last

  let move_page_up cursor =
    let y' = (Cursor.y cursor) - page_offset in
    let y'' = max 0 y' in
    Cursor.goto ~y:y'' cursor

  let move_page_down cursor =
    let y' = (Cursor.y cursor) + page_offset in
    let y'' = max 0 y' in
    Cursor.goto ~y:y'' cursor

  let move movement_context =
    function
      | Blocks        -> BlockMovement.movement
      | Words         -> WordMovement.movement
      | Digits        -> DigitMovement.movement
      | Lines         -> LineMovement.movement
      | Chars         -> CharMovement.movement
      | Paragraphs    -> ParagraphMovement.movement
      | Parens        -> ParenMovement.movement
      | Brackets      -> BracketMovement.movement
      | Braces        -> BraceMovement.movement
      | Selection     -> SelectionMovement.movement movement_context

  let compute_movement movement_context mode =
    function
      | Left          -> move movement_context mode Move.Left
      | Right         -> move movement_context mode Move.Right
      | Up            -> move movement_context mode Move.Up
      | Down          -> move movement_context mode Move.Down
      | Start         -> move movement_context mode Move.Start
      | End           -> move movement_context mode Move.End
      | PageUp        -> cursor_position_bridge move_page_up
      | PageDown      -> cursor_position_bridge move_page_down
      | FileStart     -> cursor_position_bridge move_file_start
      | FileEnd       -> cursor_position_bridge move_file_end
end


type redraw_level = Nodraw | FrameDirty | Redraw


(* TextView is an intermediary step in writing a filebuffer section on the screen
 * It helps with layout composition. *)
module TextView : sig
  type t

  val mk_textview               : int -> t
  val fill                      : t -> int -> Cursor.t -> unit
  val relative_lineno           : t -> unit
  val h_offset                  : t -> int -> unit

end = struct

  (* TODO: also count tabs for elastic tabs ! *)
  type t = {
    linenos           : int array ;
    lines             : string array ;
    line_offsets      : int array ;
    line_lens         : int array ;
    mutable cursor_x  : int ;
    mutable cursor_y  : int ;
    (* Consider adding h_offset and v_offset ? *)
  }

  let no_lineno = min_int (* Used to indicate wrapped lines *)

  let mk_textview len = {
    linenos         = Array.make len 0 ;    (* When a line is wrapped, its lineno is 'no_lineno' *)
    lines           = Array.make len "" ;
    line_offsets    = Array.make len 0 ;
    line_lens       = Array.make len 0 ;
    cursor_x        = 0 ;
    cursor_y        = 0 ;
  }

  let rec fill_lines textview cursor i =
    let line = Cursor.line_get cursor in
    let lineno = 1 + (Cursor.y cursor) in
    array_set textview.linenos i lineno ;
    array_set textview.lines i line ;
    array_set textview.line_offsets i 0 ;
    array_set textview.line_lens i (slen line) ;
    (* BUG? is this skipping the last line ? *)
    if Cursor.line_next cursor = Continue && i + 1 < alen textview.lines
      then fill_lines textview cursor (i + 1)

  let fill textview v_offset cursor =
    let lineno_zero = textview.cursor_y - v_offset in
    assert_that (0 <= lineno_zero) ;
    assert_that (0 <= v_offset) ;
    assert_that (v_offset < alen textview.lines) ;
    textview.cursor_x <- Cursor.x cursor ;
    textview.cursor_y <- Cursor.y cursor - v_offset ;
    Cursor.goto ~y:lineno_zero cursor ;
    fill_lines textview cursor 0

  let relative_lineno textview =
    for i = 0 to astop textview.linenos do
      let lineno = textview.linenos.(i) in
      if lineno > no_lineno
        then array_set textview.linenos i (lineno - textview.cursor_y)
    done

  let h_offset textview h_offset =
    (* TODO: needs to be added to the position mapping matrix *)
    textview.cursor_x <- textview.cursor_x - h_offset ;
    for i = 0 to astop textview.line_offsets do
      array_set textview.line_offsets i h_offset
    done

  let view_position_to_text_position x y =
    (* This can be computed if I keep the horizontal offset, and the cursor mapping *)
    mk_v2 x y

end


module Fileview : sig
  type t

  val init_fileview           : Filebuffer.t -> t
  val set_mov_mode            : Movement.mode -> t -> t
  val apply_movement          : Movement.movement -> v2 -> t -> t
  val cursor                  : t -> v2
  val adjust_view             : v2 -> t -> t
  val swap_line_number_mode   : t -> t
  val swap_linebreaking_mode  : t -> t
  val toggle_show_token       : t -> t
  val toggle_show_neighbor    : t -> t
  val toggle_show_selection   : t -> t
  val recenter_view           : v2 -> t -> t
  val draw                    : t -> Screen.t -> redraw_level -> bool -> unit

end = struct

  open Filebuffer

  type filebuffer = Filebuffer.t
  type screen     = Screen.t

  type numbering_mode = Absolute | CursorRelative

  module LineNumberCache = struct
    let line_number_cache_t1 = Sys.time () ;;
    (* TODO: - dynmically populate cache as needed by resizing the cache array if needed *)

    let negative_offset = 200
    let hardcoded_size  = 10000 + negative_offset

    let format_n n =
      Printf.sprintf "%5d " (n - negative_offset)

    let cache =
      Array.init hardcoded_size format_n

    let get base_offset n =
      array_get cache (base_offset + n + negative_offset)

    let line_number_cache_t2 = Sys.time () ;;
    Printf.fprintf logs "cache %f\n" (line_number_cache_t2 -. line_number_cache_t1) ;;
  end

  type t = {
    filebuffer        : filebuffer ;
    cursor            : v2 ;       (* current position in file space: x = column index, y = row index *)
    view              : v2 ;       (* x,y offset of the rectangle view into the text *)
    numbering         : numbering_mode ;
    mov_mode          : Movement.mode ;
    show_token        : bool ;
    show_neighbor     : bool ;
    show_selection    : bool ;
    context           : MovementContext.t ;
  }

  let init_fileview filebuffer = {
    filebuffer        = filebuffer ;
    cursor            = v2_zero ;
    view              = v2_zero ;
    numbering         = CursorRelative ;
    mov_mode          = Movement.Chars ;
    show_token        = true ;
    show_neighbor     = true ;
    show_selection    = true ;
    context           = let open MovementContext in {
      selection = Filebuffer.search filebuffer "end" ;
    } ;
  }

  let set_mov_mode m t = {
    t with
      mov_mode = m ;
  }

  let cursor { cursor } =
    cursor

  let view_height { view ; filebuffer } =
    (Filebuffer.file_length filebuffer) - view.y

  let adjust_view { x ; y } t =
    let text_height = y - 2 in (* -1 for header line, -1 for indexing starting at 0 *)
    let text_width  = x - 6 in (* -6 for line numbering CLEANUP: remove this hardcoded offset *)
    let view_x =
      if t.cursor.x < t.view.x then
        0
      else if t.cursor.x > t.view.x + text_width then
        t.cursor.x - text_width
      else
        t.view.x
    in
    let view_y =
      if t.cursor.y < t.view.y then
        t.cursor.y
      else if t.cursor.y > t.view.y + text_height then
        t.cursor.y - text_height
      else
        t.view.y
    in {
      t with
        view  = mk_v2 view_x view_y ;
    }

  let apply_movement mov screen_size t =
    let cursor' = Movement.compute_movement t.context t.mov_mode mov t.filebuffer t.cursor in

    if kDEBUG then (
      let msg =
        Printf.sprintf"apply_movement after mode=%s mov=%s %d,%d -> %d,%d\n"
          (Movement.mode_to_string t.mov_mode)
          (Movement.movement_to_string mov)
          t.cursor.y t.cursor.x
          cursor'.y cursor'.x
      in
      assert_that (cursor'.y >= 0) ~msg:msg ;
      assert_that (cursor'.y < Filebuffer.file_length t.filebuffer) ~msg:msg
    ) ;

    adjust_view screen_size { t with cursor = cursor' }

  let swap_line_number_mode t =
    let new_mode = match t.numbering with
    | Absolute        -> CursorRelative
    | CursorRelative  -> Absolute
    in {
      t with
        numbering = new_mode ;
    }

  let swap_linebreaking_mode = id

  let toggle_show_token t = {
    t with
      show_token  = not t.show_token ;
    }

  let toggle_show_neighbor t = {
    t with
      show_neighbor = not t.show_neighbor ;
  }

  let toggle_show_selection t = {
    t with
      show_selection  = not t.show_selection ;
  }

  (* TODO: should y_recenter and x_recenter together or separately ? *)
  let recenter_view { x ; y } t =
    let view_x = t.cursor.x - x / 2 in
    let view_y = t.cursor.y - y / 2 in {
      t with
        view = mk_v2 (max view_x 0) (max view_y 0) ;
    }

  (* TODO: move drawing in separate module ? *)

  (* CLEANUP: probably I should draw the line number separately from the main text: this would have a number
   * of benefits in term of codeflow simplicity and not having to put +6 horizontal offsets everywhere and
   * recreate the same objects. *)

  let frame_default_line      = "~  "

  (* TODO: fileview should remember the last x scrolling offset and make it sticky, like the y scrolling *)
  let fill_framebuffer t is_focused screen framebuffer =
    let screen_size = Screen.screen_size screen in
    let text_width  = screen_size.x in
    let text_height = screen_size.y in
    let text_stop_y = min text_height (view_height t) in

    let line_number_offset =
      match t.numbering with
        | Absolute        -> 1
        | CursorRelative  -> -t.cursor.y
    in

    let last_x_index = text_width - 6 in
    let base_scrolling_offset = last_x_index - 1 in
    let (cursor_x, scrolling_offset) =
      if t.cursor.x < last_x_index
        then (t.cursor.x, 0)
        else (base_scrolling_offset, t.view.x) (* t.cursor.x - base_scrolling_offset) *)
    in
    let cursor = mk_v2 (cursor_x + 6) (t.cursor.y - t.view.y) in
    if is_focused then
      Screen.put_cursor screen cursor ;

    let x_scrolling_offset = scrolling_offset in

    (* Text area *)
    for i = 0 to text_stop_y - 1 do
      let line_idx = i + t.view.y in
      let l = Filebuffer.line_at t.filebuffer line_idx in
      let x_len = (slen l) - x_scrolling_offset in
      Framebuffer.put_line framebuffer ~x:0 ~y:i ~len:6 (LineNumberCache.get line_number_offset line_idx) ;
      if x_len > 0 then
        Framebuffer.put_line framebuffer ~x:6 ~y:i ~offset:x_scrolling_offset ~len:x_len l
    done ;

    (* Text area color blocks *)
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.line_numbers
      (mk_rect 0 0 6 text_stop_y) ;

    (* Show cursor lines *)
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.cursor_line
      (mk_rect 6 (t.cursor.y - t.view.y) text_width (t.cursor.y - t.view.y)) ;
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.cursor_line
      (mk_rect (t.cursor.x + 6 - x_scrolling_offset) 0 (t.cursor.x + 7 - x_scrolling_offset) text_height) ;

    (* Show token where cursor currently is *)
    if t.show_token then (
      let token_s = Movement.compute_movement t.context t.mov_mode Movement.Start t.filebuffer t.cursor in
      let token_e = Movement.compute_movement t.context t.mov_mode Movement.End t.filebuffer token_s in
      let y_start = token_s.y - t.view.y in
      let y_end   = token_e.y - t.view.y in
      (* The current token can leak out of the current screen: bound start and stop to the screen *)
      for y = max 0 y_start to min y_end text_stop_y do
        let len = Filebuffer.line_length t.filebuffer (y + t.view.y) in
        let x0 =
          if y = y_start then token_s.x else 0 in       (* start from x=0 for lines after the first *)
        let x1 =
          if y = y_end then (token_e.x + 1) else len in (* ends at end-of-line for lines before the last *)
        let x0' = 6 + (max 0 ((min x0 x1) - x_scrolling_offset)) in
        let x1' = 6 + (max x0 x1) - x_scrolling_offset in
        (* TODO: this should use a put_color_line api to avoid making rect records *)
        (* BUG: add horizontal_scrolling correction ?? *)
        if 6 <= x0' && x0' < x1' then
          Framebuffer.put_color_rect
            framebuffer Config.default.colors.current_token (mk_rect x0' y x1' y)
      done ;
    ) ;

    (* Show selection *)
    if t.show_selection then (
      let show_selection selection_rect =
          let tl_x = 6 + (max 0 ((rect_x selection_rect) - x_scrolling_offset)) in
          let tl_y = (rect_y selection_rect) - t.view.y in
          let br_x = (rect_x_end selection_rect) + 7 - x_scrolling_offset in
          let br_y = (rect_y_end selection_rect) - t.view.y in
          if 0 <= br_y && tl_y < text_height && tl_x < br_x then (
            let r = mk_rect tl_x tl_y br_x br_y in
            Framebuffer.put_color_rect framebuffer Config.default.colors.selection r
          )
      in
      Array.iter show_selection t.context.selection ;
    ) ;

    (* Show Left/Right/Up/Down tokens relatively to where current token is *)
    if t.show_neighbor then (
      let lightup_pixel t framebuffer colors pos =
          let x = pos.x + 6 - x_scrolling_offset in
          let y = pos.y - t.view.y  in
          if 6 <= x then
            Framebuffer.put_color_rect framebuffer colors (mk_rect x y (x + 1) y)
      in

      Movement.compute_movement t.context t.mov_mode Movement.Left t.filebuffer t.cursor
        |> lightup_pixel t framebuffer Config.default.colors.leftright_neighbor ;
      Movement.compute_movement t.context t.mov_mode Movement.Right t.filebuffer t.cursor
        |> lightup_pixel t framebuffer Config.default.colors.leftright_neighbor ;
      Movement.compute_movement t.context t.mov_mode Movement.Up t.filebuffer t.cursor
        |> lightup_pixel t framebuffer Config.default.colors.updown_neighbor ;
      Movement.compute_movement t.context t.mov_mode Movement.Down t.filebuffer t.cursor
        |> lightup_pixel t framebuffer Config.default.colors.updown_neighbor ;
    ) ;

    (* No-text area *)
    for y = text_stop_y to text_height do
      Framebuffer.put_line framebuffer ~x:0 ~y:y frame_default_line
    done ;
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.no_text
      (mk_rect 0 text_stop_y 1 text_height) ;

    (* Cursor vertical line *)
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.string
      (mk_rect 0 cursor.y 6 cursor.y)

  let put_border_frame t screen header_color =
    Screen.put_line screen ~x:0 ~y:0
      (Printf.sprintf
        "%s %d,%d  mode=%s"
        (Filebuffer.header t.filebuffer)
        t.cursor.y t.cursor.x
        (Movement.mode_to_string t.mov_mode)) ;
    (* header *)
    Screen.put_color_rect
      screen
      header_color
      (mk_rect 0 0 (Screen.screen_width screen) 0) ;
    (* border *)
    Screen.put_color_rect
      screen
      Config.default.colors.border
      (* BUG: this overlap with first char of top header ! *)
      (mk_rect 0 1 1 (Screen.screen_height screen))

  let draw t screen redraw is_focused =
    let textscreen = Screen.mk_subscreen screen
      (* 1 for border column, 1 for header space *)
      (mk_rect 1 1 (Screen.screen_width screen) (Screen.screen_height screen))
    in
    let header_color =
      if is_focused
        then Config.default.colors.focus_header
        else Config.default.colors.header
    in
    let framebuffer = !fb in
    match redraw with
    | Nodraw ->
        ()
    | FrameDirty ->
        put_border_frame t screen header_color
    | Redraw -> (
        put_border_frame t screen header_color ;
        Screen.clear textscreen ;
        Framebuffer.clear framebuffer ;
        fill_framebuffer t is_focused textscreen framebuffer ;
        Screen.put_framebuffer textscreen framebuffer ;
      )


(* NEW fill_screen function

  features to take into account:
    correctly iterating through the text line per line
    put the cursor at the right vertical offset
    put beginning of lines at the right horizontal offset
    correctly expand tabs
      support fixed tabs columns
      support elastic tabs
    put all color segments
    support mouse cursor to text cursor mapping
    support horizontal and vertical screen dragging

    screen and/or framebuffer should be enhanced with a mouse_cursor _coord_mapping

    possible data transformation flow:

      cursor + view height
        -> text array  // array can be cached for the screen and reused
        -> text array + tab annotations
        -> text array is mutated to account for line overflow, line clipping, horizontal offset
        -> text array is finally blitted onto screen ?

        Q: where to insert selections and color blocks ?
          text array can keep range per line of y offset and x ranges -> this guide the coloring
*)

end


module Stats = struct

  type t = {
    gc_stats            : Gc.stat ;
    last_major_words    : float ;
    last_minor_words    : float ;
    timestamp           : float ;
    last_input_duration : float ;
    last_cycle_duration : float ;
    last_key_input      : Keys.key ;
  }

  let init_stats () = {
    gc_stats            = Gc.quick_stat () ;
    last_major_words    = 0. ;
    last_minor_words    = 0. ;
    timestamp           = Sys.time () ;
    last_input_duration = 0. ;
    last_cycle_duration = 0. ;
    last_key_input      = Keys.Unknown '?' ;
  }

  let update_stats key now input_duration stats = {
    gc_stats            = Gc.quick_stat () ;
    last_major_words    = stats.gc_stats.Gc.major_words ;
    last_minor_words    = stats.gc_stats.Gc.minor_words ;
    timestamp           = now ;
    last_input_duration = input_duration ;
    last_cycle_duration = now -. stats.timestamp ;
    last_key_input      = key ;
  }

  let word_byte_size =
    float (Sys.word_size / 8)

  let format_memory_counter word_count =
    match word_count *. word_byte_size with
    | x when x < 1024.          -> Printf.sprintf "%.2fB" x
    | x when x < 1024. *. 1024. -> Printf.sprintf "%.2fkB" (x /. 1024.)
    | x                         -> Printf.sprintf "%.2fMB" (x /. 1024. /. 1024.)

  let format_stats stats =
    let open Gc in
    Printf.sprintf "  heap = %s  alloc = (major: %s  minor: %s)  time = %.3fms"
      (format_memory_counter (float stats.gc_stats.Gc.heap_words))
      (format_memory_counter (stats.gc_stats.Gc.major_words -. stats.last_major_words))
      (format_memory_counter (stats.gc_stats.Gc.minor_words -. stats.last_minor_words))
      (1000. *. (stats.last_cycle_duration -. stats.last_input_duration))

  let log_stats f stats =
    output_string f "frame_time_ms " ;
    output_float f stats.last_cycle_duration ;
    output_string f "\n" ;
    output_string f "heap_words " ;
    output_int f stats.gc_stats.Gc.heap_words ;
    output_string f "\n" ;
    output_string f "minor_words " ;
    output_float f (stats.gc_stats.Gc.minor_words -. stats.last_minor_words) ;
    output_string logs "\n" ;
    output_string f "key " ;
    output_string f (Keys.descr_of stats.last_key_input) ;
    output_string f " " ;
    output_int f (Keys.code_of stats.last_key_input) ;
    output_string logs "\n"
end


module Tileset = struct

  type op = Resize of rect
          | FileviewOp of (v2 -> Fileview.t -> Fileview.t)
          | RotateViewsLeft
          | RotateViewsRight
          | ScreenLayoutCycleNext
          | ScreenLayoutCyclePrev
          | ScreenLayoutFlip
          | FocusNext
          | FocusPrev
          | FocusMain
          | BringFocusToMain
          | Selection of v2

  type t = {
    screen_size   : rect ;
    screen_config : ScreenConfiguration.t ;
    screen_tiles  : rect array ;
    focus_index   : int ;
    fileviews     : Fileview.t array ;
    redrawing     : redraw_level array ;
  }

  let adjust_fileviews screen_tiles fileviews =
    let n_tiles = alen screen_tiles in
    let adjust_fileview i =
      let view = array_get fileviews i in
      (* In Single tile mode, len tiles < len fileviews *)
      if i < n_tiles
        then Fileview.adjust_view (array_get screen_tiles i |> rect_size) view
        else view
    in
    Array.init (alen fileviews) adjust_fileview

  let mk_tileset ?draw:(redraw=Redraw) focus_index size screenconfig fileviews =
    assert (focus_index < alen fileviews) ;
    let screen_tiles  = ScreenConfiguration.mk_view_ports size (alen fileviews) screenconfig
    in {
      screen_size   = size ;
      screen_config = screenconfig ;
      screen_tiles  = screen_tiles ;
      focus_index   = focus_index ;
      fileviews     = adjust_fileviews screen_tiles fileviews ;
      redrawing     = Array.make (alen fileviews) redraw ;
    }

  let set_focus t focus_index =
    let t' = {
      t with
        focus_index = focus_index ;
        redrawing   = Array.make (alen t.fileviews) FrameDirty ;
    } in
    if t.screen_config.layout = ScreenConfiguration.Single then
      array_set t'.redrawing focus_index Redraw ;
    t'

  let next_focus_index t =
    (t.focus_index + 1) mod alen t.fileviews

  let prev_focus_index t =
    (t.focus_index + (alen t.fileviews) - 1) mod alen t.fileviews

  let draw_fileview t frame_buffer view_idx tile_idx is_focused =
    Fileview.draw
        (array_get t.fileviews view_idx)
        (array_get t.screen_tiles tile_idx |> Screen.mk_screen frame_buffer)
        (array_get t.redrawing view_idx)
        is_focused

  let draw_fileviews t frame_buffer =
    let n_screens = alen t.screen_tiles in
    if n_screens = 1
      then
        draw_fileview t frame_buffer t.focus_index 0 true
      else
        for i = 0 to n_screens - 1 do
          draw_fileview t frame_buffer i i (i = t.focus_index)
        done

  let get_focused_tile t =
    if alen t.screen_tiles > 1
      then array_get t.screen_tiles t.focus_index
      else array_get t.screen_tiles 0

  let apply_op t =
    (* Any operation that changes the terminal size has to use mk_tileset for recomputing the tiles
     * Any operation that remaps tiles and fileviews has to use mk_tileset for adusting fileview height *)
    function
      | Resize new_screen_size ->
          mk_tileset
            t.focus_index new_screen_size t.screen_config t.fileviews
      | FileviewOp fileview_op ->
          let fileviews' = Array.copy t.fileviews in
          t.focus_index
            |> array_get fileviews'
            |> fileview_op (t |> get_focused_tile |> rect_size)
            |> array_set fileviews' t.focus_index ;
          let t' = {
            t with
              fileviews = fileviews' ;
              redrawing = Array.make (alen fileviews') Nodraw ;
          } in
            array_set t'.redrawing t'.focus_index Redraw ;
            t'
      | ScreenLayoutCycleNext ->
          let new_config = ScreenConfiguration.cycle_config_layout_next t.screen_config in
          mk_tileset
            t.focus_index t.screen_size new_config t.fileviews
      | ScreenLayoutCyclePrev ->
          let new_config = ScreenConfiguration.cycle_config_layout_prev t.screen_config in
          mk_tileset
            t.focus_index t.screen_size new_config t.fileviews
      | ScreenLayoutFlip ->
          let new_config = ScreenConfiguration.flip_config_orientation t.screen_config in
          mk_tileset
            t.focus_index t.screen_size new_config t.fileviews
      | FocusNext ->
          set_focus t (next_focus_index t)
      | FocusPrev ->
          set_focus t (prev_focus_index t)
      | FocusMain ->
          set_focus t 0
      | BringFocusToMain ->
          let fileviews' = Array.copy t.fileviews in
          array_get t.fileviews t.focus_index |> array_set fileviews' 0 ;
          array_get t.fileviews 0             |> array_set fileviews' t.focus_index ;
          let t' =
            mk_tileset ~draw:Nodraw 0 t.screen_size t.screen_config fileviews'
          in
            array_set t'.redrawing 0 Redraw ;
            array_set t'.redrawing t.focus_index Redraw ;
            t'
      | RotateViewsRight ->
          let fileviews' = Array.copy t.fileviews in
          let focus_index' = next_focus_index t in
          array_swap fileviews' t.focus_index focus_index' ;
          let t' =
            mk_tileset ~draw:Nodraw focus_index' t.screen_size t.screen_config fileviews'
          in
            array_set t'.redrawing t.focus_index Redraw ;
            array_set t'.redrawing focus_index' Redraw ;
            t'
      | RotateViewsLeft ->
          let fileviews' = Array.copy t.fileviews in
          let focus_index' = prev_focus_index t in
          array_swap fileviews' t.focus_index focus_index' ;
          let t' =
            mk_tileset ~draw:Nodraw focus_index' t.screen_size t.screen_config fileviews'
          in
            array_set t'.redrawing t.focus_index Redraw ;
            array_set t'.redrawing focus_index' Redraw ;
            t'
      | Selection pos ->
          let pos_in_tile {x ; y } r =
            let x0 = rect_x r in
            let y0 = rect_y r in
            let x1 = rect_x_end r in
            let y1 = rect_y_end r in
            x0 <= x && x < x1 && y0 <= y && y < y1
          in
          let i = array_find (pos_in_tile pos) t.screen_tiles in
          Printf.fprintf logs "selecting tile %d for pos %d,%d\n" i pos.x pos.y ;
          flush logs ;
          if i < 0
            then t
            else set_focus t i
                  (* TODO: adjust cursor to p inside that tile *)
end


module Navigator = struct

  let read_dir is_rec prefix path e =
    let rec read_dir_handle is_rec prefix h e =
      try
        let s = Unix.readdir h in
        let e' =
          if is_rec && s = "." || s = ".."
            then e
            else Arraybuffer.append e (prefix ^ s)
        in
          read_dir_handle is_rec prefix h e'
      with
        _ -> e
    in
    try
      let h = Unix.opendir path in
      let e' = read_dir_handle is_rec prefix h e in
      Unix.closedir h ;
      e'
    with
      _ -> e

  let path_to_prefix p =
    if p = "." then "" else p ^ "/"

  let rec read_dir_rec p e =
    let open Arraybuffer in
    let e' = ref (read_dir true (path_to_prefix p) p e) in
    for i = e.next to !e'.next - 1 do
      (* BUG: do not follow links, infinite loop risks otherwise ! *)
      e' := read_dir_rec (array_get !e'.data i) !e'
    done ;
    !e'

  let dir_ls_rec path =
    let e =
      Arraybuffer.empty ()
        |> read_dir_rec path
        |> Arraybuffer.to_array
    in
      Array.sort String.compare e ;
      e

  let dir_ls path =
    let e =
      Arraybuffer.empty ()
        |> read_dir false (path_to_prefix path) path
        |> Arraybuffer.to_array
    in
      Array.sort String.compare e ;
      e

  let dir_to_filebuffer     p = p |> dir_ls |> Filebuffer.from_lines p
  let dir_rec_to_filebuffer p = p |> dir_ls_rec |> Filebuffer.from_lines p

end


(* Experimenting with parametrizing raw input capture for reusability:
 *   - insert mode
 *   - commands
 *   - search
 * TODO: consider plumbing in token movements ?
 *)
module InputCapture = struct

  type 'a t = {
    cursor:   int ;
    content:  string ;
    endvalue: 'a ;
  }

  type 'a input_parser = {
    next_char : unit -> char ;
    escape    : char -> 'a option ;
  }

  (* Pass in an escape hatch *)
  let capture_input { next_char ; escape } =
    let rec loop cursor content =
      let c = () |> next_char in
      match escape c with
        | Some a  -> {
          cursor    = cursor ;
          content   = content ;
          endvalue  = a ;
        }
        | None    ->
            (match c with
              (* BUG: do inserts and deletes at cursor position *)
              | '\127'  ->
                let content' = String.sub content 0 (max 0 ((slen content) - 1)) in
                let cursor' = max 0 (cursor - 1) in
                loop cursor' content'
              | c       ->
                loop (cursor + 1) (content ^ (Char.escaped c)))
    in
      loop 0 ""
end


module Ciseau = struct

  type mode = Normal
            | RawInput

  let mode_to_string =
    function
      | Normal      ->  "Normal"
      | RawInput    ->  "RawInput"

  let mode_to_color =
    function
      | Normal      ->  Config.default.colors.status_normal
      | RawInput    ->  Config.default.colors.status_input

  type pending_command_atom = Digit of int

  type pending_command = None
                       | Number of int list

  (* Represents an editor command *)
  type command = Noop
               | Stop     (* this is an Editor command *)
               | Resize   (* this is a TilesetOp *)
               | TilesetOp of Tileset.op
               | MoveOp of Movement.movement
               | MoveModeOp of Movement.mode
               | View of (Fileview.t -> Fileview.t) (* View ops should probably be moved to Tileset *)
               | Pending of pending_command_atom
               | Mode of mode

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
    term_dim        : v2 ;
    term_dim_descr  : string ;
    frame_buffer    : Framebuffer.t ;
    running         : bool ;
    status_screen   : Screen.t ;
    tileset         : Tileset.t ;
    filebuffers     : Filebuffer.t array ; (* TODO: turn this into buffer management layer *)
    user_input      : string ;
    pending_input   : pending_command ;
    stats           : Stats.t ;
    log_stats       : bool ;
    mode            : mode ;
  }

  let main_screen_dimensions term_dim =
    mk_rect 0 0 term_dim.x (term_dim.y - 2)

  let status_screen_dimensions term_dim =
    mk_rect 0 (term_dim.y - 2) term_dim.x term_dim.y

  let mk_status_screen frame_buffer term_dim =
    term_dim |> status_screen_dimensions |> Screen.mk_screen frame_buffer

  let mk_window_size_descr { x = w ; y = h } =
    Printf.sprintf "(%d x %d)" w h

  let test_mk_filebuffers file = [|
      Filebuffer.init_filebuffer file ;
      Filebuffer.init_filebuffer "./start_tmux.sh" ;
      Filebuffer.init_filebuffer "./ioctl.c" ;
      (*
      Filebuffer.init_filebuffer "./Makefile" ; (* FIX tabs *)
      *)
      (*
      Navigator.dir_rec_to_filebuffer "." ;
      Navigator.dir_to_filebuffer (Sys.getcwd ()) ;
      *)
    |]

  let test_mk_fileviews term_dim =
    Array.map Fileview.init_fileview

  let mk_tileset term_dim filebuffers =
    filebuffers
      |> Array.map Fileview.init_fileview
      |> Tileset.mk_tileset 0 (main_screen_dimensions term_dim) ScreenConfiguration.Configs.columns

  let init_editor file =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_framebuffer term_dim in
    let filebuffers = test_mk_filebuffers file in
    {
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      frame_buffer    = frame_buffer ;
      running         = true ;
      status_screen   = mk_status_screen frame_buffer term_dim ;

      tileset         = mk_tileset term_dim filebuffers ;
      filebuffers     = filebuffers ;

      user_input      = "" ;
      pending_input   = None;
      stats           = Stats.init_stats () ;
      log_stats       = kLOG_STATS ;
      mode            = Normal ;
    }

  let resize_editor editor =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_framebuffer term_dim in
    {
      editor with
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      frame_buffer    = frame_buffer ;
      status_screen   = mk_status_screen frame_buffer term_dim ;
      tileset         = Tileset.apply_op editor.tileset (Tileset.Resize (main_screen_dimensions term_dim)) ;
    }

  let stop_editor editor = {
      editor
        with
        running = false
    }

  let apply_operation op editor = {
      editor
        with
        tileset = Tileset.apply_op editor.tileset op
    }

  let queue_pending_command n editor = {
      editor
        with
        pending_input = enqueue_digit n editor.pending_input
    }

  let change_mode mode editor = {
      (* TODO: needs to apply necessary cleanups from outgoing mode *)
      editor
        with
        mode        = mode ;
        user_input  = "" ;
    }

  let rec apply_command =
    function
      | Noop    -> id
      | Stop    -> stop_editor
      | Resize  -> resize_editor
      | TilesetOp op ->
          apply_operation op
      | MoveOp m ->
          let fn = Fileview.apply_movement m in
          let op = Tileset.FileviewOp fn in
          apply_operation op
      | MoveModeOp m ->
          let fn any t = Fileview.set_mov_mode m t in
          let op = Tileset.FileviewOp fn in
          apply_operation op
      | View fn ->
          let fn' any fileview = fn fileview in
          let op = Tileset.FileviewOp fn' in
          apply_operation op
      | Pending (Digit n) ->
          (* cannot happen ?? *)
          queue_pending_command n
      | Mode mode ->
          change_mode mode

  let apply_command_with_repetition n command editor =
    match command with
    | MoveOp m ->
          let rec loop m n viewsize fb =
            if (n > 0)
              then loop m (n - 1) viewsize (Fileview.apply_movement m viewsize fb)
              else fb
          in
          let fileview_op = Tileset.FileviewOp (loop m n) in {
            editor with
            tileset = Tileset.apply_op editor.tileset fileview_op ;
            pending_input = None ;
          }
    | Pending (Digit n) ->
          queue_pending_command n editor
      (* for other command, flush any pending digits *)
    | _ ->
          apply_command command { editor with pending_input = None }

  let show_status editor =
    let status_text1 = "Ciseau stats: win = "
                      ^ editor.term_dim_descr
                      ^ (Stats.format_stats editor.stats)
    in
    let status_text2 = editor.user_input
    in
    Screen.put_line editor.status_screen ~x:0 ~y:0 status_text1 ;
    Screen.put_line editor.status_screen ~x:0 ~y:1 status_text2 ;
    Screen.put_color_rect editor.status_screen (mode_to_color editor.mode) (mk_rect 0 0 editor.term_dim.x 0)

  let refresh_screen editor =
    Tileset.draw_fileviews editor.tileset editor.frame_buffer ;
    show_status editor ;
    Framebuffer.render editor.frame_buffer ;
    editor

  let key_to_command = function
    | Keys.Tab          -> Mode RawInput
    | Keys.Ctrl_c       -> Stop
    | Keys.Backslash    -> View Fileview.swap_line_number_mode
    | Keys.Pipe         -> View Fileview.swap_linebreaking_mode
    | Keys.Colon        -> View Fileview.toggle_show_token
    | Keys.SemiColon    -> View Fileview.toggle_show_neighbor
    | Keys.SingleQuote  -> View Fileview.toggle_show_selection
                           (* CLEANUP: try to separate TilesetOp and FileviewOp with different variants *)
    | Keys.ParenLeft    -> TilesetOp Tileset.RotateViewsLeft
    | Keys.ParenRight   -> TilesetOp Tileset.RotateViewsRight
    | Keys.BraceLeft    -> TilesetOp Tileset.ScreenLayoutCyclePrev
    | Keys.BraceRight   -> TilesetOp Tileset.ScreenLayoutCycleNext
    | Keys.BracketLeft  -> TilesetOp Tileset.FocusPrev
    | Keys.BracketRight -> TilesetOp Tileset.FocusNext
    | Keys.Underscore   -> TilesetOp Tileset.ScreenLayoutFlip
    | Keys.Minus        -> TilesetOp Tileset.BringFocusToMain
    | Keys.Equal        -> TilesetOp Tileset.FocusMain
    | Keys.Plus         -> Resize
    | Keys.Ctrl_z       -> TilesetOp (Tileset.FileviewOp Fileview.recenter_view)
    | Keys.Space        -> TilesetOp (Tileset.FileviewOp Fileview.recenter_view)
    | Keys.Click pos    -> TilesetOp (Tileset.Selection pos)

    | Keys.Lower_w      -> MoveModeOp Movement.Words
    | Keys.Upper_w      -> MoveModeOp Movement.Blocks
    | Keys.Lower_v      -> MoveModeOp Movement.Lines
    | Keys.Upper_b      -> MoveModeOp Movement.Lines
    | Keys.Lower_c      -> MoveModeOp Movement.Chars
    | Keys.Lower_s      -> MoveModeOp Movement.Selection
    | Keys.Lower_d      -> MoveModeOp Movement.Digits
    | Keys.Lower_z      -> MoveModeOp Movement.Paragraphs
    | Keys.Lower_x      -> MoveModeOp Movement.Parens

    | Keys.ArrowUp      -> MoveOp Movement.Up
    | Keys.ArrowDown    -> MoveOp Movement.Down
    | Keys.ArrowLeft    -> MoveOp Movement.Left
    | Keys.ArrowRight   -> MoveOp Movement.Right
    | Keys.Lower_k      -> MoveOp Movement.Up
    | Keys.Lower_j      -> MoveOp Movement.Down
    | Keys.Lower_l      -> MoveOp Movement.Right
    | Keys.Lower_h      -> MoveOp Movement.Left
    | Keys.Upper_h      -> MoveOp Movement.Start
    | Keys.Upper_l      -> MoveOp Movement.End
    | Keys.Upper_j      -> MoveOp Movement.FileEnd
    | Keys.Upper_k      -> MoveOp Movement.FileStart
    | Keys.Ctrl_u       -> MoveOp Movement.PageUp
    | Keys.Ctrl_d       -> MoveOp Movement.PageDown

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

    | Keys.Ctrl_j
    | Keys.Ctrl_k
    | Keys.Upper_g
    | Keys.Upper_m
    | Keys.Upper_z
    | Keys.Lower_b
    | Keys.Unknown _
    | Keys.Escape_Z
    | Keys.Backspace
    | Keys.ClickRelease _
                        -> Noop

    | Keys.EINTR        -> Resize

  let process_command editor =
    match editor.pending_input with
    | None          -> apply_command
    | Number digits -> apply_command_with_repetition (max 1 (dequeue_digits digits))

  (* TODO: replace by a proper history of previous inputs *)
  let update_normal_mode_command key editor =
    let user_input = (pending_command_to_string editor.pending_input)
                   ^  Keys.descr_of key
                   ^ " "
                   ^ editor.user_input
    in
    let user_input' =
      if slen user_input > editor.term_dim.x
        then String.sub user_input 0 editor.term_dim.x
        else user_input
    in {
      editor with
      user_input = user_input' ;
    }

  let update_userinput key editor =
    match editor.mode with
      | Normal    -> update_normal_mode_command key editor
      | RawInput  -> editor

  let rawinput_append c editor = {
      editor with
        user_input = editor.user_input ^ (Char.escaped c) ;
    }

  let rawinput_delete editor =
    let newlen = max 0 ((slen editor.user_input) - 1)
    in {
      editor with
        user_input = String.sub editor.user_input 0 newlen
    }

  let rawinput_update =
    function
      | Keys.Ctrl_c           ->  stop_editor
      | Keys.EINTR            ->  resize_editor
      | Keys.Escape_Z         ->  change_mode Normal
      (* | Keys.Return           ->  finish_input_sequence TODO: signal end of rawinput *)
      | Keys.Tab              ->  rawinput_append '\t'
      | Keys.Backspace        ->  rawinput_delete
      | Keys.Click _
      | Keys.ClickRelease _   ->  id
      | Keys.Unknown c        ->  rawinput_append c
      | k                     ->  k |> Keys.code_of
                                    |> Char.chr
                                    |> rawinput_append

  (* TODO: introduce state_machine for decoupling editor struct from input processing *)
  let process_key key editor =
    match editor.mode with
      | Normal ->
          let fn =
            key |> key_to_command
                |> process_command editor
          in
          editor |> fn |> update_userinput key
      | RawInput ->
          rawinput_update key editor

  let update_stats key input_duration editor =
    (* CLEANUP remove 'key' and instead get last action from editor *)
    let now = Sys.time () in
    let stats' = Stats.update_stats key now input_duration editor.stats in
    if editor.log_stats then
      Stats.log_stats logs stats' ;
    {
      editor with
        stats = stats'
    }

  let process_events (next_key_fn : unit -> Keys.key) (editor : editor) : editor =
    let t1 = Sys.time () in
    let key = next_key_fn () in
    let t2 = Sys.time () in
      editor
        |> process_key key
        |> update_stats key (t2 -. t1)

  let rec loop (next_key_fn : unit -> Keys.key) (editor : editor) : unit =
    if editor.running then
      editor
        |> refresh_screen
        |> process_events next_key_fn
        |> loop next_key_fn

  let run_editor_loop editor () =
    loop Keys.next_key editor

  let main () =
    try
      let file =
        if alen Sys.argv > 1
          then Sys.argv.(1)
          else __FILE__
      in file |> init_editor
              |> run_editor_loop
              |> Term.do_with_raw_mode
    with
      e ->  Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
            Printexc.print_backtrace stdout

end


module Fuzzer = struct

  let fuzz_keys =
    Keys.defined_keys
      |> List.map (fun (k, _, _) -> k)
      |> List.filter ((<>) Keys.Ctrl_c)
      |> List.filter ((<>) Keys.EINTR)
      |> Array.of_list

  let next_key_fuzzer r n =
    let l = alen fuzz_keys in
    let i = ref 0 in
    let rec loop () =
      (*
      Unix.sleepf 0.01 ;
      *)
      incr i ;
      if !i > n
        then Keys.Ctrl_c
        else
          Random.State.int r l |> array_get fuzz_keys
    in loop

  let run_editor_loop n editor () =
    let r = Random.State.make [| 0 ; 1 ; 2 |] in
    Ciseau.loop (next_key_fuzzer r n) editor

  let main n =
    try
      let file =
        if alen Sys.argv > 1
          then Sys.argv.(1)
          else __FILE__
      in file |> Ciseau.init_editor
              |> run_editor_loop n
              |> Term.do_with_raw_mode
    with
      e ->  Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
            Printexc.print_backtrace stdout

end


let log_sigwinch sig_n =
  (* Nothing to do: when SIGWINCH is handled, the read pending on keyboard input is interrupted.
   * The EINTR interrupt codepath there will trigger the resizing *)
  ()

let sigwinch = 28 (* That's for OSX *)


let () =
  Sys.Signal_handle log_sigwinch |> Sys.set_signal sigwinch ;
  (*
  Fuzzer.main 5000 ;
   *)
  Ciseau.main () ;
  close_out logs


(* next TODOs:
 *
 * rendering:
 *  - better management of screen dragging for horizontal scrolling
 *
 *  movements:
 *  - implement easymotion
 *  - once I have a proper tokenizer for the text:
 *    - Number tokenizer which recognizes 0xdeadbeef and 6.667e-11
 *    - string, comments, movement
 *  - Proper tree navigation
 *  - bind brackets and braces delim movement
 *
 *  highlightning:
 *  - static syntax coloring based on tokens
 *
 *  hud: put movement/command history per Fileview and show in user input bar
 *
 *  perfs:
 *  - memory optimization for
 *      TokenMovement
 *      DelimMovement
 *  - add put_color_segment to avoid many rect creation
 *
 *  next features:
 *  - finish file navigation
 *  - find
 *    - free input search
 *    - add vim's incsearch feature
 *    - from current token
 *)


(* dead code or unused code below *)


module FilebufferSet : sig
  type t

  val buffers_menu  : t -> Filebuffer.t (* TODO: this should return a Menu object that wraps a filebuffer *)
  val list_buffers  : t -> Filebuffer.t array
  val open_buffers  : string -> t -> (t * Filebuffer.t)
  val get_buffer    : string -> t -> Filebuffer.t option
  val close_buffers : string -> t -> t

end = struct

  type t = {
    buffers : Filebuffer.t array
  }

  let buffers_menu t =
    t.buffers |> Array.map Filebuffer.filename
              |> Filebuffer.from_lines "opened buffers"

  let list_buffers { buffers } =
    buffers

  let open_buffers filepath { buffers } =
    (* check if that buffer is not opened yet ! *)
    let fb = Filebuffer.init_filebuffer filepath in
    ({ buffers = array_add buffers fb }, fb)

  let get_buffer filepath { buffers } =
    let e = alen buffers in
    let rec loop i =
      if i < e
        then
          let fb = array_get buffers i in
          if filepath = Filebuffer.filename fb
            then Some fb
            else loop (i + 1)
        else None
    in loop 0

  let close_buffers filepath t = (* IMPLEMENT *) t
end
