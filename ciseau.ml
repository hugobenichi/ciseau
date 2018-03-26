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
    a.(j) <- t ;
end


open ArrayOperations


module ArrayBuffer = struct
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
    status        : color_cell ;
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
      status = {
        fg    = darkgray ;
        bg    = White ;
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

  type key_symbol = Unknown
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

                  (* Other events returned by next char *)
                  | EINTR (* usually happen when terminal is resized *)

  type key = {
    symbol  : key_symbol ;
    repr    : string ;
    code    : int ;
  }

  let mk_key s r c = {
    symbol  = s ;
    repr    = r ;
    code    = c ;
  }

  let mk_unknown_key c =
    mk_key Unknown ("unknown(" ^ string_of_int c ^ ")") c

  let code_to_key_table = Array.init (256 + 32) mk_unknown_key

  let defined_keys = [
    mk_key Ctrl_c      "Ctrl_c"        3 ;
    mk_key Ctrl_d      "Ctrl_d"        4 ;
    mk_key Ctrl_j      "Ctrl_j"        10 ;
    mk_key Ctrl_k      "Ctrl_k"        11 ;
    mk_key Ctrl_u      "Ctrl_u"        21 ;
    mk_key Ctrl_z      "Ctrl_z"        26 ;
    mk_key Space       "Space"         32 ;
    mk_key SingleQuote "'"             39 ;
    mk_key Colon       ":"             58 ;
    mk_key SemiColon   ";"             59 ;
    mk_key Equal       "Equal"         61 ;
    mk_key ArrowUp     "ArrowUp"       65 ;
    mk_key ArrowDown   "ArrowDown"     66 ;
    mk_key ArrowRight  "ArrowRight"    67 ;
    mk_key ArrowLeft   "ArrowLeft"     68 ;
    mk_key Upper_b     "W"             66 ;
    mk_key Upper_w     "B"             87 ;
    mk_key Upper_g     "G"             71 ;
    mk_key Upper_h     "H"             72 ;
    mk_key Upper_j     "J"             74 ;
    mk_key Upper_k     "K"             75 ;
    mk_key Upper_l     "L"             76 ;
    mk_key Lower_b     "w"             98 ;
    mk_key Lower_c     "c"             99 ;
    mk_key Lower_s     "s"             115 ;
    mk_key Lower_d     "d"             100 ;
    mk_key Lower_z     "z"             122 ;
    mk_key Lower_x     "x"             120 ;
    mk_key Lower_h     "h"             104 ;
    mk_key Lower_j     "j"             106 ;
    mk_key Lower_k     "k"             107 ;
    mk_key Lower_l     "l"             108 ;
    mk_key Lower_w     "w"             119 ;
    mk_key Digit_0     "0"             48 ;
    mk_key Digit_1     "1"             49 ;
    mk_key Digit_2     "2"             50 ;
    mk_key Digit_3     "3"             51 ;
    mk_key Digit_4     "4"             52 ;
    mk_key Digit_5     "5"             53 ;
    mk_key Digit_6     "6"             54 ;
    mk_key Digit_7     "7"             55 ;
    mk_key Digit_8     "8"             56 ;
    mk_key Digit_9     "9"             57 ;
    mk_key Backslash   "\\"            92 ;
    mk_key ParenLeft   "("             40 ;
    mk_key ParenRight  ")"             41 ;
    mk_key Plus        "+"             43 ;
    mk_key Minus       "-"             45 ;
    mk_key BracketLeft "["             91 ;
    mk_key BracketRight "]"            93 ;
    mk_key Underscore  "_"             95 ;
    mk_key Pipe        "|"             124 ;
    mk_key BraceLeft   "{"             123 ;
    mk_key BraceRight  "}"             125 ;
    mk_key EINTR       "EINTR"         256 ;
  ]

  let _ = defined_keys |> List.iter (fun k -> code_to_key_table.(k.code) <- k)

  let code_to_key = Array.get code_to_key_table

  (* replacement for input_char which considers 0 as Enf_of_file *)
  let next_key =
    (* WARN not thread safe *)
    let buffer = Bytes.make 1 'z' in
    let rec one_byte_reader () =
      match Unix.read Unix.stdin buffer 0 1 with
      | 1   -> Bytes.get buffer 0 |> Char.code |> code_to_key
      | 0   -> one_byte_reader ()     (* timeout *)
      | _   -> fail "next_char failed"
      | exception Unix.Unix_error (errcode,  fn_name, fn_param) ->
          Printf.fprintf logs "Unix_error errmsg='%s' fn='%s'\n" (Unix.error_message errcode) fn_name ;
          match errcode with
          | Unix.EINTR ->
            (* read interrupted, usually caused SIGWINCH signal handler for terminal resize: retry read *)
            code_to_key 256
          | _ -> raise (Unix.Unix_error (errcode, fn_name, fn_param))
    in one_byte_reader

end


type linebreak = Clip | Overflow (* TODO: consider putting the linebreaking offset here ? *)


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


module Bytevector : sig
  type t

  val init_bytevector : int -> t
  val reset : t -> unit
  val append : t -> string -> unit
  val append_bytes : t -> Bytes.t -> int -> int -> unit
  val write : Unix.file_descr -> t -> unit

end = struct

  type t = {
    mutable bytes   : bytes ;
    mutable cursor  : int ;
  }

  let init_bytevector len = {
    bytes   = Bytes.make len '\000' ;
    cursor  = 0 ;
  }

  let reset bvec =
    bvec.cursor <- 0

  let append bvec s =
    let len = slen s in
    bytes_blit_string s 0 bvec.bytes bvec.cursor len ;
    bvec.cursor <- bvec.cursor + len

  let append_bytes bvec src offset len =
    bytes_blit src offset bvec.bytes bvec.cursor len ;
    bvec.cursor <- bvec.cursor + len

  let write fd bvec =
    write fd bvec.bytes bvec.cursor
end


(* main module for interacting with the terminal *)
module Term = struct

  module Control = struct
    let escape                = 27 |> Char.chr |> string_of_char ;;
    let start                 = escape ^ "[" ;;
    let finish                = escape ^ "[0m" ;;
    let clear                 = escape ^ "c" ;;
    let newline               = "\r\n"  ;;
    let cursor_hide           = start ^ "?25l" ;;
    let cursor_show           = start ^ "?25h" ;;
    let cursor_save           = start ^ "s" ;;
    let cursor_restore        = start ^ "u" ;;
    let switch_offscreen      = start ^ "?47h" ;;
    let switch_mainscreen     = start ^ "?47l" ;;
    let gohome                = start ^ "H" ;;

    let cursor_offset = mk_v2 1 1

    (* ANSI escape codes weirdness: cursor positions are 1 based in the terminal referential *)
    let cursor_control_string vec2 =
      Printf.sprintf "%s%d;%dH" start (vec2.y + 1) (vec2.x + 1)

  end

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

      stdout_write_string Control.cursor_save ;
      stdout_write_string Control.switch_offscreen ;
      tcsetattr stdin TCSAFLUSH want ;

      let cleanup () =
        tcsetattr stdin TCSAFLUSH initial ;
        stdout_write_string Control.switch_mainscreen ;
        stdout_write_string Control.cursor_restore
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
  val render            : t -> Bytevector.t -> unit
  val put_color_rect    : t -> Color.color_cell -> rect -> unit
  (* TODO: add a put_color_segment function *)
  val put_cursor        : t -> v2 -> unit
  val put_line          : t -> x:int -> y:int -> ?offset:int -> ?len:int -> string -> unit
  val put_framebuffer   : t -> rect -> int -> t -> linebreak -> v2

end = struct

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

    let render_section start stop t bvec =
      (* append lines one at a time starting from start offset, ending at stop offset *)
      let append_newline_if_needed bvec t position =
        let is_end_of_line        = (position mod t.window.x) = 0 in
        let is_not_end_of_buffer  = position < t.len in (* Do not append newline at the very end *)
        if is_end_of_line && is_not_end_of_buffer
          then Bytevector.append bvec Term.Control.newline
      in
      let rec loop t start stop colorbuffer_idx bvec =
        (* memory opt: all arguments passed explicitly *)
        if start < stop
          then
            let len = next_line_len t start stop in
            Bytevector.append bvec Term.Control.start ;
            colorbuffer_idx
              |> array_get t.fg_colors
              |> array_get Color.fg_color_control_strings
              |> Bytevector.append bvec ;
            colorbuffer_idx
              |> array_get t.bg_colors
              |> array_get Color.bg_color_control_strings
              |> Bytevector.append bvec ;
            Bytevector.append_bytes bvec t.text start len ;
            Bytevector.append bvec Term.Control.finish ;
            (* Last newline need to be appened *after* the terminating control command for colors *)
            append_newline_if_needed bvec t (start + len) ;
            loop t (start + len) stop colorbuffer_idx bvec
      in
        loop t start stop start bvec

    let render_all_sections t bvec =
      let rec loop start bvec =
        if start < t.len
          then
            let stop = next_contiguous_color_section t start in
            render_section start stop t bvec ;
            loop stop bvec
      in
        loop 0 bvec
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

  (* TODO: put in a global together with fb and recreate at every term resize event *)
  let default_fg_colors   = Array.make (256 * 512) Color.white_code
  let default_bg_colors   = Array.make (256 * 512) Color.darkgray_code
  let default_line_length = Array.make 256 0

  let fill_fg_color t offset len color =
    color
      |> Color.color_control_code
      |> array_fill t.fg_colors offset len

  let fill_bg_color t offset len color =
    color
      |> Color.color_control_code
      |> array_fill t.bg_colors offset len

  (* TODO: if ever the size of the default arrays are not sufficient, add loop to do multiple fills *)
  let clear t =
    Bytes.fill t.text 0 t.len Default.text ;
    array_blit default_fg_colors 0 t.fg_colors 0 t.len ;
    array_blit default_bg_colors 0 t.bg_colors 0 t.len ;
    (* TODO: clear z_index if I ever start using it *)
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

  let render frame_buffer render_buffer =
    Bytevector.reset render_buffer ;
    Bytevector.append render_buffer Term.Control.cursor_hide ;
    Bytevector.append render_buffer Term.Control.gohome ;
    Rendering.render_all_sections frame_buffer render_buffer ;
    Bytevector.append render_buffer (Term.Control.cursor_control_string frame_buffer.cursor) ;
    Bytevector.append render_buffer Term.Control.cursor_show ;
    if kDRAW_SCREEN
      then Bytevector.write Unix.stdout render_buffer

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
    assert (x + blitlen <= framebuffer.window.x) ;
    update_line_end framebuffer (x + blitlen) y ;
    bytes_blit_string s offset framebuffer.text bytes_offset blitlen

  (* Blit content of framebuffer 'src' into a rectangle 'src_rect' of framebuffer 'dst'.
   * If 'linebreaking' is Overflow, lines of 'src' which do not fit in 'dst_rect' are
   * wrapped to the next line in 'dst'.
   * Copy cursor in 'dst' if 'copy_cursor' is true. *)
  let put_framebuffer dst dst_rect linebreaking_offset src linebreaking =
    assert_rect_inside dst.window dst_rect ;
    assert_that (src.window.y >= rect_h dst_rect) ;

    let w_dst = rect_w dst_rect in
    let x_dst = ref (rect_x dst_rect) in
    let y_dst = ref (rect_y dst_rect) in

    let x_src = ref 0 in
    let y_src = ref 0 in
    let x_src_stop = ref 0 in

    let cursor_out = ref v2_zero in

    (* BUG: because of line are laid down from top to bottom, in Overflow mode the cursor can fallover below the
     * visible portaion of the screen !
     * To fix this properly I would need to either:
     *    1) start puting lines from the cursors, both upward and downward
     *    2) repeat put_framebuffer adding y offset until the cursor would not fall off the screen !
     *    3) make view_adjust, recenter, y scrolling, ... works with the final on frame position instead of the
     *       text position.
     *)
    while !y_dst < (rect_y_end dst_rect) do
      let o_dst = to_offset dst.window !x_dst !y_dst in
      let o_src = to_offset src.window !x_src !y_src in

      if 0 = !x_src then
        x_src_stop := array_get src.line_lengths !y_src ;

      let len = w_dst in

      bytes_blit src.text o_src dst.text o_dst len ;
      array_blit src.fg_colors o_src dst.fg_colors o_dst len ;
      array_blit src.bg_colors o_src dst.bg_colors o_dst len ;

      (* Works in Overflow mode *)
      if src.cursor.y = !y_src && !x_src <= src.cursor.x && src.cursor.x < !x_src + w_dst then (
        let x_dst_space = !x_dst + src.cursor.x mod w_dst in
        let y_dst_space = !y_dst in
        cursor_out := mk_v2 x_dst_space y_dst_space ;
        assert_v2_inside dst.window !cursor_out
      ) ;

      incr y_dst;
      match linebreaking with
        | Clip -> (
          incr y_src
        )
        | Overflow -> (
          x_src += w_dst ;
          x_dst := (rect_x dst_rect) + linebreaking_offset ;
          if !x_src_stop < !x_src then (
            x_src := 0 ;
            x_dst := (rect_x dst_rect) ;
            incr y_src
          )
        )
    done ;

    !cursor_out

end


(* A common buffer for filling Fileview content just before bliting into a backend rendering framebuffer *)
(* TODO: Not threadsafe !! find better place to put that *)
let fb = ref (Framebuffer.init_framebuffer v2_zero)


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
  val put_framebuffer   : t -> Framebuffer.t -> linebreak -> v2

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
    let x_start   = min (rect_x rect) 0 in
    let y_start   = min (rect_y rect) 0 in
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

  let put_framebuffer screen src linebreaking =
    let cursor =
      Framebuffer.put_framebuffer screen.frame_buffer screen.window 6 src linebreaking
    in
      v2_sub cursor (rect_offset screen.window)
end


module Filebuffer = struct
  type t = {
    filename    : string ;
    filepath    : string ;
    header      : string ;
    buffer      : string array ;          (* the file data, line per line *)
    buflen      : int ;                   (* number of lines in buffer, may be less than buffer array length *)
  }

  let read_file f =
    let rec loop lines ch =
      match input_line ch with
      | s                     -> loop (ArrayBuffer.append lines s) ch
      | exception End_of_file -> lines
    in
    let ch = open_in f in
    try
      let r = loop (ArrayBuffer.reserve 32 "") ch
                |> ArrayBuffer.to_array
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
  let last_line_index { buflen } = buflen - 1

  let is_line_empty filebuffer y = (line_length filebuffer y = 0)

  (* This function should generate bugs: callers cannot distinguish strings with length 1 and the empty string *)
  let last_cursor_x filebuffer y = max 0 ((line_length filebuffer y) - 1)

  (* This function should return an option for empty lines *)
  let last_cursor_x2 filebuffer y = (line_length filebuffer y) - 1

  let is_valid_cursor filebuffer x y = (0 <= y)
                                    && (y <= last_line_index filebuffer)
                                    && line_length filebuffer y > 0
                                    && (0 <= x)
                                    && (x <= last_cursor_x filebuffer y)

  let char_at filebuffer x y =
    string_at (line_at filebuffer y) x

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


module TokenMovement (T : TokenFinder) : sig
  (* Fix these two signatures for plugin into other Movement modules using go_token_first
   * and go_token_last *)
  val go_token_first  : Filebuffer.t -> int -> v2 option
  val go_token_last   : Filebuffer.t -> int -> v2 option
  val go_token_left   : Filebuffer.t -> v2 -> v2
  val go_token_right  : Filebuffer.t -> v2 -> v2
  val go_token_up     : Filebuffer.t -> v2 -> v2
  val go_token_down   : Filebuffer.t -> v2 -> v2
  val go_token_start  : Filebuffer.t -> v2 -> v2
  val go_token_end    : Filebuffer.t -> v2 -> v2
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

  let go_token_start filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> find_token cursor.x
      |> map (token_start_to_cursor cursor.y)
      |> get_or cursor

  let go_token_end filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> find_token cursor.x
      |> map (token_stop_to_cursor cursor.y)
      |> get_or cursor

  let rec go_token_first filebuffer y =
    if y > Filebuffer.last_line_index filebuffer
      then None
      else
        T.next (Filebuffer.line_at filebuffer y) None
          |> map (token_start_to_cursor y)
          |> function
              | None   -> go_token_first filebuffer (y + 1)
              | cursor -> cursor

  let go_token_right filebuffer cursor =
    let rec loop line x =
      function
        | None                        -> None
        | Some tok when x < tok.start -> Some tok
        | tok_opt                     -> tok_opt
                                          |> T.next line
                                          |> loop line x
    in
      let line = Filebuffer.line_at filebuffer cursor.y in
      loop line cursor.x (T.next line None)
        |> function
            | Some tok  -> mk_v2 tok.start cursor.y
            | None      -> go_token_first filebuffer (cursor.y + 1)
        |> get_or cursor

  let rec go_token_last filebuffer y =
    let rec loop line tok_opt =
      match (tok_opt, T.next line tok_opt) with
        | (None,      None)         -> None
        | (Some last, None)         -> Some (mk_v2 last.start y)
        | (_,         next_tok_opt) -> loop line next_tok_opt
    in
      if y < 0
        then None
        else loop (Filebuffer.line_at filebuffer y) None
              |> function
                  | None -> go_token_last filebuffer (y - 1)
                  | last -> last

  let go_token_left filebuffer cursor =
    let rec loop line x tok_opt =
      T.next line tok_opt
        |> function
            | None                                -> None
            | Some tok when token_contains x tok  -> tok_opt
            | next_tok_opt                        -> loop line x next_tok_opt
    in
      if cursor.y < 0
        then cursor
        else loop (Filebuffer.line_at filebuffer cursor.y) cursor.x None
          |> (function
              | None      -> go_token_last filebuffer (cursor.y - 1)
              | Some tok  -> Some (mk_v2 tok.start cursor.y))
          |> get_or cursor

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

  let go_token_down filebuffer cursor =
    let rec loop filebuffer y n =
      if y > Filebuffer.last_line_index filebuffer
        then None
        else
          go_token_nth n(Filebuffer.line_at filebuffer y)
            |> map (token_start_to_cursor y)
            |> function
                | None  -> loop filebuffer (y + 1) n
                | token -> token
    in
      find_token_nth cursor.x (Filebuffer.line_at filebuffer cursor.y)
        |> fmap (loop filebuffer (cursor.y + 1))
        |> get_or cursor

  let go_token_up filebuffer cursor =
    let rec loop filebuffer y n =
      if y < 0
        then None
        else
          go_token_nth n (Filebuffer.line_at filebuffer y)
            |> map (token_start_to_cursor y)
            |> function
                | None  -> loop filebuffer (y - 1) n
                | token -> token
    in
      find_token_nth cursor.x (Filebuffer.line_at filebuffer cursor.y)
        |> fmap (loop filebuffer (cursor.y - 1))
        |> get_or cursor

  let movement =
    let open Move in
    function
      | Left    -> go_token_left
      | Right   -> go_token_right
      | Up      -> go_token_up
      | Down    -> go_token_down
      | Start   -> go_token_start
      | End     -> go_token_end
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
    if is_alphanum c || c == '_'
      then Include
      else Exclude
end)
module WordMovement = TokenMovement(WordFinder)

module LineMovement = struct

  let go_line_start filebuffer cursor =
    mk_v2 0 cursor.y

  let go_line_end filebuffer cursor =
    mk_v2 (Filebuffer.last_cursor_x filebuffer cursor.y) cursor.y

  let go_line_up filebuffer cursor =
    if cursor.y > 0
      then
        let y' = cursor.y - 1 in
        let x' = min cursor.x (Filebuffer.last_cursor_x filebuffer y') in
        mk_v2 x' y'
      else cursor

  let go_line_down filebuffer cursor =
    if cursor.y + 1 < Filebuffer.file_length filebuffer
      then
        let y' = cursor.y + 1 in
        let x' = min cursor.x (Filebuffer.last_cursor_x filebuffer y') in
        mk_v2 x' y'
      else cursor

  let go_line_left filebuffer cursor =
    let cursor' = go_line_up filebuffer cursor in
    if cursor.y = cursor'.y || Filebuffer.is_line_empty filebuffer cursor'.y
      then cursor'
      else BlockMovement.go_token_first filebuffer cursor'.y
            |> OptionCombinators.get_or cursor'

  let go_line_right filebuffer cursor =
    let x' = Filebuffer.last_cursor_x filebuffer cursor.y in
    if cursor.x >= x'
      then go_line_down filebuffer cursor |> go_line_end filebuffer
      else mk_v2 x' cursor.y

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

  let go_char_left filebuffer cursor =
    if cursor.x = 0
      then
        let cursor' = LineMovement.go_line_up filebuffer cursor in
        (if cursor' = cursor
          then cursor
          else LineMovement.go_line_end filebuffer cursor')
      else mk_v2 (cursor.x - 1) cursor.y

  let go_char_right filebuffer cursor =
    let x' = Filebuffer.last_cursor_x filebuffer cursor.y in
    if cursor.x >= x'
      then
        let cursor' = LineMovement.go_line_down filebuffer cursor in
        (if cursor = cursor'
          then cursor
          else LineMovement.go_line_start filebuffer cursor')
      else mk_v2 (cursor.x + 1) cursor.y

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

  (* Go up until first line reached, or empty line above *)
  let rec find_para_start filebuffer y =
    if y = 0 || Filebuffer.is_line_empty filebuffer (y - 1)
      then y
      else find_para_start filebuffer (y - 1)

  (* Go down until last line reached, or empty line below *)
  let rec find_para_end filebuffer y =
    let y' = y + 1 in
    if y' = Filebuffer.file_length filebuffer || Filebuffer.is_line_empty filebuffer y'
      then y
      else find_para_end filebuffer y'

  (* Go up until first line reached, or non-empty line above *)
  let rec find_non_empty_above filebuffer y =
    if y = 0 || Filebuffer.is_line_empty filebuffer (y - 1) |> not
      then y
      else find_non_empty_above filebuffer (y - 1)

  (* Go down until last line reached, or empty line below *)
  let rec find_non_empty_below filebuffer y =
    let y' = y + 1 in
    if y' = Filebuffer.file_length filebuffer || Filebuffer.is_line_empty filebuffer y' |> not
      then y
      else find_non_empty_below filebuffer y'

  (* Go to first word of the current paragraph, or nowhere if cursor not inside paragraph *)
  let go_para_start filebuffer cursor =
    if Filebuffer.is_line_empty filebuffer cursor.y
      then cursor
      else
        find_para_start filebuffer cursor.y
          |> BlockMovement.go_token_first filebuffer
          |> function
              | None          -> cursor
              | Some cursor'  -> cursor'

  (* Go to last char of last line of paragraph, or nowhere if cursor not inside paragraph *)
  let go_para_end filebuffer cursor =
    if Filebuffer.is_line_empty filebuffer cursor.y
      then cursor
      else
        let y' = find_para_end filebuffer cursor.y in
        mk_v2 (Filebuffer.last_cursor_x filebuffer y') y'

  let go_para_up filebuffer cursor =
    let y' = find_para_start filebuffer cursor.y in
    if y' = 0
      then cursor
      else
        let y'' = find_non_empty_above filebuffer y' in
        if y'' = 0
          then cursor
          else go_para_start filebuffer (mk_v2 0 (y'' - 1))

  let go_para_down filebuffer cursor =
    let y' = find_para_end filebuffer cursor.y in
    if y' = 0
      then cursor
      else
        let y'' = 1 + find_non_empty_below filebuffer y' in
        if y'' = Filebuffer.file_length filebuffer
          then cursor
          else go_para_start filebuffer (mk_v2 0 y'')

  let go_para_left filebuffer cursor =
    let cursor' = go_para_up filebuffer cursor in
    if cursor = cursor'
      then cursor'
      else go_para_end filebuffer cursor'

  let go_para_right filebuffer cursor =
    let cursor' = go_para_down filebuffer cursor in
    if cursor = cursor'
      then cursor'
      else go_para_end filebuffer cursor'

  let movement : Move.t -> Filebuffer.t -> v2 -> v2 =
    let open Move in
    function
      | Start   -> go_para_start
      | End     -> go_para_end
      | Left    -> go_para_left
      | Right   -> go_para_right
      | Up      -> go_para_up
      | Down    -> go_para_down
end


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
  let get_kind_at filebuffer x y =
    if Filebuffer.is_valid_cursor filebuffer x y
      then Filebuffer.char_at filebuffer x y |> K.get_kind
      else 0

  (* Go to first 'left' delimiter on the left of current position *)
  let rec go_first_left filebuffer x y skip =
    if x < 0
      then
        (if y = 0
          then None
          else
            let y' = y - 1 in
            let x' = Filebuffer.last_cursor_x filebuffer y' in
            go_first_left filebuffer x' y' false)
      else
        if not skip && get_kind_at filebuffer x y |> is_left
          then Some (mk_v2 x y)
          else go_first_left filebuffer (x - 1) y false

  (* Go to first 'left' delimiter on the right of current position *)
  let rec go_first_right filebuffer x y skip =
    if x >= Filebuffer.line_length filebuffer y
      then
        (if y = Filebuffer.last_line_index filebuffer
          then None
          else go_first_right filebuffer 0 (y + 1) false)
      else
        if not skip && get_kind_at filebuffer x y |> is_left
          then Some (mk_v2 x y)
          else go_first_right filebuffer (x + 1) y false

  (* Go to first 'right' delimiter on the left of current position *)
  let rec go_first_end_left filebuffer x y skip =
    if x < 0
      then
        (if y = 0
          then None
          else
            let y' = y - 1 in
            let x' = Filebuffer.last_cursor_x filebuffer y' in
            go_first_end_left filebuffer x' y' false)
      else
        if not skip && get_kind_at filebuffer x y |> is_right
          then Some (mk_v2 x y)
          else go_first_end_left filebuffer (x - 1) y false

  (* Move cursor left until balance is 0 *)
  let rec go_left filebuffer x y b =
    if x < 0
      then
        (if y = 0
          then None
          else go_left filebuffer (Filebuffer.last_cursor_x filebuffer (y - 1)) (y - 1) b)
      else
        let b' = b + (get_kind_at filebuffer x y) in
        if b' = 0
          then Some (mk_v2 x y)
          else go_left filebuffer (x - 1) y b'

  (* Move cursor right until balance is 0 *)
  let rec go_right filebuffer x y b =
    if x >= Filebuffer.line_length filebuffer y
      then
        (if y = Filebuffer.last_line_index filebuffer
          then None
          else go_right filebuffer 0 (y + 1) b)
      else
        let b' = b + (get_kind_at filebuffer x y) in
        if b' = 0
          then Some (mk_v2 x y)
          else go_right filebuffer (x + 1) y b'

  let go_delim_left filebuffer cursor =
    go_first_left filebuffer cursor.x cursor.y true |> get_or cursor

  let go_delim_end_left filebuffer cursor =
    go_first_end_left filebuffer cursor.x cursor.y true |> get_or cursor

  let go_delim_right filebuffer cursor =
    go_first_right filebuffer cursor.x cursor.y true |> get_or cursor

  let go_delim_start filebuffer cursor =
    let k = get_kind_at filebuffer cursor.x cursor.y in
    if is_left k
      then cursor
      else go_left filebuffer cursor.x cursor.y (0 - k - 1)
            |> get_or cursor

  let go_delim_end filebuffer cursor =
    let k = get_kind_at filebuffer cursor.x cursor.y in
    if is_right k
      then cursor
      else go_right filebuffer cursor.x cursor.y (1 - k)
            |> get_or cursor

  let go_delim_up filebuffer cursor =
    cursor
      |> go_delim_start filebuffer
      |> go_delim_end_left filebuffer
      |> go_delim_start filebuffer

  let go_delim_down filebuffer cursor =
    cursor
      |> go_delim_start filebuffer
      |> go_delim_end filebuffer
      |> go_delim_right filebuffer

  let movement : Move.t -> Filebuffer.t -> v2 -> v2 =
    let open Move in
    function
      | Left    -> go_delim_left
      | Right   -> go_delim_right
      | Up      -> go_delim_up
      | Down    -> go_delim_down
      | Start   -> go_delim_start
      | End     -> go_delim_end
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

  let move_file_start filebuffer any = mk_v2 0 0
  let move_file_end   filebuffer any = mk_v2 0 (Filebuffer.last_line_index filebuffer)

  let move_page_up filebuffer { x ; y } =
    let y' = max (y - page_offset) 0
    in {
      x = min x (Filebuffer.last_cursor_x filebuffer y') ;
      y = y' ;
    }

  let move_page_down filebuffer { x ; y } =
    let y' = min (y + page_offset) (Filebuffer.last_line_index filebuffer)
    in {
      x = min x (Filebuffer.last_cursor_x filebuffer y') ;
      y = y' ;
    }

  let noop_move mov filebuffer cursor =
    cursor

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
      | PageUp        -> move_page_up
      | PageDown      -> move_page_down
      | FileStart     -> move_file_start
      | FileEnd       -> move_file_end
end


type redraw_level = Nodraw | FrameDirty | Redraw


module Fileview : sig
  type t

  val init_fileview           : Filebuffer.t -> t
  val set_mov_mode            : Movement.mode -> t -> t
  val apply_movement          : Movement.movement -> int -> t -> t
  val cursor                  : t -> v2
  val adjust_view             : int -> t -> t
  val last_line_index         : t -> int
  val swap_line_number_mode   : t -> t
  val swap_linebreaking_mode  : t -> t
  val toggle_show_token       : t -> t
  val toggle_show_neighbor    : t -> t
  val toggle_show_selection   : t -> t
  val recenter_view           : int -> t -> t
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
    view_start        : int ;      (* index of first row in view *)
    numbering         : numbering_mode ;
    linebreaking      : linebreak ;
    mov_mode          : Movement.mode ;
    show_token        : bool ;
    show_neighbor     : bool ;
    show_selection    : bool ;
    context           : MovementContext.t ;
  }

  let init_fileview filebuffer = {
    filebuffer        = filebuffer ;
    cursor            = v2_zero ;
    view_start        = 0 ;
    numbering         = CursorRelative ;
    linebreaking      = Clip ;
    mov_mode          = Movement.Chars ;
    show_token        = true ;
    show_neighbor     = true ;
    show_selection    = true ;
    context           = let open MovementContext in {
      selection = Filebuffer.search filebuffer "cursor" ;
    } ;
  }

  let set_mov_mode m t = {
    t with
      mov_mode  = m ;
  }

  let cursor { cursor } =
    cursor

  let view_height { view_start ; filebuffer } =
    (Filebuffer.file_length filebuffer) - view_start

  let last_line_index { filebuffer } =
    Filebuffer.last_line_index filebuffer

  let adjust_view screen_height t =
    let text_height = screen_height - 2 in (* -1 for header line, -1 for indexing starting at 0 *)
    let view_start =
      if t.cursor.y < t.view_start then
        t.cursor.y
      else if t.cursor.y > t.view_start + text_height then
        t.cursor.y - text_height
      else
        t.view_start
    in
      { t with
          view_start  = view_start ;
      }

  let apply_movement mov view_height t =
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

    adjust_view view_height { t with cursor = cursor' }

  let swap_line_number_mode t =
    let new_mode = match t.numbering with
    | Absolute        -> CursorRelative
    | CursorRelative  -> Absolute
    in {
      t with
        numbering = new_mode ;
    }

  let swap_linebreaking_mode t =
    let new_mode = match t.linebreaking with
    | Clip      -> Overflow
    | Overflow  -> Clip
    in {
      t with
        linebreaking  = new_mode ;
    }

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

  (* TODO: this should also recenter the view horizontally in Clip mode *)
  let recenter_view view_height t =
    let new_start = t.cursor.y - view_height / 2 in {
      t with
        view_start  = max new_start 0 ;
    }

  (* TODO: move drawing in separate module ? *)

  (* CLEANUP: probably I should draw the line number separately from the main text: this would have a number
   * of benefits in term of codeflow simplicity and not having to put +6 horizontal offsets everywhere and
   * recreate the same objects. *)

  (* PERF: resize dynamically and fit to term size ! *)
  let framebuffer = Framebuffer.init_framebuffer (mk_v2 300 100)

  let frame_default_line      = "~  "
  let frame_continuation_line = "..."

  (* TODO: fileview should remember the last x scrolling offset and make it sticky, like the y scrolling *)
  let fill_framebuffer t screen framebuffer =
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
      if t.linebreaking = Overflow || t.cursor.x < last_x_index
        then (t.cursor.x, 0)
        else (base_scrolling_offset, t.cursor.x - base_scrolling_offset)
    in
    let cursor_x_screenspace = 6 + cursor_x in

    let x_scrolling_offset = scrolling_offset in

    (* Continuation dots for overflow mode *)
    if t.linebreaking = Overflow then
      for y = 0 to text_height do
        Screen.put_line screen ~x:0 ~y:y frame_continuation_line
      done ;

    (* Text area *)
    for i = 0 to text_stop_y - 1 do
      let line_idx = i + t.view_start in
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

    (* BUG: with horizontal scrolling > 0, most of the color blocks below are wrong *)

    (* Show cursor lines *)
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.cursor_line
      (mk_rect 6 (t.cursor.y - t.view_start) text_width (t.cursor.y - t.view_start)) ;
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.cursor_line
      (mk_rect (t.cursor.x + 6) 0 (t.cursor.x + 7) text_height) ;

    (* Show token where cursor currently is *)
    if t.show_token then (
      let token_s = Movement.compute_movement t.context t.mov_mode Movement.Start t.filebuffer t.cursor in
      let token_e = Movement.compute_movement t.context t.mov_mode Movement.End t.filebuffer token_s in
      let y_start = token_s.y - t.view_start in
      let y_end   = token_e.y - t.view_start in
      (* The current token can leak out of the current screen: bound start and stop to the screen *)
      for y = max 0 y_start to min y_end text_stop_y do
        let len = Filebuffer.line_length t.filebuffer (y + t.view_start) in
        let x0 =
          if y = y_start then token_s.x else 0 in       (* start from x=0 for lines after the first *)
        let x1 =
          if y = y_end then (token_e.x + 1) else len in (* ends at end-of-line for lines before the last *)
        let x0' = 6 + (min x0 x1) in
        let x1' = 6 + (max x0 x1) in
        (* TODO: this should use a put_color_line api to avoid making rect records *)
        Framebuffer.put_color_rect
          framebuffer Config.default.colors.current_token (mk_rect x0' y x1' y)
      done ;
    ) ;

    (* BUG: these show_* color rectangles cause weirdness in Overflow mode ! *)
    (* Show selection *)
    if t.show_selection then (
      let translate_selection t rect =
          let tl_x = (rect_x rect) + 6 in
          let tl_y = (rect_y rect) - t.view_start in
          let br_x = (rect_x_end rect) + 7 in
          let br_y = (rect_y_end rect) - t.view_start in
          mk_rect tl_x tl_y br_x br_y
      in

      t.context.selection
        (* PERF: only keep rectangles that might be in the view *)
        |> Array.map (translate_selection t)
        |> Array.iter (Framebuffer.put_color_rect framebuffer Config.default.colors.selection) ;
    ) ;

    (* Show Left/Right/Up/Down tokens relatively to where current token is *)
    if t.show_neighbor then (
      let lightup_pixel t framebuffer colors pos =
          let x = pos.x + 6 in
          let y = pos.y - t.view_start  in
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

    (* Cursor: pass down cursor to framebuffer -> put_framebuffer will compute the screen position and pass it back *)
    let cursor = mk_v2 cursor_x_screenspace (t.cursor.y - t.view_start) in
    Framebuffer.put_cursor framebuffer cursor ;
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.string
      (mk_rect 0 cursor.y 6 cursor.y)


  let put_border_frame t screen textsize is_focused =
    Screen.put_line screen ~x:0 ~y:0
      (Printf.sprintf
        "%s %d,%d  mode=%s"
        t.filebuffer.Filebuffer.header
        t.cursor.y t.cursor.x
        (Movement.mode_to_string t.mov_mode)) ;
    (* header *)
    let size = Screen.screen_size screen in
    Screen.put_color_rect
      screen
      (if is_focused
        then Config.default.colors.focus_header
        else Config.default.colors.header)
      (mk_rect 0 0 size.x 0) ;
    (* border *)
    Screen.put_color_rect
      screen
      Config.default.colors.border
      (mk_rect 0 1 1 (Screen.screen_height screen))

  let draw t screen redraw is_focused =
    (* PERF: many rectangles for colors could be hoisted into the screen record and not allocated *)
    let textscreen = Screen.mk_subscreen screen
      (* 1 for border column, 1 for header space *)
      (mk_rect 1 1 (Screen.screen_width screen) (Screen.screen_height screen))
    in
    let framebuffer = !fb in
    match redraw with
    | Nodraw ->
        ()
    | FrameDirty ->
        put_border_frame t screen (Screen.screen_size textscreen) is_focused
    | Redraw -> (
        put_border_frame t screen (Screen.screen_size textscreen) is_focused ;
        Screen.clear textscreen ;
        Framebuffer.clear framebuffer ;
        fill_framebuffer t textscreen framebuffer ;
        (* CLEANUP: pass the args in a struct and do the drawing of cursor there *)
        let final_cursor = Screen.put_framebuffer textscreen framebuffer t.linebreaking in
        if is_focused then
          Screen.put_cursor textscreen final_cursor
      )

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
    last_key_input      = Keys.mk_unknown_key 0 ;
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
    output_string f stats.last_key_input.repr ;
    output_string f " " ;
    output_int f stats.last_key_input.code ;
    output_string logs "\n"
end


module Tileset = struct

  type op = Resize of rect
          | FileviewOp of (int -> Fileview.t -> Fileview.t)
          | RotateViewsLeft
          | RotateViewsRight
          | ScreenLayoutCycleNext
          | ScreenLayoutCyclePrev
          | ScreenLayoutFlip
          | FocusNext
          | FocusPrev
          | FocusMain
          | BringFocusToMain

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
        then Fileview.adjust_view (array_get screen_tiles i |> rect_h) view
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

  let set_focus t focus_index = {
      t with
        focus_index = focus_index ;
        redrawing   = Array.make (alen t.fileviews) FrameDirty ;
    }

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
          let screen_height = get_focused_tile t |> rect_h in
          t.focus_index
            |> array_get fileviews'
            |> fileview_op screen_height
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
end


module Ciseau = struct

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
    render_buffer   : Bytevector.t ;
    frame_buffer    : Framebuffer.t ;
    running         : bool ;
    status_screen   : Screen.t ;
    tileset         : Tileset.t ;
    filebuffers     : Filebuffer.t array ; (* TODO: turn this into buffer management layer *)
    user_input      : string ;
    pending_input   : pending_command ;
    stats           : Stats.t ;
    log_stats       : bool ;
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
      (* Filebuffer.init_filebuffer "./Makefile" ; (* FIX tabs *) *)
      (* FileNavigator.dir_to_filebuffer (Sys.getcwd ()) ; *)
    |]

  let test_mk_fileviews term_dim =
    Array.map Fileview.init_fileview

  let mk_tileset term_dim filebuffers =
    filebuffers
      |> Array.map Fileview.init_fileview
      |> Tileset.mk_tileset 0 (main_screen_dimensions term_dim) ScreenConfiguration.Configs.zero

  let init_editor file =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_framebuffer term_dim in
    let filebuffers = test_mk_filebuffers file in
    fb := Framebuffer.init_framebuffer term_dim ;
    {
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      render_buffer   = Bytevector.init_bytevector 0x100000 ;
      frame_buffer    = frame_buffer ;
      running         = true ;
      status_screen   = mk_status_screen frame_buffer term_dim ;

      tileset         = mk_tileset term_dim filebuffers ;
      filebuffers     = filebuffers ;

      user_input      = "" ;
      pending_input   = None;
      stats           = Stats.init_stats () ;
      log_stats       = kLOG_STATS ;
    }

  let resize_editor editor =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_framebuffer term_dim in
    fb := Framebuffer.init_framebuffer term_dim ;
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

  let rec apply_command =
    function
      | Noop    -> (fun x -> x)
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

  let apply_command_with_repetition n command editor =
    match command with
    | MoveOp m ->
          let rec loop m n view_height fb =
            if (n > 0)
              then loop m (n - 1) view_height (Fileview.apply_movement m view_height fb)
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
    Screen.put_color_rect editor.status_screen Config.default.colors.status (mk_rect 0 0 editor.term_dim.x 0)

  let refresh_screen editor =
    Tileset.draw_fileviews editor.tileset editor.frame_buffer ;
    show_status editor ;
    Framebuffer.render editor.frame_buffer editor.render_buffer ;
    editor

  let key_to_command = function
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

    | Keys.Lower_w      -> MoveModeOp Movement.Words
    | Keys.Upper_w      -> MoveModeOp Movement.Blocks
    | Keys.Lower_b      -> MoveModeOp Movement.Lines
    | Keys.Upper_b      -> MoveModeOp Movement.Lines
    | Keys.Lower_c      -> MoveModeOp Movement.Chars
    | Keys.Lower_s      -> MoveModeOp Movement.Selection
    | Keys.Lower_d      -> MoveModeOp Movement.Digits
    | Keys.Lower_z      -> MoveModeOp Movement.Paragraphs
    | Keys.Lower_x      -> MoveModeOp Movement.Parens

    | Keys.ArrowUp      -> MoveOp Movement.Up
    | Keys.ArrowDown    -> MoveOp Movement.Down
    | Keys.ArrowRight   -> MoveOp Movement.Left
    | Keys.ArrowLeft    -> MoveOp Movement.Right
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

    | Keys.Ctrl_j       -> Noop
    | Keys.Ctrl_k       -> Noop
    | Keys.Upper_g      -> Noop
    | Keys.Unknown      -> Noop (* ignore for now *)

    | Keys.EINTR        -> Resize

  let process_command editor =
    match editor.pending_input with
    | None          -> apply_command
    | Number digits -> apply_command_with_repetition (max 1 (dequeue_digits digits))

  let process_key editor =
    key_to_command >> (process_command editor)

  (* TODO: replace by a proper history of previous inputs *)
  let make_user_input key editor =
    let user_input = (pending_command_to_string editor.pending_input)
                   ^  key.Keys.repr
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

  let update_stats key input_duration editor =
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
        |> process_key editor key.Keys.symbol
        |> make_user_input key
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
      |> List.filter (fun k -> k.Keys.symbol <> Keys.Ctrl_c)
      |> Array.of_list

  let stop_key =
    Keys.code_to_key 3 (* Ctrl_c *)

  let next_key_fuzzer r n =
    let l = alen fuzz_keys in
    let i = ref 0 in
    let rec loop () =
      (*
      Unix.sleepf 0.01 ;
      *)
      incr i ;
      if !i > n
        then stop_key
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
 *  - bug: in paragraph mode when selecting the top most paragraph I get stuck there !
 *  - bug: with multiple view there is some crosstalk where the left border of the left view gets eat by the
 *    the right Overflow view !
 *  - bug: fix the cursor desired position offset caused by line breaking in overflow mode
 *  - bug: text color blocks with non zero horizontal scrolling are wrong
 *  - better management of screen dragging for horizontal scrolling
 *  - need to audit put_color_rect from bottom up
 *
 *  general cleanups:
 *  - variable renaming
 *  - solidification
 *  - doc
 *  - use named parameters and optional parameters
 *
 *  movements:
 *  - implement easymotion
 *  - once I have a proper tokenizer for the text:
 *    - Number tokenizer which recognizes 0xdeadbeef and 6.667e-11
 *    - Math operator movement
 *    - string movement
 *  - Proper tree navigation
 *  - bind brackets and braces delim movement
 *
 *  highlightning:
 *  - static syntax coloring based on tokens
 *
 *  hud: put movement/command history per Fileview and show in user input bar
 *
 *  minimal edition features
 *    - token deletion
 *    - token swap
 *    - token rotate
 *
 * perfs:
 *  - memory optimization for
 *      TokenMovement
 *      DelimMovement
 *
 * others:
 *  - hammer the code with asserts and search for more bugs in the drawing
 *
 * next features:
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
    t.buffers |> Array.map (fun fb -> fb.Filebuffer.filename)
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
          if fb.Filebuffer.filename = filepath
            then Some fb
            else loop (i + 1)
        else None
    in loop 0

  let close_buffers filepath t = (* IMPLEMENT *) t
end


module FileNavigator = struct

  let dir_ls path =
    let rec loop entries handle =
      match Unix.readdir handle with
      | s                     -> loop (s :: entries) handle
      | exception End_of_file -> entries
    in
    let handle = Unix.opendir path in
    let entries =
      loop [] handle
        |> List.rev
        |> Array.of_list
    in
      Array.sort String.compare entries ;
      Unix.closedir handle ;
      entries

  let dir_to_filebuffer path =
    path |> dir_ls |> Filebuffer.from_lines path

end
