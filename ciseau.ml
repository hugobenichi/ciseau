let starttime = Sys.time ()
let logs = open_out "/tmp/ciseau.log"

let kLOG_STATS = true

let tab_to_spaces = "  "

let alen = Array.length
let blen = Bytes.length
let slen = String.length

let try_finally action cleanup =
  let rez =
    try Ok (action ()) with e -> Error e
  in
    cleanup () ;
    match rez with
    | Ok success  -> success
    | Error error -> raise error

let (>>) f g x = g (f x)

let output_int f    = string_of_int >> output_string f
let output_float f  = string_of_float >> output_string f

let string_of_char c = String.make 1 c

let is_space      chr = (chr = ' ') || (chr = '\t') || (chr = '\r') || (chr = '\n')
let is_letter     chr = (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z'))
let is_digit      chr = ('0' <= chr) && (chr <= '9')
let is_alphanum   chr = (is_digit chr) || (is_letter chr)
let is_printable  chr = (' ' <= chr) && (chr <= '~')

let write fd buffer len =
  if Unix.write fd buffer 0 len <> len then raise (Failure "fd write failed")


module Error = struct
  open Printexc

  let default_depth = 20

  let format_loc =
    function
      | None -> "unknown loc"
      | Some { filename ; line_number ; start_char ; end_char } ->
          Printf.sprintf "at %s:\t%5d\t%d - %d" filename line_number start_char end_char

  let get_backtrace () =
    get_callstack default_depth
      |> backtrace_slots
      |> function
          | None -> "no stacktrace info"
          | Some slots ->
              slots
                |> Array.map (Slot.location >> format_loc)
                |> Array.to_list
                |> String.concat "\n"

  exception E of string * string

  let e msg =
    E (msg, get_backtrace ())

  let _ =
    Printexc.register_printer
      (function
        | E (msg, stacktrace)   ->  Some (msg ^ "\n" ^ stacktrace)
        | _                     ->  None)
end


module Slice = struct

  type range = int * int

  type 'a t = {
    data : 'a array ;
    range : range ;
  }

  let bound_checking = true

  let check_range (s, e) l =
    if bound_checking && (s < 0 || l < e) then
      raise (Error.e (Printf.sprintf "Bad slice range: cannot slice (%d,%d) from array of len %d" s e l))

  let shift (s, e) i =
    if bound_checking && (i < 0 || (e - s) < i) then
      raise (Error.e (Printf.sprintf "Slice index %d access is out of bounds (%d,%d)" i s e)) ;
    s + i

  let shift_range (s, e) (s', e') =
    check_range (s', e') (e - s) ;
    (s + s', s + e')

  let len { range = (s, e) } =
    e - s

  let get { data ; range } i =
    shift range i |> Array.get data

  let first slice =
    get slice 0

  let last slice =
    get slice ((len slice) - 1)

  let set { data ; range } i x =
    Array.set data (shift range i) x (* memory opt: do not use partial application + |> or >> *)

  let mk_slice s e data =
    check_range (s, e) (alen data) ;
    {
      data = data ;
      range = (s, e) ;
    }

  let init_slice_with_capacity len capacity zero =
    mk_slice 0 len (Array.make capacity zero)

  let init_slice len zero =
    mk_slice 0 len (Array.make len zero)

  let wrap_array data =
    mk_slice 0 (alen data) data

  let of_list ls =
    ls |> Array.of_list |> wrap_array

  let init_slice_fn len fn =
    Array.init len fn |> wrap_array

  let to_array { data ; range = (s, e) } =
    Array.sub data s (e - s)

  let clone slice =
    slice |> to_array |> wrap_array

  let reslice new_range { data ; range } =
    {
      data = data ;
      range = shift_range range new_range ;
    }

  let slice_left e slice =
    reslice (0, e) slice

  let slice_right s slice =
    reslice (s, len slice) slice

  let split slice pivot =
    (slice_left slice pivot, slice_right slice pivot)

  let iter fn slice =
    for i = 0 to (len slice) - 1 do
      fn (get slice i)
    done

  let iteri fn slice =
    for i = 0 to (len slice) - 1 do
      fn i (get slice i)
    done

  let rev slice =
    let l = len slice in
    let rev_index i = l - i - 1 in
    init_slice_fn l (rev_index >> get slice)

  let map fn slice =
    Array.init (len slice) (get slice >> fn) |> wrap_array

  let fold fn zero slice =
    let rec loop e acc i =
      if i < e
        then loop e (i |> get slice |> fn acc) (i + 1)
        else acc
    in
      loop (len slice) zero 0

  let filter fn slice =
    let slice' = clone slice in
    let fn out_idx elem =
      if fn elem
        then (set slice' out_idx elem ; out_idx + 1 )
        else out_idx
    in
      slice_left (fold fn 0 slice) slice'

  let copy dst_slice src_slice =
    let len = min (len dst_slice) (len src_slice) in
    let src_offset = shift src_slice.range 0 in
    let dst_offset = shift dst_slice.range 0 in
    Array.blit src_slice.data src_offset dst_slice.data dst_offset len

  let append elem { data ; range = (s, e) } =
    let len = alen data in
    let data' =
      if e < len
        then data
        else
          let new_data = Array.make (2 * len) data.(0) in
          Array.blit data 0 new_data 0 len ;
          new_data
    in
      Array.set data' e elem ;
      mk_slice s (e + 1) data'

  let cat slice1 slice2 =
    let l1 = len slice1 in
    let l2 = len slice2 in
    if l1 = 0
      then slice2
    else if l2 = 0
      then slice1
    else
      (* TODO: see if slice2 fits in slice1 backing array *)
      let output = init_slice (l1 + l2) (get slice1 0) in
      copy output slice1 ;
      copy (slice_right l1 output) slice2 ;
      output

  let sort_slice fn slice =
    (* TODO: better sort_slice function !! *)
    let ary = to_array slice
    in
      Array.fast_sort fn ary ;
      wrap_array ary
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

  let v2_zero = mk_v2 0 0

  let (<+>) t1 t2 =
    mk_v2 (t1.x + t2.x) (t1.y + t2.y)

  let (<->) t1 t2 =
    mk_v2 (t1.x - t2.x) (t1.y - t2.y)

  (* Check if second v2 argument is inside the implicit rectanlge woth topleft (0,0)
   * and first v2 argument as bottomright corner. *)
  let assert_v2_inside { x = xlim ; y = ylim } { x ; y } =
    if (x < 0) && (y < 0) && (x > xlim) && (y > ylim)
      then raise (Error.e (Printf.sprintf "(%d,%d) out of bound of (%d,%d" x y xlim ylim))
end


open Vec2


module Rect = struct
  type rect = {
    topleft     : v2 ;
    bottomright : v2 ;
  }

  let mk_rect tl_x tl_y br_x br_y = {
    topleft     = mk_v2 tl_x tl_y ;
    bottomright = mk_v2 br_x br_y ;
  }
end


open Rect


module Token = struct
  type 'a token = {
    kind  : 'a ;
    start : int ;
    stop  : int ;
  }

  let find_in_slice x tokens =
    assert (x < (Slice.last tokens).stop) ;
    let rec loop i =
      let { start ; stop } = Slice.get tokens i in
      if i < stop
        then i
        else loop (i + 1)
    in
      loop 0

end


module Movement = struct
  type t = Left | Right | Up | Down | Start | End
end


module MovementMode = struct
  type m = SpaceTokens | Lines | Chars (* | plus other kinds *)

  let mode_to_string =
    function
      | SpaceTokens   -> "Tokens"
      | Lines         -> "Lines"
      | Chars         -> "Chars"
end


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

  type color_cell = {
    fg : t ;
    bg : t ;
  }

  let black   = Normal Black ;;
  let red     = Normal Red ;;
  let green   = Normal Green ;;
  let yellow  = Normal Yellow ;;
  let blue    = Normal Blue ;;
  let magenta = Normal Magenta ;;
  let cyan    = Normal Cyan ;;
  let white   = Normal White ;;

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
    line_numbers  : color_cell ;
    focus_header  : color_cell ;
    header        : color_cell ;
    status        : color_cell ;
    user_input    : color_cell ;
    border        : color_cell ;
    no_text       : color_cell ;
  }

  type cfg = {
    colors    : colors ;
    page_size : int;
  }

  let darkgray = Color.Gray 2

  let default : cfg = {
    colors = {
      operator = {
        fg    = green ;
        bg    = darkgray ;
      } ;
      structure = {
        fg    = red ;
        bg    = darkgray ;
      } ;
      string  = {
        fg    = yellow ;
        bg    = darkgray ;
      } ;
      spacing = {
        fg    = darkgray ;
        bg    = darkgray ;
      } ;
      numbers = {
        fg    = magenta ;
        bg    = darkgray ;
      } ;
      default = {
        fg    = white ;
        bg    = darkgray ;
      } ;
      cursor_line = {
        fg    = white ;
        bg    = black ;
      } ;
      line_numbers = {
        fg    = green ;
        bg    = darkgray ;
      } ;
      focus_header = {
        fg    = darkgray ;
        bg    = yellow ;
      } ;
      header = {
        fg    = darkgray ;
        bg    = cyan ;
      } ;
      status = {
        fg    = darkgray ;
        bg    = white ;
      } ;
      user_input = {
        fg    = white ;
        bg    = darkgray ;
      } ;
      border = {
        fg    = white ;
        bg    = white ;
      } ;
      no_text = {
        fg    = Bold Magenta ;
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
                  | Colon
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
    mk_key Colon       ":"             58 ;
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
      | _   -> raise (Failure "next_char failed")
      | exception Unix.Unix_error (errcode,  fn_name, fn_param) ->
          Printf.fprintf logs "Unix_error errmsg='%s' fn='%s'\n" (Unix.error_message errcode) fn_name ;
          match errcode with
          | Unix.EINTR ->
            (* read interrupted, usually caused SIGWINCH signal handler for terminal resize: retry read *)
            code_to_key 256
          | _ -> raise (Unix.Unix_error (errcode, fn_name, fn_param))
    in one_byte_reader

end


module Block = struct
  type t = {
    text    : string ;
    offset  : int ;
    len     : int ;
  }

  let mk_block t = {
    text    = t ;
    offset  = 0 ;
    len     = slen t ;
  }

  let zero_block =
    mk_block ""
end


module Segment = struct
  type t = {
    pos : v2 ;
    len : int ;
  }

  let mk_segment x y l = {
    pos = mk_v2 x y ;
    len = l ;
  }
end


type linebreak = Clip | Overflow


module Line = struct
  type t = String of string | Block of Block.t | Blocks of Block.t list

  let zero_line = String ""

  let of_string s   = String s
  let of_block b    = Block b
  let of_blocks bs  = Blocks bs
end


module Area = struct

  (* Specifies an area w.r.t to a Screen, typically to be converted to a rectangle in Frambuffer
   * space for blitting colors or text *)
  type t =
      HorizontalSegment of Segment.t
    | VerticalSegment of Segment.t
    | Rectangle of rect
      (* Bounds for Line and Column are implicitly defined w.r.t some Screen bounds *)
    | Line of int
    | Column of int

  (* Converts an area in screen space into a rectangle in framebuffer space.
   * This follows the Framebuffer.put_color_rect convention:
   *  - last line in included: y length is correct for for loops
   *  - right-most column is excluded: horizontal length is correct for blit like functions *)
  let area_to_rectangle screen_offset screen_size =
    function
      | HorizontalSegment { Segment.pos ; Segment.len } ->
          assert (pos.x < screen_size.x) ;
          assert (pos.y < screen_size.y) ;
          assert (pos.x + len <= screen_size.x) ;
          mk_rect
            (screen_offset.x + pos.x)
            (screen_offset.y + pos.y)
            (screen_offset.x + len)
            (screen_offset.y + pos.y)
      | VerticalSegment { Segment.pos ; Segment.len } ->
          assert (pos.x < screen_size.x) ;
          assert (pos.y < screen_size.y) ;
          assert (len <= screen_size.y) ;
          mk_rect
            (screen_offset.x + pos.x)
            (screen_offset.y + pos.y)
            (screen_offset.x + pos.x + 1)
            (screen_offset.y + len)
      | Line y ->
          assert (y < screen_size.y) ;
          mk_rect
            (screen_offset.x + 0)
            (screen_offset.y + y)
            (screen_offset.x + screen_size.x)
            (screen_offset.y + y)
      | Column x ->
          assert (x < screen_size.x) ;
          mk_rect
            (screen_offset.x + x)
            (screen_offset.y + 0)
            (screen_offset.x + x + 1)
            (screen_offset.y + screen_size.y)
      | Rectangle { topleft ; bottomright } ->
          assert (topleft.x < screen_size.x) ;
          assert (topleft.y < screen_size.y) ;
          assert (bottomright.x < screen_size.x) ;
          assert (bottomright.y < screen_size.y) ;
          mk_rect
            (screen_offset.x + topleft.x)
            (screen_offset.y + topleft.y)
            (screen_offset.x + bottomright.x)
            (screen_offset.y + bottomright.y)
end


module Colorblock = struct
  type t = {
    area    : Area.t ;
    colors  : Color.color_cell ;
  }

  let mk_colorblock area colors = {
    area    = area ;
    colors  = colors;
  }

  let mk_colorsegment x y len colors =
    mk_colorblock (Area.HorizontalSegment (Segment.mk_segment x y len)) colors
end


module Textview = struct
  type t = {
    lines         : Line.t Slice.t ;
    colors        : Colorblock.t Slice.t ;
    cursor        : v2 option ;
  }
end


module type BytevectorType = sig
  type t

  val init_bytevector : int -> t
  val reset : t -> unit
  val append : t -> string -> unit
  val append_bytes : t -> Bytes.t -> int -> int -> unit
  val write : Unix.file_descr -> t -> unit
end


module type FramebufferType = sig
  type t
  type bytevector
  type segment

  val init_framebuffer  : v2 -> t
  val clear             : t -> unit
  val render            : t -> bytevector -> unit
  val put_color_rect    : t -> Color.color_cell -> rect -> unit
  val put_cursor        : t -> v2 -> unit
  val put_line          : t -> int -> int -> int -> Line.t -> unit
end


module type ScreenType = sig
  type t
  type framebuffer
  type block
  type segment

  val get_size      : t -> v2
  val get_offset    : t -> v2
  val get_width     : t -> int
  val get_height    : t -> int
  val mk_screen     : framebuffer -> rect -> t
  val mk_subscreen  : t -> rect -> t
  val put_text      : t -> Textview.t -> unit
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
    Slice.init_slice_fn n compute_segment

  let flip_xy_rect { topleft ; bottomright } =
    mk_rect topleft.y topleft.x bottomright.y bottomright.x

  let rec mk_view_ports total_area n_screen =
    function
      | { layout = Single } ->
          Slice.init_slice 1 total_area
      | _ when n_screen = 1 ->
          mk_view_ports total_area 1 Configs.zero
      | { layout = Columns ; orientation = Normal } ->
          let { topleft = offset ; bottomright = size } = total_area in
          split size.x n_screen
            |> Slice.map (fun (xl, xr) -> mk_rect (offset.x + xl) offset.y (offset.x + xr) size.y)
      | { layout = Columns ; orientation = Mirror } ->
          mk_config Columns Normal
            |> mk_view_ports total_area n_screen
            |> Slice.rev
      | { layout = Rows ; orientation } ->
          mk_config Columns orientation
            |> mk_view_ports (flip_xy_rect total_area) n_screen
            |> Slice.map flip_xy_rect
      | { layout = ColumnMajor ; orientation } ->
          let halves = mk_view_ports total_area 2 (mk_config Columns orientation) in
          let minors = mk_view_ports (Slice.get halves 1) (n_screen - 1) Configs.rows in
          Slice.cat (Slice.slice_left 1 halves) minors
      | { layout = RowMajor ; orientation } ->
          mk_config ColumnMajor orientation
            |> mk_view_ports (flip_xy_rect total_area) n_screen
            |> Slice.map flip_xy_rect
end


module type FileviewType = sig
  type t
  type view
  type filebuffer
  type screen

  val init_fileview : filebuffer -> t
  val set_mov_mode : MovementMode.m -> t -> t
  val apply_movement : (t -> v2) -> int -> t -> t
  val do_movement : Movement.t -> int -> t -> t
  val cursor : t -> v2
  val adjust_view : int -> t -> t
  val adjust_cursor : v2 -> t -> t
  val current_line : t -> string
  val current_char : t -> char
  val buflen : t -> int
  val swap_line_number_mode : t -> t
  val swap_linebreaking_mode : t -> t
  val recenter_view : int -> t -> t
  val draw : t -> screen -> bool -> unit
end


module type FilebufferSetType = sig
  type t
  type filebuffer

  val buffers_menu : t -> filebuffer (* TODO: this should return a Menu object that wraps a filebuffer *)
  val list_buffers : t -> filebuffer Slice.t
  val open_buffers : string -> t -> (t * filebuffer)
  val get_buffer : string -> t -> filebuffer option
  val close_buffers : string -> t -> t
end


module Bytevector : BytevectorType = struct

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
    Bytes.blit_string s 0 bvec.bytes bvec.cursor len ;
    bvec.cursor <- bvec.cursor + len

  let append_bytes bvec src offset len =
    Bytes.blit src offset bvec.bytes bvec.cursor len ;
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
      let {x ; y } = cursor_offset <+> vec2 in
      Printf.sprintf "%s%d;%dH" start y x

    let color_control_string_table : (Color.color_cell, string) Hashtbl.t = Hashtbl.create 1000

    let color_control_string colors =
      match Hashtbl.find color_control_string_table colors with
      | control_string -> control_string
      | exception Not_found ->
          let fg_code = Color.color_control_code colors.Color.fg in
          let bg_code = Color.color_control_code colors.Color.bg in
          let control_string = Printf.sprintf "38;5;%d;48;5;%dm" fg_code bg_code in
          Hashtbl.add color_control_string_table colors control_string ;
          control_string

  end

  external get_terminal_size : unit -> (int * int) = "get_terminal_size"

  let get_terminal_dimensions () =
    let (term_rows, term_cols) = get_terminal_size () in
    mk_v2 term_cols term_rows

  let do_with_raw_mode action =
    let open Unix in
    let stdout_write_string s =
      if (write_substring stdout s 0 (slen s)) <> slen s then raise (Failure "sdtout write failed")
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
      try_finally action (fun () ->
        tcsetattr stdin TCSAFLUSH initial ;
        stdout_write_string Control.switch_mainscreen ;
        stdout_write_string Control.cursor_restore
      )
    )

end


module Framebuffer : (FramebufferType with type bytevector = Bytevector.t and type segment = Segment.t) = struct

  module Default = struct
    let fg    = Color.white ;;
    let bg    = Config.darkgray ;;
    let z     = 0 ;;
    let text  = ' ' ;;
  end

  type bytevector = Bytevector.t
  type segment    = Segment.t

  type t = {
    text        : Bytes.t ;
    fg_colors   : Color.t array ;
    bg_colors   : Color.t array ;
    z_index     : int array ;
    len         : int ;
    window      : v2 ;

    mutable cursor : v2 ;
  }

  module Priv = struct
    let colors_at t offset =
      let open Color in {
        fg = t.fg_colors.(offset) ;
        bg = t.bg_colors.(offset) ;
      }

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

    let get_color_string t color_idx =
      (* MEMORY OPT: remove colors_at, for instance by cutting the color
       * control string in two parts and apprending both separately. *)
      Term.Control.color_control_string (colors_at t color_idx)

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
      let rec loop t start stop color_control_string bvec =
        (* memory opt: all arguments passed explicitly *)
        if start < stop
          then
            let len = next_line_len t start stop in
            Bytevector.append bvec Term.Control.start ;
            Bytevector.append bvec color_control_string ;
            Bytevector.append_bytes bvec t.text start len ;
            Bytevector.append bvec Term.Control.finish ;
            (* Last newline need to be appened *after* the terminating control command for colors *)
            append_newline_if_needed bvec t (start + len) ;
            loop t (start + len) stop color_control_string bvec
      in
        loop t start stop (get_color_string t start) bvec

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
      fg_colors   = Array.make len Default.fg ;
      bg_colors   = Array.make len Default.bg ;
      z_index     = Array.make len Default.z ;
      len         = len ;
      window      = vec2 ;
      cursor      = v2_zero ;
    }

  let clear t =
    Bytes.fill t.text 0 t.len Default.text ;
    Array.fill t.fg_colors 0 t.len Default.fg ;
    Array.fill t.bg_colors 0 t.len Default.bg ;
    Array.fill t.z_index 0 t.len Default.z

  let render frame_buffer render_buffer =
    Bytevector.reset render_buffer ;
    Bytevector.append render_buffer Term.Control.cursor_hide ;
    Bytevector.append render_buffer Term.Control.gohome ;
    Priv.render_all_sections frame_buffer render_buffer ;
    Bytevector.append render_buffer (Term.Control.cursor_control_string frame_buffer.cursor) ;
    Bytevector.append render_buffer Term.Control.cursor_show ;
    Bytevector.write Unix.stdout render_buffer

  let put_color_rect t { Color.fg ; Color.bg } { topleft ; bottomright } =
    for y = topleft.y to bottomright.y do
      let offset = y * t.window.x + topleft.x in
      let len = bottomright.x - topleft.x in
      Array.fill t.fg_colors offset len fg ;
      Array.fill t.bg_colors offset len bg
    done

  let put_cursor t cursor =
    t.cursor <- cursor

  let get_offset t x y =
    assert (x <= t.window.x) ;
    assert (y <= t.window.y) ;
    y * t.window.x + x

  let put_string t maxblitlen x y text =
    let offset = get_offset t x y in
    let blitlen = min maxblitlen (slen text) in
    assert (blitlen <= (t.len - offset)) ;
    Bytes.blit_string text 0 t.text offset blitlen

  let put_block
      t maxblitlen x y
      { Block.text ; Block.offset = block_offset ; Block.len = block_len } =
    let bytes_offset = get_offset t x y in
    let blitlen = min maxblitlen block_len in
    assert (blitlen <= (t.len - bytes_offset)) ;
    Bytes.blit_string text block_offset t.text bytes_offset blitlen

  let rec put_line framebuffer maxblitlen x y =
    let open Line in
    function
    | String s        ->
        put_string framebuffer maxblitlen x y s
    | Block b         ->
        put_block framebuffer maxblitlen x y b
    | Blocks []       -> ()
    | Blocks (b :: t) ->
        put_block framebuffer maxblitlen x y b ;
        let blitlen = min maxblitlen b.Block.len in
        let x' = x + blitlen in
        let maxblitlen' = maxblitlen - blitlen in
        if blitlen = b.Block.len then
          put_line framebuffer maxblitlen' x' y (Blocks t)
end


module Screen : (ScreenType with type framebuffer = Framebuffer.t and type block = Block.t and type segment = Segment.t) = struct

  type framebuffer  = Framebuffer.t
  type block        = Block.t
  type segment      = Segment.t

  type t = {
    size            : v2 ;
    screen_offset   : v2 ;
    frame_buffer    : Framebuffer.t ;
  }

  let get_size t =
    t.size

  let get_offset t =
    t.screen_offset

  let get_width t =
    t.size.x

  let get_height t =
    t.size.y

  (* Makes a screen with 'fb' as the backend framebuffer.
   * Passed in rectangle defines the screen absolute coordinates w.r.t the framebuffer *)
  let mk_screen fb { topleft ; bottomright } = {
    size                = bottomright <-> topleft ;
    screen_offset       = topleft ;
    frame_buffer        = fb ;
  }

  (* Makes a subscreen from this screen.
   * Passed in rectangle defines the subscreen absolut coordinates w.r.t the parent screen *)
  let mk_subscreen { size ; screen_offset ; frame_buffer } { topleft ; bottomright } =
    assert_v2_inside size topleft ;
    assert_v2_inside size bottomright ;
    mk_screen frame_buffer {
      topleft = (screen_offset <+> topleft) ;
      bottomright = (screen_offset <+> bottomright) ;
    }

  (* Put 'line' on 'screen' at 'line_y_offset' row if 'line_y_offset' is valid. *)
  let put_line screen line_y_offset line =
    if line_y_offset < screen.size.y then
      Framebuffer.put_line
        screen.frame_buffer
        screen.size.x
        screen.screen_offset.x
        (screen.screen_offset.y + line_y_offset)
        line

  let put_colorblock screen { Colorblock.area ; Colorblock.colors } =
    area
      |> Area.area_to_rectangle screen.screen_offset screen.size
      |> Framebuffer.put_color_rect screen.frame_buffer colors

  let put_text screen { Textview.lines ; Textview.colors ; Textview.cursor } =
    Slice.iteri (put_line screen) lines ;
    Slice.iter (put_colorblock screen) colors ;
    match cursor with
    | Some pos ->
        Framebuffer.put_cursor screen.frame_buffer (pos <+> screen.screen_offset)
    | None ->
        ()
end


module Filebuffer = struct
  type t = {
    filename    : string ;
    filepath    : string ;
    header      : string ;
    buffer      : string Slice.t ;        (* the file data, line per line *)
    buflen      : int ;                   (* number of lines in buffer, may be less than buffer array length *)
  }

  let read_file f =
    let rec loop lines ch =
      match input_line ch with
      | s                     -> loop (Slice.append s lines) ch
      | exception End_of_file -> lines
    in
    let ch = open_in f in
    let action () = loop (Slice.init_slice_with_capacity 0 32 "") ch in
    let cleanup () = close_in ch in
    try_finally action cleanup

  let from_lines file lines =
    let filepath = (Sys.getcwd ()) ^ "/" ^ file in {
      filename      = file ;
      filepath      = filepath ;
      header        = filepath ^ "  " ^ (lines |> Slice.len |> string_of_int) ^ "L " ;
      buffer        = lines ;
      buflen        = Slice.len lines ;
    }

  let init_filebuffer file =
    file |> read_file |> from_lines file

  let line_at { buffer } =
    Slice.get buffer

  let line_length { buffer } =
    Slice.get buffer >> slen (* TODO: take into account '\t' *)

  let last_cursor_x filebuffer y =
    (line_length filebuffer y) - 1 |> max 0
end


(* TODO:  - simplify go_first / go_right and go_last / go_right
 *        - use arrays or slices instead of List
 *        - clean up the tokenizers
 *        - implements Up/Down
 *        - break tokenizer from movement functions for reusability with other tokenizers
 *)
module SpaceTokenizer = struct
  open Token

  type kind = NonSpacePrintable | Other

  type t = kind token

  let mk_tok k i j = {
    kind  = k ;
    start = i ;
    stop  = j ;
  }

  let kind_of c =
    if c <> ' ' && is_printable c
      then NonSpacePrintable
      else Other

  let is_printable { kind } =
    ( kind = NonSpacePrintable )

  let tokenize text =
    let s = slen text in
    let kind_at i = assert (i < slen text) ; String.get text i |> kind_of in

    let rec loop tokens token =
      let i = token.stop in
      if i >= s
        then token :: tokens |> List.filter is_printable |> List.rev
        else
          match (token.kind, kind_at i) with
            | (k1, k2) when k1 = k2   -> loop tokens (mk_tok k1 token.start (i + 1))
            | (_, k2)                 -> loop (token :: tokens) (mk_tok k2 i (i + 1))
    in
      if s = 0
        then []
        else mk_tok (kind_at 0) 0 1 |> loop []

  let first ls =
    List.nth_opt ls 0

  let last ls =
    ls |> List.rev |> first

  let rec find x =
    function
      | [] -> None
      | tok :: tail when tok.start <= x && x < tok.stop -> Some tok
      | _ :: tail -> find x tail

  let rec next x =
    function
      | [] -> None
      | tok :: tail when x < tok.start -> Some tok
      | _ :: tail -> next x tail

  let rec prev x =
    function
      | []
      | _ :: [] -> None
      | t1 :: t2 :: tail when t1.stop <= x && x < t2.stop -> Some t1
      | _ :: tail -> prev x tail

  let rec go_token_first filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> tokenize
      |> first
      |> function
          | Some ({ Token.start}) -> mk_v2 start cursor.y
          | None ->
              if cursor.y + 1 < filebuffer.buflen
                then go_token_first filebuffer (mk_v2 0 (cursor.y + 1))
                else cursor

  let rec go_token_last filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> tokenize
      |> last
      |> function
          | Some ({ Token.start }) -> mk_v2 start cursor.y
          | None ->
              if cursor.y > 0
                then
                  let cursor_y' = cursor.y - 1 in
                  let cursor' = mk_v2 (Filebuffer.last_cursor_x filebuffer cursor_y') cursor_y' in
                  go_token_last filebuffer cursor'
                else v2_zero

  let go_token_right filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> tokenize
      |> next cursor.x
      |> function
          | Some ({ Token.start }) -> mk_v2 start cursor.y
          | None ->
              if cursor.y + 1 < filebuffer.buflen
                then go_token_first filebuffer (mk_v2 0 (cursor.y + 1))
                else cursor

  let go_token_left filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> tokenize
      |> prev cursor.x
      |> function
          | Some ({ Token.start }) -> mk_v2 start cursor.y
          | None ->
              if cursor.y > 0
                then
                  let cursor_y' = cursor.y - 1 in
                  let cursor' = mk_v2 (Filebuffer.last_cursor_x filebuffer cursor_y') cursor_y' in
                  go_token_last filebuffer cursor'
                else v2_zero

  let go_token_up filebuffer cursor =
    (* TODO *)
    cursor

  let go_token_down filebuffer cursor =
    (* TODO *)
    cursor

  let go_token_start filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> tokenize
      |> find cursor.x
      |> function
          | Some ({ Token.start })  -> mk_v2 start cursor.y
          | None                    -> cursor

  let go_token_end filebuffer cursor =
    Filebuffer.line_at filebuffer cursor.y
      |> tokenize
      |> find cursor.x
      |> function
          | Some ({ Token.stop }) -> mk_v2 (stop - 1) cursor.y
          | None                  -> cursor

  let movement =
    let open Movement in
    function
      | Left    -> go_token_left
      | Right   -> go_token_right
      | Up      -> go_token_up
      | Down    -> go_token_down
      | Start   -> go_token_start
      | End     -> go_token_end
end


module LineMovement = struct
  open Movement

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
    if cursor.y + 1 < filebuffer.Filebuffer.buflen
      then
        let y' = cursor.y + 1 in
        let x' = min cursor.x (Filebuffer.last_cursor_x filebuffer y') in
        mk_v2 x' y'
      else cursor

  let go_line_left filebuffer cursor =
    let cursor' = SpaceTokenizer.go_token_first filebuffer cursor in
    if cursor.x <= cursor'.x
      then
        let cursor2 = go_line_up filebuffer cursor in
        let cursor3 = SpaceTokenizer.go_token_first filebuffer cursor2 in
        (if cursor3 = cursor
          then cursor2
          else cursor3)
      else cursor'

  let go_line_right filebuffer cursor =
    let x' = Filebuffer.last_cursor_x filebuffer cursor.y in
    if cursor.x >= x'
      then go_line_down filebuffer cursor |> go_line_end filebuffer
      else go_line_end filebuffer cursor

  let movement =
    function
      | Left    -> go_line_left
      | Right   -> go_line_right
      | Up      -> go_line_up
      | Down    -> go_line_down
      | Start   -> go_line_start
      | End     -> go_line_end
end


module CharMovement = struct
  open Movement

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

  let movement =
    function
      | Start  (* What to do here ? *)
      | End    (* what to do here ? *)
      | Left    -> go_char_left
      | Right   -> go_char_right
      | Up      -> LineMovement.go_line_up
      | Down    -> LineMovement.go_line_down
end


module FileNavigator = struct

  let dir_ls path =
    let rec loop entries handle =
      match Unix.readdir handle with
      | s                     -> loop (Slice.append s entries) handle
      | exception End_of_file -> entries |> Slice.sort_slice String.compare
    in
    let handle = Unix.opendir path in
    let entries = loop (Slice.init_slice_with_capacity 0 16 "") handle in
      Unix.closedir handle ;
      entries

  let dir_to_filebuffer path =
    path |> dir_ls |> Filebuffer.from_lines path

end


(* TODO: Some of the first cracks in this reprensentation are already showing up.
         For instance, empty lines are empty strings, but in the file they take characters
         this requires special handling in the editor, because we still need to restore these empty lines at save
         and need to display them.
         The naive version of move_next_word therefore fails on empty lines without special handling.
         Similarly move_next_paragraph requires special cursor advance function, because it must not ignore
         empty line, while move_next_word must absolutely do.
         Furthermore, next word, next line, end-of-line, and so on should be first class concept in this
         representation. *)
module Fileview : (FileviewType with type view = Textview.t and type filebuffer = Filebuffer.t and type screen = Screen.t) = struct
  open Filebuffer

  type view       = Textview.t
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
      Array.init hardcoded_size (format_n >> Line.of_string)

    let get base_offset n =
      Array.get cache (base_offset + n + negative_offset)

    let line_number_cache_t2 = Sys.time () ;;
    Printf.fprintf logs "cache %f\n" (line_number_cache_t2 -. line_number_cache_t1) ;;
  end

  type t = {
    filebuffer    : filebuffer ;
    cursor        : v2 ;       (* current position in file space: x = column index, y = row index *)
    view_start    : int ;      (* index of first row in view *)
    numbering     : numbering_mode ;
    linebreaking  : linebreak ;
    mov_mode      : MovementMode.m ;
  }

  let init_fileview filebuffer = {
    filebuffer    = filebuffer ;
    cursor        = v2_zero ;
    view_start    = 0 ;
    numbering     = CursorRelative ;
    linebreaking  = Clip ;
    mov_mode      = MovementMode.Chars ;
  }

  let set_mov_mode m t =
    { t with mov_mode = m }

  let line_at t =
    Slice.get t.filebuffer.buffer

  let current_line t =
    line_at t t.cursor.y

  let current_char t =
    String.get (current_line t) t.cursor.x

  let current_line_len =
    current_line >> slen

  let line_len t i =
    line_at t i |> slen

  let cursor t = t.cursor

  let buflen t =
    Slice.len t.filebuffer.buffer

  let adjust_view screen_height t =
    let text_height = screen_height - 2 in (* -1 for header line, -1 for indexing starting at 0 *)
    if t.cursor.y < t.view_start then
      { t with
        view_start  = t.cursor.y ;
      }
    else if t.cursor.y > t.view_start + text_height then
      { t with
        view_start  = t.cursor.y - text_height ;
      }
    else t

  let adjust_cursor vec2 t = { t with cursor = vec2 }

  let apply_movement fn view_height t =
    t |> adjust_cursor (fn t) |> adjust_view view_height

  let do_movement m view_height t =
    let cursor' =
      match t.mov_mode with
      | SpaceTokens -> SpaceTokenizer.movement m t.filebuffer t.cursor
      | Lines       -> LineMovement.movement m t.filebuffer t.cursor
      | Chars       -> CharMovement.movement m t.filebuffer t.cursor
    in
      t |> adjust_cursor cursor' |> adjust_view view_height

  let swap_line_number_mode t =
    let new_mode = match t.numbering with
    | Absolute        -> CursorRelative
    | CursorRelative  -> Absolute
    in { t with numbering = new_mode }

  let swap_linebreaking_mode t =
    let new_mode = match t.linebreaking with
    | Clip      -> Overflow
    | Overflow  -> Clip
    in { t with linebreaking = new_mode }

  let recenter_view view_height t =
    let new_start = t.cursor.y - view_height / 2 in
    { t with view_start = max new_start 0 }

  (* TODO: move drawing in separate module ? *)

  module DrawingDefault = struct
    let frame_line        = Line.String " ~"
    let line_continuation = Line.String " ..."
    let text_line         = Line.String ""

    let header_focused_colorblock   = Colorblock.mk_colorblock (Area.Line 0) Config.default.colors.focus_header
    let header_unfocused_colorblock = Colorblock.mk_colorblock (Area.Line 0) Config.default.colors.header
  end

  (* facilitates passing data in and out of fill_linesinfo, put_text_lines, put_frame, ... *)
  type linesinfo = {
    (* input *)
    text_stop_y     : int ;
    text_size       : v2 ;
    line_number     : int -> Line.t ;
    (* output *)
    line_buffer     : Line.t Slice.t ;
    frame_buffer    : Line.t Slice.t ;
    mutable cursor  : v2 ;
    mutable last_y  : int ; (* index of the last line in line_buffer *)
  }

  (* PERF: hoist in fileview struct *)
  let mk_linesinfo t screen_size =
    let text_width = screen_size.x - 6 in (* line number offset *)
    let text_height = screen_size.y - 1 in (* header line *)
    let line_number_offset =
      match t.numbering with
        | Absolute        -> 1
        | CursorRelative  -> -t.cursor.y
    in
    {
      text_size     = mk_v2 text_width text_height ;
      text_stop_y   = min text_height ((buflen t) - t.view_start) ;
      line_buffer   = Slice.init_slice text_height DrawingDefault.text_line ;
      frame_buffer  = Slice.init_slice screen_size.y DrawingDefault.frame_line ;
      line_number   = LineNumberCache.get line_number_offset ;
      cursor        = v2_zero ;
      last_y        = 0 ;
    }

  let fill_linesinfo_with_clipping (t : t) (linesinfo : linesinfo) =
    let { text_size ; line_number ; line_buffer ; frame_buffer } = linesinfo in
    let x_last_index = text_size.x - 1 in (* Last valid x cursor position before scrolling *)
    let x_scrolling = max 0 (t.cursor.x - x_last_index) in
    linesinfo.cursor <- mk_v2 (t.cursor.x - x_scrolling) (t.cursor.y - t.view_start) ;
    for i = 0 to linesinfo.text_stop_y - 1 do
      let line_idx = i + t.view_start in
      let l = Slice.get t.filebuffer.buffer line_idx in
      let x_len = (slen l) - x_scrolling in
      let b =
        if x_len < 1 then Line.zero_line else {
          Block.text = l ;
          Block.offset = x_scrolling ;
          Block.len = min x_len text_size.x ;
        } |> Line.of_block
      in
      Slice.set line_buffer i b ;
      Slice.set frame_buffer (i + 1) (line_number line_idx) ;
      linesinfo.last_y <- i
    done

  let fill_linesinfo_with_wrapping t linesinfo =
    let { text_size ; line_number ; line_buffer ; frame_buffer } = linesinfo in
    (* 'i' is the input index, 'j' is the output index *)
    let rec loop j i line_x_pos =
      if j < linesinfo.text_stop_y then (
        let line_y_pos = i + t.view_start in
        let l = Slice.get t.filebuffer.buffer line_y_pos in
        let line_len = slen l in (* TODO: replace with Line.len for supporting tabs *)
        let b = {
          Block.text = l ;
          Block.offset = line_x_pos ;
          Block.len = min (line_len - line_x_pos) text_size.x ;
        } in
        let len_left = line_len - b.len - line_x_pos in
        let (next_i, next_offset) =
          if len_left = 0
            then (i + 1, 0)
            (* TODO: the line_x_pos should always moves in increments equal to 'text_size.x' *)
            else (i, line_x_pos + b.len)
        in
        Slice.set line_buffer j (Line.of_block b) ;
        Slice.set frame_buffer (j + 1)
          (if line_x_pos = 0
            then (line_number line_y_pos)
            else DrawingDefault.line_continuation) ;
        if line_y_pos = t.cursor.y && line_x_pos = 0 then (
          linesinfo.cursor <- mk_v2 (t.cursor.x mod text_size.x) (j + t.cursor.x / text_size.x) ;
          Printf.fprintf logs "line_y_pox=%d j=%d i=%d <-cursor.y=%d\n" line_y_pos j i linesinfo.cursor.y ;
          flush logs
        ) ;
        linesinfo.last_y <- j ;
        loop (j + 1) next_i next_offset
      )
    in
      loop 0 0 0

  let fill_linesinfo =
    function
    | Clip      -> fill_linesinfo_with_clipping
    | Overflow  -> fill_linesinfo_with_wrapping

  let put_text_lines text_screen linesinfo is_focused =
    let open Textview in {
      lines         = linesinfo.line_buffer ;
      colors        = Slice.wrap_array [|
        Colorblock.mk_colorblock (Area.Line linesinfo.cursor.y) Config.default.colors.cursor_line ;
        Colorblock.mk_colorblock (Area.Column linesinfo.cursor.x) Config.default.colors.cursor_line ;
      |] ;
      cursor        = if is_focused then Some linesinfo.cursor else None ;
    } |> Screen.put_text text_screen

  let put_frame screen linesinfo is_focused =
    let open Textview in {
      lines         = linesinfo.frame_buffer ;
      colors        = Slice.wrap_array [|
        (* PERF: this array could be hoisted into the fileview *)
        (if is_focused
          then DrawingDefault.header_focused_colorblock
          else DrawingDefault.header_unfocused_colorblock) ;
        (* default '~' column *)
        Colorblock.mk_colorblock
          (Area.VerticalSegment (Segment.mk_segment 1 1 linesinfo.text_size.y))
          Config.default.colors.no_text ;
        (* line numbers *)
        Colorblock.mk_colorblock
          (Area.Rectangle (mk_rect 1 1 6 (1 + linesinfo.last_y)))
          Config.default.colors.line_numbers ;
        (* border *)
        Colorblock.mk_colorblock
          (Area.VerticalSegment (Segment.mk_segment 0 1 linesinfo.text_size.y))
          Config.default.colors.border ;
        (* current cursor line *)
        Colorblock.mk_colorblock
          (Area.HorizontalSegment
            (Segment.mk_segment 1 (linesinfo.cursor.y + 1 (* +1 for header *)) 5))
          Config.default.colors.string ;
      |] ;
      cursor        = None ;
    } |> Screen.put_text screen

  let mk_text_subscreen screen =
    Screen.mk_subscreen screen {
      topleft = mk_v2 6 1 ; (* 6 for border + line number, 1 for header space *)
      bottomright = Screen.get_size screen ;
    }

  let draw t screen is_focused =
    let linesinfo = mk_linesinfo t (Screen.get_size screen) in
    fill_linesinfo t.linebreaking t linesinfo ;
    Slice.set linesinfo.frame_buffer 0 (Line.of_blocks [
      Block.mk_block t.filebuffer.Filebuffer.header  ;
      Block.mk_block (Printf.sprintf "%d,%d" t.cursor.y t.cursor.x) ;
      Block.mk_block ("  mode=" ^ MovementMode.mode_to_string t.mov_mode) ;
    ]) ;
    put_text_lines
      (mk_text_subscreen screen)
      linesinfo
      is_focused ;
    put_frame
      screen
      linesinfo
      is_focused
end


module FilebufferMovements = struct
  open Fileview

  let op_set_mov_mode m ignored t =
    Fileview.set_mov_mode m t

  let op_set_mov_tokens = op_set_mov_mode MovementMode.SpaceTokens
  let op_set_mov_lines  = op_set_mov_mode MovementMode.Lines
  let op_set_mov_chars  = op_set_mov_mode MovementMode.Chars

  let saturate_up length x = min (max (length - 1) 0) x

  let move_n_up n t = {
    x = (cursor t).x ;
    y = (cursor t).y |> fun x -> x - n |> max 0 ;
  }

  let move_n_down n t = {
    x = (cursor t).x ;
    y = (cursor t).y |> (+) n |> saturate_up (buflen t) ;
  }

  let move_page_up    t = move_n_up Config.default.page_size t ;;
  let move_page_down  t = move_n_down Config.default.page_size t ;;
  let move_file_start t = { x = (cursor t).x ; y = 0 } ;;
  let move_file_end   t = { x = (cursor t).x ; y = (buflen t) - 1 } ;;
end


module FilebufferSet : (FilebufferSetType with type filebuffer = Filebuffer.t) = struct

  type filebuffer = Filebuffer.t

  type t = {
    buffers : filebuffer Slice.t
  }

  let buffers_menu t =
    t.buffers |> Slice.map (fun fb -> fb.Filebuffer.filename)
              |> Filebuffer.from_lines "opened buffers"

  let list_buffers { buffers } =
    buffers

  let open_buffers filepath { buffers } =
    (* check if that buffer is not opened yet ! *)
    let fb = Filebuffer.init_filebuffer filepath in
    let buffers' = buffers |> Slice.clone |> Slice.append fb in
    ({ buffers = buffers' }, fb)

  let get_buffer filepath { buffers } =
    let e = Slice.len buffers in
    let rec loop i =
      if i < e
        then
          let fb = Slice.get buffers i in
          if fb.Filebuffer.filename = filepath
            then Some fb
            else loop (i + 1)
        else None
    in loop 0

  let close_buffers filepath t = (* IMPLEMENT *) t
end


module Stats = struct

  (* TODO: add every X a full stats collection for printing total current footprint *)
  (* TODO: print all allocated word diff to logs to get quantiles by post processing *)
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
    screen_tiles  : rect Slice.t ;
    focus_index   : int ;
    fileviews     : Fileview.t Slice.t ;
  }

  let adjust_fileviews screen_tiles fileviews =
    let n_tiles = Slice.len screen_tiles in
    let adjust_fileview i =
      let view = Slice.get fileviews i in
      (* In Single tile mode, len tiles < len fileviews *)
      if i < n_tiles
        then
          let tile = Slice.get screen_tiles i in
          let height = tile.bottomright.y - tile.topleft.y in
          Fileview.adjust_view height view
        else
          view
    in
    Slice.init_slice_fn (Slice.len fileviews) adjust_fileview

  let mk_tileset focus_index size screenconfig fileviews =
    assert (focus_index < Slice.len fileviews) ;
    let screen_tiles  = ScreenConfiguration.mk_view_ports size (Slice.len fileviews) screenconfig
    in {
      screen_size   = size ;
      screen_config = screenconfig ;
      screen_tiles  = screen_tiles ;
      focus_index   = focus_index ;
      fileviews     = adjust_fileviews screen_tiles fileviews ;
    }

  let draw_fileview t frame_buffer view_idx tile_idx =
      Fileview.draw
        (Slice.get t.fileviews view_idx)
        (Slice.get t.screen_tiles tile_idx |> Screen.mk_screen frame_buffer)

  let draw_fileviews t frame_buffer =
    let n_screens = Slice.len t.screen_tiles in
    if n_screens = 1
      then
        draw_fileview t frame_buffer t.focus_index 0 true
      else
        for i = 0 to n_screens - 1 do
          draw_fileview t frame_buffer i i (i = t.focus_index)
        done

  let get_focused_tile t =
    if Slice.len t.screen_tiles > 1
      then Slice.get t.screen_tiles t.focus_index
      else Slice.get t.screen_tiles 0

  let apply_op t =
    (* Any operation that changes the terminal size has to use mk_tileset for recomputing the tiles
     * Any operation that remaps tiles and fileviews has to use mk_tileset for adusting fileview height *)
    function
      | Resize new_screen_size ->
          mk_tileset
            t.focus_index new_screen_size t.screen_config t.fileviews
      | FileviewOp fileview_op ->
          let fileviews' = Slice.clone t.fileviews in
          let focused_tile = get_focused_tile t in
          let screen_height = focused_tile.bottomright.y - focused_tile.topleft.y in
          t.focus_index
            |> Slice.get fileviews'
            |> fileview_op screen_height
            |> Slice.set fileviews' t.focus_index ;
          { t with fileviews = fileviews' }
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
          let new_focus = (t.focus_index + 1) mod Slice.len t.fileviews  in
          { t with focus_index = new_focus }
      | FocusPrev ->
          let new_focus = (t.focus_index + (Slice.len t.fileviews) - 1) mod Slice.len t.fileviews in
          { t with focus_index = new_focus }
      | FocusMain ->
          { t with focus_index = 0 }
      | BringFocusToMain ->
          let fileviews' = Slice.clone t.fileviews in
          Slice.get t.fileviews t.focus_index |> Slice.set fileviews' 0 ;
          Slice.get t.fileviews 0             |> Slice.set fileviews' t.focus_index ;
          mk_tileset
            0 t.screen_size t.screen_config fileviews'
      (* TODO: for RotateViews, consider also rotating the focus_index *)
      | RotateViewsLeft ->
          let last_index = (Slice.len t.fileviews) - 1 in
          let fileviews' = Slice.clone t.fileviews in
          Slice.get t.fileviews last_index |> Slice.set fileviews' 0 ;
          Slice.copy (Slice.slice_right 1 fileviews') t.fileviews ;
          mk_tileset
            t.focus_index t.screen_size t.screen_config fileviews'
      | RotateViewsRight ->
          let last_index = (Slice.len t.fileviews) - 1 in
          let fileviews' = Slice.clone t.fileviews in
          Slice.get t.fileviews last_index |> Slice.set fileviews' 0 ;
          Slice.copy (Slice.slice_right 1 fileviews') t.fileviews ;
          mk_tileset
            t.focus_index t.screen_size t.screen_config fileviews'
end


module Ciseau = struct

  type pending_command_atom = Digit of int

  type pending_command = None
                       | Number of int list

  (* Represents an editor command *)
  type command = Noop
               | Stop
               | Resize
               | TilesetOp of Tileset.op
               | Move of (Fileview.t -> v2)
               | MoveVerb of Movement.t
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
    filebuffers     : Filebuffer.t Slice.t ; (* TODO: turn this into buffer management layer *)
    user_input      : string ;
    pending_input   : pending_command ;
    status_colorblocks : Colorblock.t Slice.t ;
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
      Filebuffer.init_filebuffer "./ciseau.ml" ;
      Filebuffer.init_filebuffer "./ioctl.c" ;
      (* Filebuffer.init_filebuffer "./Makefile" ; (* FIX tabs *) *)
      (* FileNavigator.dir_to_filebuffer (Sys.getcwd ()) ; *)
    |] |> Slice.wrap_array

  let test_mk_fileviews term_dim =
    Slice.map Fileview.init_fileview

  let mk_status_colorblocks term_width =
    Slice.wrap_array [|
      (* TODO: replace with Area.Line *)
      Colorblock.mk_colorsegment 0 0 term_width Config.default.colors.status ;
      Colorblock.mk_colorsegment 0 1 term_width Config.default.colors.user_input ;
    |]

  let mk_tileset term_dim filebuffers =
    filebuffers
      |> Slice.map Fileview.init_fileview
      |> Tileset.mk_tileset 0 (main_screen_dimensions term_dim) ScreenConfiguration.Configs.columns

  let init_editor file =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_framebuffer term_dim in
    let filebuffers = test_mk_filebuffers file
    in {
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
      status_colorblocks = mk_status_colorblocks term_dim.x ;
      stats           = Stats.init_stats () ;
      log_stats       = kLOG_STATS ;
    }

  let resize_editor editor =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_framebuffer term_dim
    in {
      editor with
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      frame_buffer    = frame_buffer ;
      status_screen   = mk_status_screen frame_buffer term_dim ;
      tileset         = Tileset.apply_op editor.tileset (Tileset.Resize (main_screen_dimensions term_dim)) ;
      status_colorblocks = mk_status_colorblocks term_dim.x ;
    }

  let queue_pending_command editor = function
    | Digit n -> { editor with pending_input = enqueue_digit n editor.pending_input }

  let apply_command command editor =
    match command with
    | Noop    -> editor
    | Stop    -> { editor with running = false }
    | Resize  -> resize_editor editor
    | TilesetOp op ->
        { editor with tileset = Tileset.apply_op editor.tileset op }
    | Move fn ->
        let fileview_op = Tileset.FileviewOp (Fileview.apply_movement fn) in {
          editor with
          tileset = Tileset.apply_op editor.tileset fileview_op ;
        }
    | MoveVerb mov ->
        let fileview_op = Tileset.FileviewOp (Fileview.do_movement mov) in {
          editor with
          tileset = Tileset.apply_op editor.tileset fileview_op ;
        }
    | View fn ->
        let fileview_op = Tileset.FileviewOp (fun ignored_view_height fileview -> fn fileview) in {
          editor with
          tileset = Tileset.apply_op editor.tileset fileview_op ;
        }
      (* cannot happen ?? *)
    | Pending ((Digit n) as d) ->
        queue_pending_command editor d

  let apply_command_with_repetition n command editor =
    match command with
    | Move fn ->
      let rec loop n view_height fb =
        if (n > 0)
          then loop (n - 1) view_height (Fileview.apply_movement fn view_height fb)
          else fb
      in
      let fileview_op = Tileset.FileviewOp (loop n) in {
        editor with
        tileset = Tileset.apply_op editor.tileset fileview_op ;
        pending_input = None ;
      }
    | Pending ((Digit n) as d)  -> queue_pending_command editor d
      (* for other command, flush any pending digits *)
    | _ -> apply_command command { editor with pending_input = None }

  let show_status editor =
    let status_text1 = "Ciseau stats: win = "
                      ^ editor.term_dim_descr
                      ^ (Stats.format_stats editor.stats)
    in
    let status_text2 = editor.user_input
    in
      let open Textview in
      Screen.put_text editor.status_screen {
        lines         = Slice.wrap_array [|
          Line.String status_text1 ;
          Line.String status_text2 ;
        |] ;
        colors        = editor.status_colorblocks ;
        cursor        = None ;
      }

  let refresh_screen editor =
    (* PERF: only clear rectangles per subscreen *)
    Framebuffer.clear editor.frame_buffer ;
    Tileset.draw_fileviews editor.tileset editor.frame_buffer ;
    show_status editor ;
    Framebuffer.render editor.frame_buffer editor.render_buffer ;
    editor

  let key_to_command = function
    | Keys.Ctrl_c       -> Stop
    | Keys.Backslash    -> View Fileview.swap_line_number_mode
    | Keys.Pipe         -> View Fileview.swap_linebreaking_mode
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

    (* TODO: Introduce shortcut Variant to replace TilesetOp (Tileset.FileviewOp _) *)
    | Keys.Lower_w      -> TilesetOp (Tileset.FileviewOp FilebufferMovements.op_set_mov_tokens) (* chenge to word when available *)
    | Keys.Upper_w      -> TilesetOp (Tileset.FileviewOp FilebufferMovements.op_set_mov_tokens)
    | Keys.Lower_b      -> TilesetOp (Tileset.FileviewOp FilebufferMovements.op_set_mov_lines)
    | Keys.Upper_b      -> TilesetOp (Tileset.FileviewOp FilebufferMovements.op_set_mov_lines)
    | Keys.Lower_c      -> TilesetOp (Tileset.FileviewOp FilebufferMovements.op_set_mov_chars)

    | Keys.Ctrl_d       -> Move FilebufferMovements.move_page_down
    | Keys.Ctrl_u       -> Move FilebufferMovements.move_page_up
                          (* TODO: reimplement paragraph *)
    | Keys.Ctrl_j       -> Noop (* Move FilebufferMovements.move_next_paragraph *)
    | Keys.Ctrl_k       -> Noop (* Move FilebufferMovements.move_prev_paragraph *)

    (* TODO: rebind these
    | Keys.Alt_k        -> Move FilebufferMovements.move_file_start
    | Keys.Alt_j        -> Move FilebufferMovements.move_file_end
     *)

    | Keys.ArrowUp      -> MoveVerb Movement.Up
    | Keys.ArrowDown    -> MoveVerb Movement.Down
    | Keys.ArrowRight   -> MoveVerb Movement.Left
    | Keys.ArrowLeft    -> MoveVerb Movement.Right
    | Keys.Lower_k      -> MoveVerb Movement.Up
    | Keys.Lower_j      -> MoveVerb Movement.Down
    | Keys.Lower_l      -> MoveVerb Movement.Right
    | Keys.Lower_h      -> MoveVerb Movement.Left

    | Keys.Upper_h      -> MoveVerb Movement.Start
    | Keys.Upper_j      -> MoveVerb Movement.Down
    | Keys.Upper_k      -> MoveVerb Movement.Up
    | Keys.Upper_l      -> MoveVerb Movement.End

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

    | Keys.Lower_z      -> Noop
    | Keys.Lower_x      -> Noop
    | Keys.Upper_g      -> Noop
    | Keys.Colon        -> Noop
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
      Printexc.record_backtrace true ;
      let file =
        if alen Sys.argv > 1
          then Sys.argv.(1)
          else __FILE__
      in file |> init_editor
              |> run_editor_loop
              |> Term.do_with_raw_mode
    with
      e ->
          e |> Printexc.to_string |> Printf.printf "\nerror: %s\n" ;
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
      (* Unix.sleepf 0.1 ; *)
      incr i ;
      if !i > n
        then stop_key
        else
          Random.State.int r l |> Array.get fuzz_keys
    in loop

  let run_editor_loop n editor () =
    let r = Random.State.make [| 0 ; 1 ; 2 |] in
    Ciseau.loop (next_key_fuzzer r n) editor

  let main n =
    try
      Printexc.record_backtrace true ;
      let file =
        if alen Sys.argv > 1
          then Sys.argv.(1)
          else __FILE__
      in file |> Ciseau.init_editor
              |> run_editor_loop n
              |> Term.do_with_raw_mode
    with
      e ->
          e |> Printexc.to_string |> Printf.printf "\nerror: %s\n" ;
          Printexc.print_backtrace stdout

end


let log_sigwinch sig_n =
  (* Nothing to do: when SIGWINCH is handled, the read pending on keyboard input is interrupted.
   * The EINTR interrupt codepath there will trigger the resizing *)
  ()

let sigwinch = 28 (* That's for OSX *)


let () =
  Sys.Signal_handle log_sigwinch |> Sys.set_signal sigwinch ;
  Ciseau.main () ;
  (* Fuzzer.main 500 ; *)
  close_out logs


(* next TODOs:
 *
 * new movements
 *  - replug repetition
 *  - fuse Move and MoveVerb
 *  - implement other movement modes
 *      - words
 *      - digits
 *      - paragraph
 *      - curly braces, parentheses, brackets
 *  - preserve desired cursor.x when spanning lines where length is less than cursors.x
 *  - implement easymotion
 *
 *  highlightning
 *    - mode for show all tokens in current token movement mode
 *    - mode for showing next / prev / up / down landing locations
 *
 *
 *  minimal edition features
 *    - starts to add token deletion bound to 'x'
 *
 * bugs:
 *    - in Overflow mode, the cursor dragging in adjust_view is off
 *      - since in Clip mode the view is correct, it seems that the issue is an incorrect cursor position (again !)
 *      - the adjust_view error seems to be equal to the number of overflow lines in the screenview.
 *
 *  - Framebuffer should be cleared selectively by subrectangles that need to be redrawn.
 *  - hammer the code with asserts and search for more bugs in the drawing
 *  - write function docs and comments
 *
 * next features:
 *  - finish file navigation
 *  - word selections ?
 *  - find
 *    - add vim's incsearch feature
 *  - static keyword hightlightning
 *)
