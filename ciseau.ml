(* next TODOs:
 *  - optimize a bit memory usage
 *      - The Framebuffer should only be cleared selectively by subrectangles to redraw stuff that needs to be redrawn
 *      - introduce an array slice type and replace a bunch of list with arrays or array slices
 *
 *  - add highlight fusion to get_view and support selections, cursor line highlight, ...
 *)


let starttime = Sys.time ()
let logs = open_out "/tmp/ciseau.log"

let conj ls x = List.cons x ls

(* TODO replace with List.init in ocaml 4.06 *)
let mk_list size e =
  let rec loop acc n =
  if n = 0
    then acc
    else loop (e :: acc) (n - 1)
  in
    loop [] size

let tab_to_spaces = "  "

let alen = Array.length ;;
let blen = Bytes.length ;;
let slen = String.length ;;

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

let truncate_string l s =
  if slen s > l then String.sub s 0 l else s

let is_empty s = (slen s = 0)

let is_space      chr = (chr = ' ') || (chr = '\t') || (chr = '\r') || (chr = '\n') ;;
let is_letter     chr = (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z')) ;;
let is_digit      chr = ('0' <= chr) && (chr <= '9') ;;
let is_alphanum   chr = (is_digit chr) || (is_letter chr) ;;
let is_printable  chr = (' ' <= chr) && (chr <= '~') ;;

let write fd buffer len =
  if Unix.write fd buffer 0 len <> len then raise (Failure "fd write failed")


module Slice = struct

  type range = int * int

  type 'a t = {
    data : 'a array ;
    range : range ;
  }

  exception Bad_range of string
  exception Out_of_bounds of string

  let bound_checking = true

  let check_range (s, e) l =
    if bound_checking && (s < 0 || l < e)
      then raise (Bad_range (Printf.sprintf "cannot slice (%d,%d) from array of len %d" s e l))

  let shift (s, e) i =
    if bound_checking && (i < 0 || (e - s) < i)
      then raise (Out_of_bounds (Printf.sprintf "index %d is out of bounds (%d,%d)" i s e)) ;
    s + i

  let shift_range (s, e) (s', e') =
    check_range (s', e') (e - s) ;
    (s + s', s + e')

  let len { range = (s, e) } =
    e - s

  let get { data ; range } i =
    i |> shift range |> Array.get data

  let set { data ; range } i =
    i |> shift range |> Array.set data

  let mk_slice s e data =
    check_range (s, e) (alen data) ;
    {
      data = data ;
      range = (s, e) ;
    }

  let init_slice len capacity zero =
    mk_slice 0 len (Array.make capacity zero)

  let wrap_array data =
    mk_slice 0 (alen data) data

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
      let output = init_slice (l1 + l2) (l1 + l2) (get slice1 0) in
      copy output slice1 ;
      copy (slice_right l1 output) slice2 ;
      output

  let sort_slice fn slice =
    (* TODO: better sort_slice function !! *)
    let ary = to_array slice
    in
      Array.fast_sort fn ary ;
      wrap_array ary

  let test _ =
    try
      Printexc.record_backtrace true ;

      let to_string fn slice =
        slice |> map fn |> to_array |> Array.to_list |> String.concat " ; "
      in
      let println s =
        print_string s ;
        print_newline ()
      in

      let a = [| 1 ; 2 ; 3 ; 4 ; 5 ; 6 |] in
      let s1 = wrap_array a in
      s1 |> len |> Printf.printf "%d\n" ;
      s1 |> reslice (3, 4) |> len |> Printf.printf "%d\n" ;
      s1 |> slice_left 3 |> len |> Printf.printf "%d\n" ;
      s1 |> slice_left 6 |> len |> Printf.printf "%d\n" ;

      print_newline () ;

      s1 |> iter print_int ; print_newline () ;
      s1 |> reslice (3, 4) |> iter print_int ; print_newline () ;
      s1 |> reslice (1, 4) |> iter print_int ; print_newline () ;
      s1 |> slice_left 3 |> iter print_int ; print_newline () ;
      s1 |> slice_left 6 |> iter print_int ; print_newline () ;

      print_newline () ;

      s1 |> map (fun x -> x * 2) |> to_string string_of_int |> println ;
      s1 |> reslice (3, 4) |> map (fun x -> x * 2) |> to_string string_of_int |> println ;
      s1 |> reslice (1, 4) |> map (fun x -> x * 2) |> to_string string_of_int |> println ;

      print_newline () ;

      s1 |> filter (fun x -> x mod 2 == 0) |> to_string string_of_int |> println ;
      s1 |> filter (fun x -> x < 4) |> to_string string_of_int |> println ;
      s1 |> filter (fun x -> x < 9) |> to_string string_of_int |> println ;
      s1 |> filter (fun x -> x > 9) |> to_string string_of_int |> println ;
      s1 |> reslice (1, 4) |> filter (fun x -> x mod 2 = 0) |> to_string string_of_int |> println ;

      print_newline () ;

      let s2 = clone s1 in
      set (slice_right 2 s2) 3 42 ;
      s1 |> to_string string_of_int |> println ;
      s2 |> to_string string_of_int |> println ;

      print_newline () ;

      let s3 = wrap_array [| 0 ; 0 ; 0 ; 0 ; 0 |] in
      s3 |> to_string string_of_int |> println ;
      copy s3 s1 ;
      s3 |> to_string string_of_int |> println ;
      [| 0 ; 0 ; 0 ; 0 ; 0 ; 0|] |> wrap_array |> copy s3 ;
      s3 |> to_string string_of_int |> println ;
      copy s3 (reslice (0, 2) s1) ;
      copy (slice_right 3 s3) (slice_right 3 s1) ;
      s3 |> to_string string_of_int |> println ;

      print_newline () ;

      [| 0 ; 0 ; 0 |] |> wrap_array |> append 1 |> append 2 |> append 3 |> to_string string_of_int |> println  ;
      [| 20 ; 30 ; 40 |] |> wrap_array |> reslice (1, 1) |> append 1 |> to_string string_of_int |> println  ;
      [| 20 ; 30 ; 40 |] |> wrap_array |> reslice (1, 2) |> append 1 |> to_string string_of_int |> println  ;
      [| 20 ; 30 ; 40 |] |> wrap_array |> reslice (1, 2) |> append 1 |> append 2 |> append 3 |> append 3 |> to_string string_of_int |> println  ;
      print_newline () ;

      [| 0 |]                 |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 |]             |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 ; 2 |]         |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 ; 2 ; 3 |]     |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 ; 2 ; 3 ; 4 |] |> wrap_array |> rev |> to_string string_of_int |> println ;

      ()
  with
    e ->
        e |> Printexc.to_string |> Printf.printf "\nerror: %s\n" ;
        Printexc.print_backtrace stdout

end


module Vec2 = struct

  type v2 = {
    x : int ;
    y : int ;
  }

  let v2_zero = {
    x = 0 ;
    y = 0 ;
  }

  let v2_x1y0 = {
    x = 1 ;
    y = 0 ;
  }

  let v2_x0y1 = {
    x = 0 ;
    y = 1 ;
  }

  let mk_v2 x y = {
    x = x ;
    y = y ;
  }

  let (<+>) t1 t2 =
    mk_v2 (t1.x + t2.x) (t1.y + t2.y)

  let (<->) t1 t2 =
    mk_v2 (t1.x - t2.x) (t1.y - t2.y)

  let v2_to_string t =
    Printf.sprintf "%d,%d" t.y t.x

  let v2_to_offset stride vec2 =
    vec2.y * stride + vec2.x

  let offset_to_v2 stride offset =
    mk_v2 (offset mod stride) (offset / stride)
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
  }

  type cfg = {
    colors : colors ;
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
        bg    = Gray 4 ;
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
        fg    = darkgray ;
        bg    = Gray 8 ;
      } ;
    } ;
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
                  | Alt_h
                  | Alt_j
                  | Alt_k
                  | Alt_l
                  | Space
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
                  | Lower_w
                  | Upper_b
                  | Upper_w
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

  let _ = [
    mk_key Ctrl_c      "Ctrl_c"        3 ;
    mk_key Ctrl_d      "Ctrl_d"        4 ;
    mk_key Ctrl_j      "Ctrl_j"        10 ;
    mk_key Ctrl_k      "Ctrl_k"        11 ;
    mk_key Ctrl_u      "Ctrl_u"        21 ;
    mk_key Ctrl_z      "Ctrl_z"        26 ;
    mk_key Space       "Space"         32 ;
    mk_key Equal       "Equal"         61 ;
    mk_key ArrowUp     "ArrowUp"       65 ;
    mk_key ArrowDown   "ArrowDown"     66 ;
    mk_key ArrowRight  "ArrowRight"    67 ;
    mk_key ArrowLeft   "ArrowLeft"     68 ;
    mk_key Upper_b     "W"             66 ;
    mk_key Upper_w     "B"             87 ;
    mk_key Lower_b     "w"             98 ;
    mk_key Lower_h     "h"             104 ;
    mk_key Lower_j     "j"             106 ;
    mk_key Lower_k     "k"             107 ;
    mk_key Lower_l     "l"             108 ;
    mk_key Lower_w     "w"             119 ;
    mk_key Alt_h       "Alt_h"         153 ;
    mk_key Alt_j       "Alt_j"         134 ;
    mk_key Alt_k       "Alt_k"         154 ;
    mk_key Alt_l       "Alt_l"         172 ;
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
  ] |> List.iter (fun k -> code_to_key_table.(k.code) <- k)

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
    colors  : Color.color_cell ;
  }

  let mk_block t c = {
    text    = t ;
    offset  = 0 ;
    len     = slen t ;
    colors  = c ;
  }

  let wrap_string s =
    mk_block s Config.default.colors.default

  let rec adjust_length want_len blk =
    if want_len > blk.len
      then {
        blk with
        text = blk.text ^ (String.make (want_len - blk.len) ' ') ;
        len = want_len ;
      }
      else {
        blk with
        len = want_len ;
      }

  let split_at l b =
    let blen = b.len in
    if blen <= l
      then (b, None)
      else
        let t1 = String.sub b.text 0 l in
        let t2 = String.sub b.text l (blen - l) in
        ( mk_block t1 b.colors, Some (mk_block t2 b.colors) )

  let break_block_line left ls =
    let rec loop left acc ls =
      match ls with
        | [] -> (acc, ls)
        | _ when left < 1 -> (acc, ls)
        | b :: r ->
            match split_at left b with
            | (b1, Some b2) -> (b1 :: acc, b2 :: r)
            | (_, None) -> loop (left - b.len) (b :: acc) r
    in
      let (rev_first_line, remainder) = loop left [] ls
      in  (List.rev rev_first_line, remainder)

  let rec drop n ls =
    if n = 0
      then []
      else match ls with
        | [] -> []
        | h :: t -> h :: (drop (n - 1) t)

  let break_block_line_text bounds block_lines =
    let rec loop acc =
      function
      | [] -> acc
      | blocks :: rest_lines ->
          match break_block_line bounds.x blocks with
          | (blocks', [])         -> loop (blocks' :: acc) rest_lines
          | (blocks', remainder)  -> loop (blocks' :: acc) (remainder :: rest_lines)
    in
      block_lines |> loop [] |> List.rev |> drop bounds.y

  let clip_block_line_text bounds block_lines =
    let rec loop acc =
      function
      | [] -> acc
      | blocks :: rest_lines ->
          let (blocks', _) = break_block_line bounds.x blocks in
          loop (blocks' :: acc) rest_lines
    in
      block_lines |> loop [] |> List.rev |> drop bounds.y

  type linebreak    = Clip | Overflow

  let get_line_breaker =
    function
    | Clip      -> clip_block_line_text
    | Overflow  -> break_block_line_text

end

module Segment = struct
  type t = { pos : v2 ; len : int }

  let mk_segment x y l = {
    pos = mk_v2 x y ;
    len = l ;
  }
end

(* Represents the result of projecting a line of text inside a drawing view rectangle *)
type line_info = LineInfo of {
  blocks      : Block.t list ;
}

type text_view = TextView of {
  offset        : int ;
  lines         : line_info list ;
  cursor        : v2 ;
  linebreaking  : Block.linebreak ;
}

module type BytevectorType = sig
  type t

  val init_bytevector : int -> t
  val reset : t -> t
  val append : string -> t -> t
  val append_bytes : Bytes.t -> int -> int -> t -> t
  val write : Unix.file_descr -> t -> unit
end


module type FramebufferType = sig
  type t
  type bytevector
  type segment

  val init_frame_buffer : v2 -> t
  val clear             : t -> unit
  val render            : v2 -> t -> bytevector -> unit
  val put_block         : v2 -> Block.t -> t -> unit
end


module type ScreenType = sig
  type t
  type framebuffer
  type block
  type segment

  val get_size        : t -> v2
  val get_offset      : t -> v2
  val get_width       : t -> int
  val get_height      : t -> int
  val init_screen     : framebuffer -> rect -> t
  val put_block_lines : t -> Block.linebreak -> int -> block list list -> unit
  val put_line        : t -> int -> block -> unit
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
    Slice.init_slice_fn n (fun i -> (a * i, a * ( i + 1) ))

  let flip_xy_rect { topleft ; bottomright } =
    mk_rect topleft.y topleft.x bottomright.y bottomright.x

  let rec mk_view_ports total_area n_screen =
    function
      | { layout = Single } ->
          Slice.init_slice 1 1 total_area
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

  val init_fileview : int -> filebuffer -> t (* TODO: eliminate view_h param *)
  val apply_movement : (t -> v2) -> t -> t
  val cursor : t -> v2
  val cursor_next_char : t -> v2
  val cursor_prev_char : t -> v2
  val cursor_next_line : t -> v2
  val cursor_prev_line : t -> v2
  val is_current_char_valid : t -> bool
  val adjust_cursor : v2 -> t -> t
  val current_line : t -> string
  val current_char : t -> char
  val buflen : t -> int
  val view_diff : t -> int
  val swap_line_number_mode : t -> t
  val swap_linebreaking_mode : t -> t
  val recenter_view : t -> t
  val cursor_relative_to_view : v2 -> t -> v2
  val file_length_string : t -> string
  val get_view : int -> t -> view

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
    bytes : bytes ;
    len : int ;
  }

  module Priv = struct

    let scale size =
      size |> float |> ( *. ) 1.45 |> ceil |> truncate

    let rec next_size needed_size size =
      if needed_size <= size then size else next_size needed_size (scale size)

    let grow new_size bytes =
      Bytes.extend bytes 0 (new_size - (blen bytes))

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

  end

  let init_bytevector len = {
    bytes = Bytes.make len (Char.chr 0) ;
    len   = 0 ;
  }

  let reset bytevec = {
    bytes = bytevec.bytes ;
    len   = 0 ;
  }

  let append s t =
    let s_len = slen s in
    let t' = Priv.grow s_len t in
      Bytes.blit_string s 0 t'.bytes t.len s_len ;
      t'

  let append_bytes srcbytes srcoffset len t =
    let t' = Priv.grow len t in
      Bytes.blit srcbytes srcoffset t'.bytes t.len len ;
      t'

  let write fd t =
    write fd t.bytes t.len
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
  }

  module Priv = struct
    let colors_at t offset =
      let open Color in {
        fg = t.fg_colors.(offset) ;
        bg = t.bg_colors.(offset) ;
      }

    let next_contiguous_color_section t start =
      let colors_to_match = colors_at t start in
      let rec loop stop =
        if stop < t.len && (colors_at t stop) = colors_to_match
        then loop (stop + 1)
        else stop
      in loop (start + 1)

    let render_section color_cell start stop t bvec =
      (* append lines one at a time starting from start offset, ending at stop offset *)
      let next_line_len t start stop =
        min (t.window.x - (start mod t.window.x)) (stop - start)
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
               |> Bytevector.append (Term.Control.color_control_string color_cell)
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

    let render_all_sections t bvec =
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
  end

  let init_frame_buffer vec2 =
    let len = vec2.x * vec2.y
    in {
      text        = Bytes.make len Default.text ;
      fg_colors   = Array.make len Default.fg ;
      bg_colors   = Array.make len Default.bg ;
      z_index     = Array.make len Default.z ;
      len         = len ;
      window      = vec2 ;
    }

  let clear t =
    Bytes.fill t.text 0 t.len Default.text ;
    Array.fill t.fg_colors 0 t.len Default.fg ;
    Array.fill t.bg_colors 0 t.len Default.bg ;
    Array.fill t.z_index 0 t.len Default.z

  let render cursor frame_buffer render_buffer =
    render_buffer |> Bytevector.reset
                  |> Bytevector.append Term.Control.cursor_hide
                  |> Bytevector.append Term.Control.gohome
                  |> Priv.render_all_sections frame_buffer
                  |> Bytevector.append (Term.Control.cursor_control_string cursor)
                  |> Bytevector.append Term.Control.cursor_show
                  |> Bytevector.write Unix.stdout

  let put_block pos { Block.text ; Block.offset ; Block.len ; Block.colors } t =
    let vec_offset = v2_to_offset t.window.x pos in
    let len' = min len (t.len - vec_offset) in
    if vec_offset < t.len then
      let { Color.fg ; Color.bg } = colors in
      Array.fill t.fg_colors vec_offset len' fg ;
      Array.fill t.bg_colors vec_offset len' bg ;
      Bytes.blit_string text offset t.text vec_offset len'

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

  let init_screen cb { topleft = offset ; bottomright = size } = {
    size                = size ;
    screen_offset       = offset ;
    frame_buffer        = cb ;
  }

  let put_block screen start blk =
    let start' = start <+> screen.screen_offset in
    Framebuffer.put_block start' blk screen.frame_buffer ;
    mk_v2 (start.x + blk.Block.len) start.y

  let put_block_lines screen linebreak y_offset block_lines =
    let start = mk_v2 0 y_offset in
    let bounds = screen.size <-> start in
    let mk_line_start i = mk_v2 0 (y_offset + i) in
    let put_block_line i blks =
      blks |> List.fold_left (put_block screen) (mk_line_start i) |> ignore
    in
    block_lines
      |> (Block.get_line_breaker linebreak) bounds
      |> List.iteri put_block_line

  let put_line screen y_offset blk =
    put_block screen (mk_v2 0 y_offset) blk |> ignore
    (* put_block screen (mk_v2 0 y_offset) (Block.adjust_length screen.size.x blk) |> ignore *)
end


module Filebuffer = struct
  type t = {
      filename    : string ;
      filepath    : string ;
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
    let action () = loop (Slice.init_slice 0 32 "") ch in
    let cleanup () = close_in ch in
    try_finally action cleanup

  let from_lines file lines = (* TODO: refactor with Slice *)
    let buffer = lines in {
      filename      = file ;
      filepath      = (Sys.getcwd ()) ^ "/" ^ file ;
      buffer        = buffer ;
      buflen        = Slice.len buffer ;
    }

  let init_filebuffer file =
    file |> read_file |> from_lines file
end


module FileNavigator = struct

  let dir_ls path =
    let rec loop entries handle =
      match Unix.readdir handle with
      | s                     -> loop (Slice.append s entries) handle
      | exception End_of_file -> entries |> Slice.sort_slice String.compare
    in
    let handle = Unix.opendir path in
    let entries = loop (Slice.init_slice 0 16 "") handle in
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
module Fileview : (FileviewType with type view = text_view and type filebuffer = Filebuffer.t and type screen = Screen.t) = struct
  open Filebuffer

  type view       = text_view
  type filebuffer = Filebuffer.t
  type screen     = Screen.t

  type numbering_mode = Absolute | CursorRelative



  module LineNumberCache = struct
    let line_number_cache_t1 = Sys.time () ;;
    (* TODO: - dynmically populate cache as needed by resizing the cache array if needed *)

    let negative_offset = 200
    let hardcoded_size  = 10000 + negative_offset

    let format_n n =
      Printf.sprintf "%3d " (n - negative_offset)

    let mk_block n =
      Block.mk_block (format_n n) Config.default.colors.line_numbers

    let cache =
      Array.init hardcoded_size mk_block

    let get n =
      Array.get cache (n + negative_offset)

    let line_number_cache_t2 = Sys.time () ;;
    Printf.fprintf logs "cache %f\n" (line_number_cache_t2 -. line_number_cache_t1) ;;
  end

  type t = {
    filebuffer    : filebuffer ;
    cursor        : v2 ;       (* current position in file space: x = column index, y = row index *)
    view_start    : int ;      (* index of first row in view *)
    view_diff     : int ;      (* number of rows in the view *)
    numbering     : numbering_mode ;
    linebreaking  : Block.linebreak ;
  }

  let init_fileview view_h filebuffer = {
    filebuffer    = filebuffer ;
    cursor        = v2_zero ;
    view_start    = 0 ;
    view_diff     = view_h - 1;
    numbering     = CursorRelative ;
    linebreaking  = Block.Overflow ;
  }

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

  let is_current_char_valid t =
    t.cursor.x < current_line_len t

  let cursor t = t.cursor

  let buflen t =
    Slice.len t.filebuffer.buffer

  let view_diff t =
    t.view_diff

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
    let new_mode = match t.numbering with
    | Absolute        -> CursorRelative
    | CursorRelative  -> Absolute
    in { t with numbering = new_mode }

  let swap_linebreaking_mode t =
    let open Block in
    let new_mode = match t.linebreaking with
    | Clip      -> Overflow
    | Overflow  -> Clip
    in { t with linebreaking = new_mode }

  let recenter_view t =
    let new_start = t.cursor.y - t.view_diff / 2 in
    { t with view_start = max new_start 0 }

  let move_cursor_left2 t = {
    x = t.cursor.x |> dec |> max 0 ;
    y = t.cursor.y ;
  }

  let cursor_next_char_proto t vec2 =
    (* BUG: infinite loop on file where the matcher never return true *)
    let rec first_non_empty y =
      match y with
      | _ when y = (buflen t)   -> first_non_empty 0
      | _ when 0 = line_len t y -> first_non_empty (y + 1)
      | _                       -> y
    in
      if vec2.x + 1 < slen (current_line t)
      then { x = vec2.x + 1; y = vec2.y}
      else { x = 0; y = first_non_empty (vec2.y + 1) } (* skip empty lines *)

  let cursor_next_char t =
    cursor_next_char_proto t t.cursor

  let cursor_prev_char t =
    (* BUG: infinite loop on file where the matcher never return true *)
    let rec last_non_empty y =
      match y with
      | _ when y = -1           -> last_non_empty ((buflen t) - 1)
      | _ when 0 = line_len t y -> last_non_empty (y - 1)
      | _                       -> y
    in
      if t.cursor.x - 1 > 0
      then { x = t.cursor.x - 1 ; y = t.cursor.y }
      else
        let y' = last_non_empty (t.cursor.y - 1) in
        { x = (line_len t y') - 1 ; y = y'}

  let cursor_next_line t = {
      x = t.cursor.x ;
      y = (t.cursor.y + 1) mod (buflen t) ;
    }

  let cursor_prev_line t =
    let y' = if t.cursor.y - 1 >= 0 then t.cursor.y - 1 else (buflen t) - 1 in {
      x = t.cursor.x ;
      y = y' ;
    }

  let file_length_string t =
    (string_of_int (buflen t)) ^ "L"

  let get_line_numbering_offset t =
    t.view_start + match t.numbering with
    | Absolute        -> 1 ;
    | CursorRelative  -> -t.cursor.y

  let get_line t offset i =
    LineInfo {
      blocks = [ offset + i |> Slice.get t.filebuffer.buffer |> Block.wrap_string ]
    }

  let get_lines view_start view_max t =
    let start = t.view_start in
    let stop = min (buflen t) (start + view_max) in
    Array.init (stop - start) (get_line t start) |> Array.to_list

  let get_view maxlines t =
    TextView {
      offset        = get_line_numbering_offset t ;
      lines         = get_lines t.view_start maxlines t ;
      cursor        = t.cursor ;
      linebreaking  = t.linebreaking ;
    }

  let border_block =
    Block.mk_block " " Config.default.colors.border

  let print_default_fill screen =
    let fill_y_offset = 1 in
    mk_list (Screen.get_height screen) [border_block]
      |> Screen.put_block_lines screen Block.Overflow fill_y_offset

  let print_header t screen colors =
    let header_offset = 0 in
    (* PERF: cache this string in screen and only update when length change *)
    let header = t.filebuffer.Filebuffer.filepath
                ^ "  " ^ (file_length_string t)
                ^ "  " ^ (t |> cursor |> v2_to_string)
    in
      Screen.put_line screen header_offset (Block.mk_block header colors)

  let mk_line_number_block n =
    LineNumberCache.get n

  let prepend_line_numbers offset lines =
    let rec loop n acc =
      function
      | [] -> List.rev acc
      | LineInfo { blocks } :: t ->
          let acc' = mk_line_number_block n |> conj blocks |> List.cons border_block |> conj acc
          in
            loop (n + 1) acc' t
    in
      loop offset [] lines

  let print_file_buffer fileview screen =
    let y_offset = 1 in
    let n_lines = (Screen.get_height screen) - 1 in
    let TextView { offset ; lines ; linebreaking } = get_view n_lines fileview in
    lines |> prepend_line_numbers offset
          (* TODO: fuse selection into the block list *)
          |> Screen.put_block_lines screen linebreaking y_offset

  let draw t screen is_focused =
    let header_colors =
      if is_focused then Config.default.colors.focus_header else Config.default.colors.header
    in
    print_header t screen header_colors ;
    print_default_fill screen ; (* PERF: only put default filling when needed *)
    print_file_buffer t screen

  let get_line_len_hacky t y_offset =
    y_offset |> Slice.get t.filebuffer.buffer |> slen (* TODO: take into account '\t' characters *)

  let mk_offset_table t bounds =
    let table_len =
      min (max bounds.y t.view_diff) (t.filebuffer.Filebuffer.buflen - t.view_start)
    in
    let offset0 = -t.view_start in
    let line_offset_table = Array.make table_len offset0 in
    let rec loop i offset =
      if i < table_len
        then
          let line_len = get_line_len_hacky t (i + t.view_start) in
          let offset' = offset + (line_len / bounds.x) in
          line_offset_table.(i) <- offset ;
          loop (i + 1) offset'
        else
          line_offset_table
    in
      loop 0 offset0

  let text_space_to_screen_space line_offset_table t width { x ; y } =
    let y' = y + line_offset_table.(y - t.view_start) + (x / width) in
    let x' = x mod width in
    mk_v2 x' y'

  let cursor_relative_to_view bounds t =
    match t.linebreaking with
    | Block.Clip ->
        mk_v2 t.cursor.x (t.cursor.y - t.view_start)
    | Block.Overflow ->
        let table = mk_offset_table t bounds in
        output_string logs
          ("[ " ^ (table |> Array.map string_of_int |> Array.to_list |> String.concat" ") ^ " ]\n") ;
        flush logs ;
        text_space_to_screen_space table t bounds.x t.cursor
end


module FilebufferMovements = struct
  open Fileview

  let saturate_up length x = min (max (length - 1) 0) x

  (* TODO: refactor to remove the use of adjust cursor *)
  let rec cursor_move_while u f t =
    if f t
    then t |> adjust_cursor (u t) |> cursor_move_while u f
    else t

  let move_n_up n t = {
    x = (cursor t).x ;
    y = (cursor t).y |> fun x -> x - n |> max 0 ;
  }

  let move_n_down n t = {
    x = (cursor t).x ;
    y = (cursor t).y |> (+) n |> saturate_up (buflen t) ;
  }

  (* move_* commands saturates at 0 and end of line *)
  let move_cursor_left t = {
    x = (cursor t).x |> dec |> max 0 ;
    y = (cursor t).y ;
  }

  let move_cursor_right t = {
    x = (cursor t).x |> inc |> saturate_up (t |> current_line |> slen) ;
    y = (cursor t).y ;
  }

  let move_cursor_up   = move_n_up 1 ;;
  let move_cursor_down = move_n_down 1 ;;
  let move_page_up   t = move_n_up ((view_diff t) + 1) t ;;
  let move_page_down t = move_n_down ((view_diff t) + 1) t ;;

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

  let move_next_space t =
    t |> cursor_move_while cursor_next_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_next_char (current_char >> is_space)
      |> cursor_move_while cursor_next_char (current_char >> is_space >> not)
      |> cursor

  (* BUG: this always skips a single leading space at beginning of a line, but does not skip more than one leading space *)
  let move_prev_space t =
    t |> cursor_move_while cursor_prev_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_prev_char (current_char >> is_space)
      |> cursor_move_while cursor_prev_char (current_char >> is_space >> not)
      |> cursor

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

  let move_line_start t = { x = 0 ; y = (cursor t).y } ;;
  let move_line_end t   = { x = max 0 ((t |> current_line |> slen) - 1) ; y = (cursor t).y } ;;
  let move_file_start t = { x = (cursor t).x ; y = 0 } ;;
  let move_file_end t   = { x = (cursor t).x ; y = (buflen t) - 1 } ;;
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
  }

  let init_stats () = {
    gc_stats            = Gc.quick_stat () ;
    last_major_words    = 0. ;
    last_minor_words    = 0. ;
    timestamp           = Sys.time () ;
    last_input_duration = 0. ;
    last_cycle_duration = 0. ;
  }

  let update_stats now input_duration stats = {
    gc_stats            = Gc.quick_stat () ;
    last_major_words    = stats.gc_stats.Gc.major_words ;
    last_minor_words    = stats.gc_stats.Gc.minor_words ;
    timestamp           = now ;
    last_input_duration = input_duration ;
    last_cycle_duration = now -. stats.timestamp ;
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
end


module Tileset = struct

  type op = Resize of rect
          | FileviewOp of (Fileview.t -> Fileview.t)
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
    focus_index   : int ;
    fileviews     : Fileview.t Slice.t ;
  }

  let mk_tileset term_size fileviews = {
    screen_size   = term_size ;
    screen_config = ScreenConfiguration.Configs.zero ;
    focus_index   = 0 ;
    fileviews     = fileviews ;
  }

  let get_cursor focused_screen focused_fileview =
    Fileview.cursor_relative_to_view (Screen.get_size focused_screen) focused_fileview
                  |> (<+>) (Screen.get_offset focused_screen)
                  |> (<+>) (mk_v2 5 1)  (* +5 for line numbers, +1 for header *)

  let make_screens t frame_buffer =
    t.screen_config
      |> ScreenConfiguration.mk_view_ports t.screen_size (Slice.len t.fileviews)
      |> Slice.map (Screen.init_screen frame_buffer)

  let draw_fileviews t frame_buffer =
    let screens = make_screens t frame_buffer in
    let n_screens = Slice.len screens in
    if n_screens = 1
      then (
        let main_screen = Slice.get screens 0 in
        let focused_fileview = Slice.get t.fileviews t.focus_index in
        Fileview.draw focused_fileview main_screen true ;
        get_cursor main_screen focused_fileview
      ) else (
        for i = 0 to n_screens - 1 do
          Fileview.draw (Slice.get t.fileviews i) (Slice.get screens i) (i = t.focus_index)
        done ;
        get_cursor (Slice.get screens t.focus_index) (Slice.get t.fileviews t.focus_index)
      )

  let apply_op t =
    function
      | Resize screen_size ->
          { t with screen_size = screen_size ; }
      | FileviewOp fileview_op ->
          let t' = { t with fileviews = Slice.clone t.fileviews } in
          t'.focus_index
            |> Slice.get t'.fileviews
            |> fileview_op
            |> Slice.set t'.fileviews t'.focus_index ;
          t'

      | ScreenLayoutCycleNext ->
          { t with screen_config = ScreenConfiguration.cycle_config_layout_next t.screen_config }
      | ScreenLayoutCyclePrev ->
          { t with screen_config = ScreenConfiguration.cycle_config_layout_prev t.screen_config }
      | ScreenLayoutFlip ->
          { t with screen_config = ScreenConfiguration.flip_config_orientation t.screen_config }
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
          { t with fileviews = fileviews' ; focus_index = 0 }
      | RotateViewsLeft ->
          let last_index = (Slice.len t.fileviews) - 1 in
          let fileviews' = Slice.clone t.fileviews in
          Slice.get t.fileviews last_index |> Slice.set fileviews' 0 ;
          Slice.copy (Slice.slice_right 1 fileviews') t.fileviews ;
          { t with fileviews = fileviews' }
      | RotateViewsRight ->
          let last_index = (Slice.len t.fileviews) - 1 in
          let fileviews' = Slice.clone t.fileviews in
          Slice.get t.fileviews last_index |> Slice.set fileviews' 0 ;
          Slice.copy (Slice.slice_right 1 fileviews') t.fileviews ;
          { t with fileviews = fileviews' }
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
               | View of (Fileview.t -> Fileview.t)
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
    stats           : Stats.t
  }

  let main_screen_dimensions term_dim =
    mk_rect 0 0 term_dim.x (term_dim.y - 2)

  let status_screen_dimensions term_dim =
    mk_rect 0 (term_dim.y - 2) term_dim.x 2

  let mk_status_screen frame_buffer term_dim =
    term_dim |> status_screen_dimensions |> Screen.init_screen frame_buffer

  let mk_main_screen frame_buffer term_dim =
    term_dim |> main_screen_dimensions |> Screen.init_screen frame_buffer

  let mk_window_size_descr { x = w ; y = h } =
    "(" ^ (string_of_int w) ^ " x " ^ (string_of_int h) ^ ")"

  let test_mk_filebuffers file = [|
      Filebuffer.init_filebuffer file ;
      Filebuffer.init_filebuffer "./iter.ml" ;
      Filebuffer.init_filebuffer "./ioctl.c" ;
      (* Filebuffer.init_filebuffer "./Makefile" ; (* FIX tabs *) *)
      (* FileNavigator.dir_to_filebuffer (Sys.getcwd ()) ; *)
    |] |> Slice.wrap_array

  let test_mk_fileviews term_dim =
    Slice.map (Fileview.init_fileview (term_dim.y - 3))

  let init_editor file =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_frame_buffer term_dim in
    let filebuffers = test_mk_filebuffers file in
    let fileviews = test_mk_fileviews term_dim filebuffers
    in {
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      render_buffer   = Bytevector.init_bytevector 0x1000 ;
      frame_buffer    = frame_buffer ;
      running         = true ;
      status_screen   = mk_status_screen frame_buffer term_dim ;

      tileset         = Tileset.mk_tileset (main_screen_dimensions term_dim) fileviews ;
      filebuffers     = filebuffers ;

      user_input      = "" ;
      pending_input   = None;

      stats           = Stats.init_stats () ;
    }

  let resize_editor editor =
    let term_dim = Term.get_terminal_dimensions () in
    let frame_buffer = Framebuffer.init_frame_buffer term_dim
    in {
      editor with
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      frame_buffer    = frame_buffer ;
      status_screen   = mk_status_screen frame_buffer term_dim ;
      tileset         = Tileset.apply_op editor.tileset (Tileset.Resize (main_screen_dimensions term_dim)) ;
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
    | View fn ->
        let fileview_op = Tileset.FileviewOp fn in {
          editor with
          tileset = Tileset.apply_op editor.tileset fileview_op ;
        }
      (* cannot happen ?? *)
    | Pending ((Digit n) as d) ->
        queue_pending_command editor d

  let apply_command_with_repetition n command editor =
    match command with
    | Move fn ->
      let rec loop n fb =
        if (n > 0)
          then loop (n - 1) (Fileview.apply_movement fn fb)
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
      Screen.put_line editor.status_screen 0 (Block.mk_block status_text1 Config.default.colors.status) ;
      Screen.put_line editor.status_screen 1 (Block.mk_block status_text2 Config.default.colors.user_input)

  let refresh_screen editor =
    (* PERF: only clear rectangles per subscreen *)
    Framebuffer.clear editor.frame_buffer ;
    show_status editor ;
    let cursor_position = Tileset.draw_fileviews editor.tileset editor.frame_buffer in
    Framebuffer.render cursor_position editor.frame_buffer editor.render_buffer ;
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
    | Keys.Ctrl_z       -> View Fileview.recenter_view
    | Keys.Space        -> View Fileview.recenter_view
    | Keys.Ctrl_d       -> Move FilebufferMovements.move_page_down
    | Keys.Ctrl_j       -> Move FilebufferMovements.move_next_paragraph
    | Keys.Ctrl_k       -> Move FilebufferMovements.move_prev_paragraph
    | Keys.Ctrl_u       -> Move FilebufferMovements.move_page_up
    | Keys.Alt_k        -> Move FilebufferMovements.move_file_start
    | Keys.Alt_j        -> Move FilebufferMovements.move_file_end
    | Keys.Alt_l        -> Move FilebufferMovements.move_line_end
    | Keys.Alt_h        -> Move FilebufferMovements.move_line_start
    | Keys.ArrowUp      -> Move FilebufferMovements.move_cursor_up
    | Keys.ArrowDown    -> Move FilebufferMovements.move_cursor_down
    | Keys.ArrowRight   -> Move FilebufferMovements.move_cursor_right
    | Keys.ArrowLeft    -> Move FilebufferMovements.move_cursor_left
    | Keys.Lower_k      -> Move FilebufferMovements.move_cursor_up
    | Keys.Lower_j      -> Move FilebufferMovements.move_cursor_down
    | Keys.Lower_l      -> Move FilebufferMovements.move_cursor_right
    | Keys.Lower_h      -> Move FilebufferMovements.move_cursor_left
    | Keys.Lower_w      -> Move FilebufferMovements.move_next_word
    | Keys.Lower_b      -> Move FilebufferMovements.move_prev_word
    | Keys.Upper_w      -> Move FilebufferMovements.move_next_space
    | Keys.Upper_b      -> Move FilebufferMovements.move_prev_space
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

    | Keys.EINTR        -> Resize

  let process_command editor =
    match editor.pending_input with
    | None          -> apply_command
    | Number digits -> apply_command_with_repetition (max 1 (dequeue_digits digits))

  let process_key editor = key_to_command >> (process_command editor)

  (* TODO: replace by a proper history of previous inputs *)
  let make_user_input key editor =
    let new_user_input = (pending_command_to_string editor.pending_input)
                       ^  key.Keys.repr
                       ^ " " ^ editor.user_input
    in {
      editor with
      (* TODO: truncate_string should not be needed anymore *)
      user_input = truncate_string editor.term_dim.x new_user_input ;
    }

  let update_stats input_duration editor =
    let now = Sys.time ()
    in {
      editor with
        stats = Stats.update_stats now input_duration editor.stats
    }

  let process_events editor =
    let before_input = Sys.time () in
    let key = () |> Keys.next_key in
    let after_input = Sys.time () in
      editor |> process_key editor key.Keys.symbol
             |> make_user_input key
             |> update_stats (after_input -. before_input)

  let rec loop editor =
    if editor.running then
      editor |> refresh_screen |> process_events |> loop

  let run_editor_loop editor () = loop editor

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

type program_flags = {
  mutable resize_event_pending : bool ;
}

let global_flags : program_flags = {
  resize_event_pending = false ;
}

let log_sigwinch sig_n =
  (* resize_event_pending is actually not read anywhere. When SIGWINCH is handled, the keyboard function will get
   * interrupted. The EINTR interrupt codepath there will trigger the resizing *)
  global_flags.resize_event_pending <- true

let sigwinch = 28 (* That's for OSX *)

let () =
  Sys.Signal_handle log_sigwinch |> Sys.set_signal sigwinch ;
  (* Slice.test () ; *)
  (* ScreenConfiguration.test () ; *)
  Ciseau.main () ;
  close_out logs
