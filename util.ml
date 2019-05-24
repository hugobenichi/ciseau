let id x          = x
let const a b     = a
let flip f a b    = f b a
let psi f g a1 a2 = f (g a1) (g a2)
let (>>) f g x    = g (f x)
let neg f x       = not (f x)

let (+=) r x = (r := !r + x)
let (-=) r x = (r := !r - x)
let (+=.) r x = (r := !r +. x)
let (-=.) r x = (r := !r -. x)

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

let is_space      chr         = (chr = ' ') || (chr = '\t') || (chr = '\r') || (chr = '\n')
let is_letter     chr         = (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z'))
let is_digit      chr         = ('0' <= chr) && (chr <= '9')
let is_alphanum   chr         = (is_digit chr) || (is_letter chr)
let is_printable  chr         = (' ' <= chr) && (chr <= '~')
let output_int    f           = string_of_int >> output_string f
let output_float  f           = string_of_float >> output_string f
let print_stringln s          = print_string s ; print_newline ()
let string_of_char c          = String.make 1 c
let joiner fold sep fn        = fold (fun a x -> a ^ sep ^ (fn x)) ""
let list_to_string fn         = joiner List.fold_left " :: " fn
let array_to_string fn ary    = "[| " ^ (joiner Array.fold_left " ; " fn ary) ^ " |]"

let alen = Array.length
let blen = Bytes.length
let slen = String.length

external string_compare_fast          : string -> int -> string -> int -> int             = "string_compare_fast"
external string_starts_with           : string -> string -> bool                          = "string_starts_with"
external string_is_substring_native   : bool -> string -> string -> bool                  = "string_is_substring"
let string_is_substring ?ignore_case:(ic=false) fragment text = string_is_substring_native ic fragment text
let string_first s = String.get s 0
let string_last s = String.get s ((slen s) - 1)
let string_drop n s = String.sub s n ((slen s) - n)
let string_cut n s = String.sub s 0 ((slen s) - n)

module Options = struct
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

module Arrays = struct

  (* TODO: move to functor parameter ? *)
  let kBOUNDCHECK = true

  let astop ary = (alen ary) - 1

  let check_bounds src_offset src_len =
    if kBOUNDCHECK then
    if src_offset < 0 || src_len <= src_offset
      then fail (Printf.sprintf "set/get out of bounds: offset %d for range 0..%d" src_offset src_len)

  let check_range range_len src_offset src_len =
    if kBOUNDCHECK then
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

  let array_append a_in e =
    let l = alen a_in in
    let a_out = Array.make (l + 1) e in
    array_blit a_in 0 a_out 0 l ;
    a_out

  let array_swap a i j =
    check_bounds i (alen a) ;
    check_bounds j (alen a) ;
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

  let array_unsafe_alloc n =
    Array.make n "" |> Obj.magic

  let array_extend zero a newlen =
    if newlen <= alen a
    then a
    else let a' = Array.make newlen zero in
         Array.blit a 0 a' 0 (alen a) ;
         a'

  let inplace_sorted_merge compare_fn array1 len1 array2 len2 =
    assert_that (len1 + len2 <= alen array1) ;
    let rec sorted_insert compare_fn array1 array2 out =
      function
        | (0, 0)      ->  () (* nothing left to do *)
        | (_, 0)      ->  () (* second array exhausted *)
        | (0, in2)    ->  Array.blit array2 0 array1 0 in2 (* first array exhausted *)
        | (in1, in2) when compare_fn array1.(in1) array2.(in2) < 0
                      ->  array1.(out) <- array2.(in2) ;
                          sorted_insert compare_fn array1 array2 (out - 1) (in1, in2 - 1)
        | (in1, in2)  ->  array1.(out) <- array1.(in1) ;
                          sorted_insert compare_fn array1 array2 (out - 1) (in1 - 1, in2)
    in
      sorted_insert compare_fn array1 array2 (len1 + len2 - 1) (len1 - 1, len2 - 1)

  let rec array_sorted_merge compare_fn out offset_out array1 in1 len1 array2 in2 len2 =
    match (len1, len2) with
      | (0, 0)  ->  ()
      | (_, 0)  ->  Array.blit array1 in1 out offset_out len1
      | (0, _)  ->  Array.blit array2 in2 out offset_out len2
      | (_, _) when compare_fn array1.(in1) array2.(in2) < 0
                ->  out.(offset_out) <- array1.(in1) ;
                    array_sorted_merge compare_fn out (offset_out + 1) array1 (in1 + 1) (len1 - 1) array2 in2 len2
      | (_, _)  ->  out.(offset_out) <- array2.(in2) ;
                    array_sorted_merge compare_fn out (offset_out + 1) array1 in1 len1 array2 (in2 + 1) (len2 - 1)

  let subarray_insertion_sort compare_fn a start stop =
    let rec find_insert compare_fn a start i j =
      if start < j && compare_fn a.(j) a.(i) > 0 then find_insert compare_fn a start i (j - 1) else j
    in
    for i = start + 1 to stop do
      let j = find_insert compare_fn a start i (i - 1) in
      let t = a.(j) in
      Array.blit a j a (j + 1) (i - j) ;
      a.(i) <- t
    done

  let kInsertionThreshold = 100

  let subarray_sort compare_fn a start stop =
    let rec recursive_merge_sort buffer compare_fn a start stop =
      if stop - start <  kInsertionThreshold
        then subarray_insertion_sort compare_fn a start stop
        else begin
          let middle = (stop + start) / 2 in
          recursive_merge_sort buffer compare_fn a start middle ;
          recursive_merge_sort buffer compare_fn a (middle + 1) stop;
          array_sorted_merge compare_fn buffer 0 a start (middle - start + 1) a (middle + 1) (stop - middle) ;
          Array.blit buffer 0 a start (stop - start + 1)
        end
    in
      recursive_merge_sort (Array.copy a) compare_fn a start stop
end

module Arraybuffer = struct

  type 'a t = {
    mutable data  : 'a array ;
    mutable next  : int ;
    zero          : 'a ;
  }

  let len { next } = next

  let get { data } index = Arrays.array_get data index

  let empty zero_elem = {
    data = [||] ;
    next = 0 ;
    zero = zero_elem ;
  }

  let reserve n zero_elem = {
    data = Array.make n zero_elem ;
    next = 0 ;
    zero = zero_elem ;
  }

  let to_array { data ; next } =
    Array.sub data 0 next

  let append b e =
    if alen b.data <= b.next then
      b.data <- Arrays.array_extend b.zero b.data (max 10 (2 * b.next)) ;
    Arrays.array_set b.data b.next e ;
    b.next <- b.next + 1

  let del buffer i =
    Arrays.check_bounds i buffer.next ;
    buffer.next <- buffer.next - 1 ;
    Arrays.array_swap buffer.data i buffer.next

  let merge_insert compare_fn b a len =
    b.data <- Arrays.array_extend b.zero b.data (b.next + len) ;
    Arrays.inplace_sorted_merge compare_fn b.data b.next a len
end

(* Returns an array containing the keys in the given Hashtbl.t *)
let keys tbl =
  let len = Hashtbl.length tbl in
  if len = 0
    then [||]
    else
      let keys = Arrays.array_unsafe_alloc len in
      let i = ref 0 in
      Hashtbl.iter (fun k v -> Arrays.array_set keys !i k ; incr i) tbl ;
      keys

module Vec = struct

  type vec2 = {
    x : int ;
    y : int ;
  }

  let mk_v2 x y     = { x ; y }
  let v2_zero       = mk_v2 0 0
  let v2_add t1 t2  = mk_v2 (t1.x + t2.x) (t1.y + t2.y)
  let v2_sub t1 t2  = mk_v2 (t1.x - t2.x) (t1.y - t2.y)

  let is_v2_inside { x = xlim ; y = ylim } { x ; y } =
    (0 <= x) && (0 <= y) && (x <= xlim) && (y <= ylim)

  let is_v2_outside { x = xlim ; y = ylim } { x ; y } =
    (x < 0) || (y < 0) || (x > xlim) || (y > ylim)

  let assert_v2_inside box_v2 v2 =
    if is_v2_outside box_v2 v2
      then fail (Printf.sprintf "(%d,%d) out of bound of (%d,%d)" v2.x v2.y box_v2.x box_v2.y)
end

module Rec = struct

  type rec2 = {
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

  let rect_size   { w ; h}      = Vec.mk_v2 w h
  let rect_offset { x0 ; y0 }   = Vec.mk_v2 x0 y0
  let rect_end    { x1 ; y1 }   = Vec.mk_v2 x1 y1
  let rect_x      { x0 }        = x0
  let rect_y      { y0 }        = y0
  let rect_x_end  { x1 }        = x1
  let rect_y_end  { y1 }        = y1
  let rect_w      { w }         = w
  let rect_h      { h }         = h

  let rect_mv { Vec.x ; Vec.y } {x0 ; y0 ; x1 ; y1 } =
    mk_rect (x + x0) (y + y0) (x + x1) (y + y1)

  let assert_rect_inside bounds r =
    r |> rect_offset  |> Vec.assert_v2_inside bounds ;
    r |> rect_end     |> Vec.assert_v2_inside bounds

  let rect_to_string { x0 ; y0 ; w ; h } =
    Printf.sprintf "(%d,%d)x%dx%d" x0 y0 w h
end
