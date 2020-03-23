(* Identity function *)
val id                        : 'a-> 'a
(* Constant function *)
val const                     : 'a -> 'b -> 'a
(* Flip input types of a 2-arity function *)
val flip                      : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(* Compose two functions *)
val (>>)                      : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(* Compose two functions *)
val (>>>)                      : ('a -> 'b -> 'c) -> ('c -> 'd) -> 'a -> 'b -> 'd
(* Another 2-functions composer *)
val psi                       : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c
(* Inverse a predicate *)
val neg                       : ('a -> bool) -> 'a -> bool
(* Apply a 2-tupple to a function of 2 arguments *)
val apply_tup2                : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

(* Int ref increment/decrement *)
val (+=)                      : int ref -> int -> unit
val (-=)                      : int ref -> int -> unit

(* Float ref increment/decrement *)
val (+=.)                     : float ref -> float -> unit
val (-=.)                     : float ref -> float -> unit

(* Errors and asserts utililities *)
val fail                      : string -> 'a
val assert_that               : ?msg:string -> bool -> unit

(* char utilities *)
val is_space                  : char -> bool
val is_letter                 : char -> bool
val is_digit                  : char -> bool
val is_alphanum               : char -> bool
val is_printable              : char -> bool

(* print utilities *)
val output_int                : out_channel -> int -> unit
val output_float              : out_channel -> float -> unit
val print_stringln            : string -> unit
val string_of_char            : char -> string
val list_to_string            : ('a -> string) -> 'a list -> string
val array_to_string           : ('a -> string) -> 'a array -> string

(* Convenient aliases *)
val alen                      : 'a array -> int
val blen                      : Bytes.t -> int
val slen                      : string -> int

(* String utility functions *)
val string_compare_fast       : string -> int -> string -> int -> int
val string_starts_with        : string -> string -> bool
val string_is_substring       : ?ignore_case:bool -> string -> string -> bool
val string_first              : string -> char
val string_last               : string -> char
(* drop 'n' first characters *)
val string_drop               : int -> string -> string
(* cut 'n' last characters *)
val string_cut                : int -> string -> string

(* Combinators for options *)
module Options : sig
  val some                    : 'a -> 'a option
  val fmap                    : ('a -> 'b option) -> 'a option -> 'b option
  val map                     : ('a -> 'b) -> 'a option -> 'b option
  val get_or                  : 'a -> 'a option -> 'a
end

(* Wrappers around common Array/String/Bytes operations to get useful backtraces *)
(* TODO: add argument names to the fill and blit functions, or add overloads with argument names *)
module Arrays : sig
  (* Useful for writing loop conditions *)
  val astop                   : 'a array -> int
  val string_at               : string -> int -> char
  val array_get               : 'a array -> int -> 'a
  val array_set               : 'a array -> int -> 'a -> unit
  val array_fill              : 'a array -> int -> int -> 'a -> unit
  val array_blit              : 'a array -> int -> 'a array -> int -> int -> unit
  val bytes_blit              : Bytes.t -> int -> Bytes.t -> int -> int -> unit
  val bytes_blit_string       : string -> int -> Bytes.t -> int -> int -> unit
  val array_rev               : 'a array -> unit
  val array_append            : 'a array -> 'a -> 'a array
  val array_swap              : 'a array -> int -> int -> unit
  (* Linear search returning the first index for which the predicate returns true *)
  val array_find              : ('a -> bool) -> 'a array -> int
  val array_unsafe_alloc      : int -> 'a array
  (* Expend the given array to the requested length, filling the tail with the provided element *)
  val array_extend            : 'a -> 'a array -> int -> 'a array
  (* TODO: make len argument optional *)
  val subarray_insertion_sort : ('a -> 'a -> int) -> 'a array -> ?start:int -> len:int -> unit
  val subarray_sort           : ('a -> 'a -> int) -> 'a array -> ?start:int -> len:int -> unit
  val assert_array_sorted     : ('a -> 'a -> int) -> 'a array -> unit
end

(* Wraps a vanilla array with a cursor to provide a convenient append operation. Used by value. *)
module Arraybuffer : sig
  type 'a t
  val len                     : 'a t -> int
  val get                     : 'a t -> int -> 'a
  val mk_empty_arraybuffer    : 'a -> 'a t
  val mk_arraybuffer          : int -> 'a -> 'a t
  val to_array                : 'a t -> 'a array
  val reserve                 : 'a t -> int -> unit
  val append                  : 'a t -> 'a -> unit
  val del                     : 'a t -> int -> unit
  val sort                    : 'a t -> ('a -> 'a -> int) -> unit
  (* Insert content of the given array in order. Assumes this Arraybuffer.t and given array are both sorted. *)
  val ordered_insert          : 'a t -> ('a -> 'a -> int) -> 'a array -> unit
end

(* Returns an array containing the keys in the given Hashtbl.t *)
val keys : ('a, 'b) Hashtbl.t -> 'a array

module Vec : sig
  type vec2
  val x                       : vec2 -> int
  val y                       : vec2 -> int
  val mk_v2                   : int -> int -> vec2
  val zero                    : vec2
  val add                     : vec2 -> vec2 -> vec2
  val sub                     : vec2 -> vec2 -> vec2
  val area                    : vec2 -> int
  val (+)                     : vec2 -> vec2 -> vec2
  val (-)                     : vec2 -> vec2 -> vec2
  val ( * )                   : int -> vec2 -> vec2
  (* Check if second vec2 argument is inside the implicit rectangle woth topleft (0,0)
   * and first vec2 argument as bottomright corner. *)
  val is_v2_inside            : vec2 -> vec2 -> bool
  val is_v2_outside           : vec2 -> vec2 -> bool
  val assert_v2_inside        : vec2 -> vec2 -> unit
  val v2_string               : vec2 -> string
end

module Rec : sig
  type rec2 = {
    x0  : int ;
    y0  : int ;
    x1  : int ;
    y1  : int ;
    w   : int ;
    h   : int ;
  }
  val x0                      : rec2 -> int
  val y0                      : rec2 -> int
  val x1                      : rec2 -> int
  val y1                      : rec2 -> int
  val mk_rect                 : int -> int -> int -> int -> rec2 (* TODO: use named parameters *)
  val rect_size               : rec2 -> Vec.vec2
  val rect_offset             : rec2 -> Vec.vec2
  val rect_end                : rec2 -> Vec.vec2
  val rect_x                  : rec2 -> int
  val rect_y                  : rec2 -> int
  val rect_x_end              : rec2 -> int
  val rect_y_end              : rec2 -> int
  val rect_w                  : rec2 -> int
  val rect_h                  : rec2 -> int
  val rect_mv                 : Vec.vec2 -> rec2 -> rec2
  val rect_to_string          : rec2 -> string
  val assert_rect_inside      : Vec.vec2 -> rec2 -> unit
end
