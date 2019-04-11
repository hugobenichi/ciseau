(* Identity function *)
val id                        : 'a-> 'a
(* Constant function *)
val const                     : 'a -> 'b -> 'a
(* Flip input types of a 2-arity function *)
val flip                      : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c 
(* Compose two functions *)
val (>>)                      : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(* Another 2-functions composer *)
val psi                       : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c

(* Int ref increment/decrement *)
val (+=) : int ref -> int -> unit
val (-=) : int ref -> int -> unit

(* Errors and asserts utililities *)
val fail                      : string -> 'a
val assert_that               : ?msg:string -> bool -> unit

(* char utilities *)
val is_space                  : char -> bool
val is_letter                 : char -> bool
val is_digit                  : char -> bool
val is_alphanum               : char -> bool
val is_printable              : char -> bool
val output_int                : out_channel -> int -> unit
val output_float              : out_channel -> float -> unit
val string_of_char            : char -> string
val list_to_string            : ('a -> string) -> 'a list -> string
val array_to_string           : ('a -> string) -> 'a array -> string

(* Combinators for options *)
module Options : sig
  val some                    : 'a -> 'a option
  val fmap                    : ('a -> 'b option) -> 'a option -> 'b option
  val map                     : ('a -> 'b) -> 'a option -> 'b option
  val get_or                  : 'a -> 'a option -> 'a
end


(* Wrappers around common Array/String/Bytes operations to get useful backtraces *)
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
  val array_find              : ('a -> bool) -> 'a array -> int
  val array_unsafe_alloc      : int -> 'a array
end

(* Wraps a vanilla array with a cursor to provide a convenient append operation. Used by value. *)
module Arraybuffer : sig
  type 'a t
  val len             : 'a t -> int
  val get             : 'a t -> int -> 'a
  val empty           : 'a -> 'a t
  val reserve         : int -> 'a -> 'a t
  val to_array        : 'a t -> 'a array
  val append          : 'a t -> 'a -> unit
  val append_and_then : 'a t -> 'a -> 'a t
end

(* Returns an array containing the keys in the given Hashtbl.t *)
val keys : ('a, 'b) Hashtbl.t -> 'a array
