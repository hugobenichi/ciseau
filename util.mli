(* char utilities *)
val is_space                  : char -> bool
val is_letter                 : char -> bool
val is_digit                  : char -> bool
val is_alphanum               : char -> bool
val is_printable              : char -> bool

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

(* Combinators for options *)
module Options : sig
  val some                    : 'a -> 'a option
  val fmap                    : ('a -> 'b option) -> 'a option -> 'b option
  val map                     : ('a -> 'b) -> 'a option -> 'b option
  val get_or                  : 'a -> 'a option -> 'a
end
