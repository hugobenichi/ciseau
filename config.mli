(* Types of Keys generated when defining an option for parametric type 'a *)

(* An option value is simply a stateful getter *)
type 'a option_t = unit -> 'a

(* Load options from given directory path, overwriting any previously loaded options,
 * Return error message or None *)
val load            : string -> unit
val reload          : unit -> unit
val generate_config : ?path:string -> unit -> unit
val clear_options   : unit -> unit

(* Defining new options *)
val define_option   : name:string -> parser:(string list -> 'a) -> serializer:('a -> string list) -> default:'a -> 'a option_t
val int_option      : int -> string -> int option_t
val bool_option     : bool -> string -> bool option_t
val string_option   : string -> string -> string option_t
