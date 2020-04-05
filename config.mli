(* Types of Keys generated when defining an option for parametric type 'a *)
type 'a option_t

(* Accessing current option from loaded config files *)
val get             : 'a option_t -> 'a
val has             : 'a option_t -> bool

(* Load options from given directory path, overwriting any previously loaded options,
 * Return error message or None *)
val load_options    : string -> string option
val generate_config : unit -> unit
val clear_options   : unit -> unit
val reload          : unit -> unit

(* Defining new options *)
(* TODO: change parser to string list -> 'a *)
val define_option   : name:string -> parser:(string -> 'a) -> serializer:('a -> string) -> default:'a -> 'a option_t
val int_option      : int -> string -> int option_t
val bool_option     : bool -> string -> bool option_t
val string_option   : string -> string -> string option_t
