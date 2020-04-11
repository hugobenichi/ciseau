(* Types of Keys generated when defining an option for parametric type 'a *)
(* TODO: remove this and instead directly return a (() -> 'a) function *)
type 'a option_t

(* Accessing current option from loaded config files *)
val get             : 'a option_t -> 'a
val has             : 'a option_t -> bool

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
