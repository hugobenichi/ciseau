(* Types of Keys generated when defining an option for parametric type 'a *)
type 'a option_t

(* Load options from given directory path, overwriting any previously loaded options,
 * Return error message or None *)
val load_options    : string -> string option
val clear_options   : unit -> unit
(* Define a new option *)
val define_option   : name:string -> parser:(string -> 'a) -> default:'a -> 'a option_t
val get_option      : 'a option_t -> 'a
val has_option      : 'a option_t -> bool
