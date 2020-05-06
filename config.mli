(* By default, options are loaded from config files at and ././ciseaurc (higher precedence).
 * Config file paths can also be defined as as the env variable CISEAU_CONFIGS (as a ':' separated list).
 *)

(* An option value is simply a stateful getter (with internal caching) *)
type 'a option_t = unit -> 'a

(* Load config from path. Can overwrites existing options *)
val load            : string -> unit
(* Reload all known configuration paths *)
val reload          : unit -> unit
(* Generate a config file for currently loaded options *)
val generate_config : ?path:string -> unit -> unit

(* Defining new options *)
val define_option   : name:string -> parser:(string list -> 'a) -> serializer:('a -> string list) -> default:'a -> 'a option_t
val int_option      : int -> string -> int option_t
val bool_option     : bool -> string -> bool option_t
val string_option   : string -> string -> string option_t
