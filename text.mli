type text

val load : string -> (text, string) result
val save : string -> text -> (unit, string) result (* overrides any file with the same path *)

val to_strings : text -> string array

val text_test : unit -> unit
