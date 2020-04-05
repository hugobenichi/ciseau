open Util

let kDEFAULT_OPTION_PATH  = "$HOME/.ciseaurc"
let kLOCAL_OPTION_PATH    = "./.ciseaurc"

(* TODO: change val from string to string list *)
type 'a option_t = {
  name        : string ;
  parser      : string -> 'a ;
  serializer  : 'a -> string ;
  default     : 'a ;
}

(* Global constant that stores raw key values read from config files *)
let sKeyvals : (string, string) Hashtbl.t = Hashtbl.create 10

let process_lines lines =
  for i = 0 to Arrays.astop lines do
    Arrays.array_get lines i
      |> string_split is_space
      |> function
          | [] -> ()
          | k :: [] -> Hashtbl.replace sKeyvals k ""
          | k :: v :: _ -> Hashtbl.replace sKeyvals k v
  done

let load_options path =
  path |> read_file |> process_lines ;
  None

(* TODO: implement *)
let generate_default_config () =
  ()

let generate_current_config = generate_default_config

let clear_options () = Hashtbl.clear sKeyvals

let define_option ~name:name ~parser:parser ~serializer:serializer ~default:default = {
  name ;
  parser ;
  serializer ;
  default ;
}
let int_option      default name = define_option ~name:name ~parser:int_of_string   ~serializer:string_of_int   ~default:default
let bool_option     default name = define_option ~name:name ~parser:bool_of_string  ~serializer:string_of_bool  ~default:default
let string_option   default name = define_option ~name:name ~parser:id              ~serializer:id              ~default:default

let get opt =
  try
    opt.name |> Hashtbl.find sKeyvals |> opt.parser
  with
    _ -> opt.default

let has { name } = Hashtbl.mem sKeyvals name

let _ =
  ignore (load_options kDEFAULT_OPTION_PATH) ;
  ignore (load_options kLOCAL_OPTION_PATH) ;
  ()
