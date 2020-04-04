let kDEFAULT_OPTION_PATH  = "$HOME/.ciseaurc"
let kLOCAL_OPTION_PATH    = "./.ciseaurc"

type 'a option_t = {
  name      : string ;
  parser    : string -> 'a ;
  default   : 'a ;
}

(* Global constant that stores raw key values read from config files *)
let sKeyvals : (string, string) Hashtbl.t = Hashtbl.create 10

let load_options path = None

let clear_options () = Hashtbl.clear sKeyvals

let define_option ~name:name ~parser:parser ~default:default = { name ; parser ; default }

let get_option opt =
  try
    opt.name |> Hashtbl.find sKeyvals |> opt.parser
  with
    _ -> opt.default

let has_option { name } = Hashtbl.mem sKeyvals name
