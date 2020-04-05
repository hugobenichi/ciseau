open Util

(* TODO: read these from environment variables first *)
let kDEFAULT_OPTION_PATH  = "$HOME/.ciseaurc"
let kLOCAL_OPTION_PATH    = "./.ciseaurc"

(* TODO: change val from string to string list *)
type 'a option_t = {
  name                  : string ;
  parser                : string -> 'a ;
  serializer            : 'a -> string ;
  default               : 'a ;
  mutable cached_value  : 'a ;
  mutable cached_gen    : int ;
}


(* Global hashtable that stores raw key values read from config files *)
let sKeyvals : (string, string) Hashtbl.t = Hashtbl.create 10
(* Global hashtable that stores all defined options *)
let sOptions : (string, unit option_t) Hashtbl.t = Hashtbl.create 10
(* TODO: instead of using cache generation, directly mutate cached values in all options *)
let sCacheGeneration = ref 0

let get opt =
  if !sCacheGeneration <= opt.cached_gen
    then opt.cached_value
    else let v =
      try
        opt.name |> Hashtbl.find sKeyvals |> opt.parser
      with
        _ -> opt.default
    in
      opt.cached_value <- v ;
      opt.cached_gen <- !sCacheGeneration ;
      v

let has { name } = Hashtbl.mem sKeyvals name

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
  incr sCacheGeneration ;
  path |> read_file |> process_lines ;
  None

let clear_options () = Hashtbl.clear sKeyvals

let define_option ~name:name ~parser:parser ~serializer:serializer ~default:default =
  let opt = { name ; parser ; serializer ; default ; cached_gen = 0 ; cached_value = default } in
  Hashtbl.replace sOptions name (Obj.magic opt) ; (* maaaagic ! *)
  opt

let int_option      default name = define_option ~name:name ~parser:int_of_string   ~serializer:string_of_int   ~default:default
let bool_option     default name = define_option ~name:name ~parser:bool_of_string  ~serializer:string_of_bool  ~default:default
let string_option   default name = define_option ~name:name ~parser:id              ~serializer:id              ~default:default

let generate_config () =
  try
    let ch = open_out kLOCAL_OPTION_PATH in
    let options = keys sOptions in
    Array.sort String.compare options ;
    let keep_longest n1 n2 =
      if slen n1 < slen n2 then n2 else n1
    in
    (* Adjust values on same column *)
    let longest_name = Array.fold_left keep_longest "" options in
    let tab_len = ((slen longest_name) land (lnot 3)) + 4 in
    for i = 0 to Arrays.astop options do
      let name = Arrays.array_get options i in
      let opt = Hashtbl.find sOptions name in
      output_string ch name ;
      for j = 0 to tab_len - (slen name) - 1 do
        output_char ch ' '
      done ;
      output_string ch (opt.serializer (get opt)) ;
      output_char ch '\r' ;
      output_char ch '\n' ;
    done ;
    close_out ch
  with _ -> ()

let reload () =
  ignore (load_options kDEFAULT_OPTION_PATH) ;
  ignore (load_options kLOCAL_OPTION_PATH)

let gen () = !sCacheGeneration

let _ =
  reload () ;
  at_exit generate_config ;
  ()

