open Util

(* TODO: read these from environment variables first *)
(* TODO: collapse into a list of path that is specified as a single ENV var *)
let kDEFAULT_OPTION_PATH  = "$HOME/.ciseaurc"
let kLOCAL_OPTION_PATH    = "./.ciseaurc"

type 'a option_t = {
  name                  : string ;
  parser                : string list -> 'a ;
  serializer            : 'a -> string list ;
  default               : 'a ;
  mutable cached_value  : 'a option ;
}


(* Global hashtable that stores raw key values read from config files *)
let sKeyvals : (string, string list) Hashtbl.t = Hashtbl.create 10
(* Global hashtable that stores all defined options *)
let sOptions : (string, unit option_t) Hashtbl.t = Hashtbl.create 10

let get opt =
  match opt.cached_value with
    | Some v -> v
    | None -> begin
        let v =
          try
            opt.name |> Hashtbl.find sKeyvals |> opt.parser
          with
            _ -> opt.default
        in
          opt.cached_value <- Some v ;
          v
    end

let has { name } = Hashtbl.mem sKeyvals name

let process_lines lines =
  for i = 0 to Arrays.astop lines do
    Arrays.array_get lines i
      |> string_split is_space
      |> function
          | [] -> ()
          | k :: v -> begin
            Hashtbl.replace sKeyvals k v ;
            (* PERF: do nothing if the old v is like the new v *)
            try
              let opt = Hashtbl.find sOptions k in
              opt.cached_value <- Some (opt.parser v)
            with _ -> ()
          end
  done

let load_options path =
  path |> read_file |> process_lines ;
  None

let clear_options () = Hashtbl.clear sKeyvals

let define_option ~name:name ~parser:parser ~serializer:serializer ~default:default =
  let opt = { name ; parser ; serializer ; default ; cached_value = None } in
  Hashtbl.replace sOptions name (Obj.magic opt) ; (* maaaagic ! *)
  opt

let list1 x = [x]
let int_option      default name = define_option ~name:name ~parser:(List.hd >> int_of_string)   ~serializer:(string_of_int >> list1)   ~default:default
let bool_option     default name = define_option ~name:name ~parser:(List.hd >> bool_of_string)  ~serializer:(string_of_bool >> list1)  ~default:default
let string_option   default name = define_option ~name:name ~parser:(List.hd >> id)              ~serializer:(list1)                    ~default:default

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
      let rec loop sep =
        function
          | [] -> ()
          | hd :: tl -> begin
              output_string ch sep ;
              output_string ch hd ;
              loop " " tl
            end
      in
      loop "" (opt.serializer (get opt)) ;
      output_char ch '\r' ;
      output_char ch '\n' ;
    done ;
    close_out ch
  with _ -> ()

let reload () =
  (* BUG: if an option is removed from the config, the cached value might be incorrect
   * and it should be reset to the default value *)
  ignore (load_options kDEFAULT_OPTION_PATH) ;
  ignore (load_options kLOCAL_OPTION_PATH)

let _ =
  reload () ;
  at_exit generate_config ;
  ()

