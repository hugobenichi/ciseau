open Util
open Util.Arrays

type dir_type = DT_BLK       (* This is a block device. *)
              | DT_CHR       (* This is a character device. *)
              | DT_DIR       (* This is a directory. *)
              | DT_FIFO      (* This is a named pipe (FIFO). *)
              | DT_LNK       (* This is a symbolic link. *)
              | DT_REG       (* This is a regular file. *)
              | DT_SOCK      (* This is a UNIX domain socket. *)
              | DT_UNKNOWN   (* The file type could not be determined. *)
external readdir_t : Unix.dir_handle -> string * dir_type = "readdir_t"

let dir_type_name =
  function
    | DT_BLK      -> "DT_BLK"
    | DT_CHR      -> "DT_CHR"
    | DT_DIR      -> "DT_DIR"
    | DT_FIFO     -> "DT_FIFO"
    | DT_LNK      -> "DT_LNK"
    | DT_REG      -> "DT_REG"
    | DT_SOCK     -> "DT_SOCK"
    | DT_UNKNOWN  -> "DT_UNKNOWN"

type index_entry = {
  dtype     : dir_type ;
  path      : string ;
  tokens    : string list ;
}

let zero_index_entry = { path = "" ; dtype = DT_UNKNOWN ; tokens = [] }

module Suffixarray = struct
  (* Is this reasonable for real input
   *
   *  strategy 1): a suffix array whose entries are the full path of every files found in a subtree of the filesystem
   *
   *  linux kernel 4.14: 67057 entries, total char length is 2452128
   *    overhead of index array made of int offset + int index into an array of string would be:
   *      all string entries:         67k * block header + 67k null byte + 2.4Mb ~= 3Mb
   *      array of string entries:    67k * ptr + array overhead ~= 67k
   *      array of int tuples:        2.4M * ptr + 2.4M * block header + 8B * 2 = 76Mb !!
   *      array of packed int tuples: 2.4M * 2 * 8B = 38.4Mb
   *
   *  framework/
   *    total entries: 69446
   *    total string length: 4215909
   *
   *  strategy 2): break down all paths in intermediary token (directory names) and make a suffix array with tokens
   *
   *  If I build a in-memory tree of all entries with just tokens
   *    I can consolidate all tokens for directories
   *    index all tokens in the suffix array (maybe using just real substrings to begin with)
   *    create a map where all existing tokens are pointing to a list of all paths that contains just tokens
   *      for all path
   *        for all token in path
   *          index in the map that path as value for that token
   *      this can be used as a jump table, from token key, find list of all tokens
   *    range creation:
   *      get user input, do a search for all tokens matching this
   *      for all such tokens, get all paths
   *      sort paths, eliminate duplicates
   *
   *   linux kernel 4.14:
   *      48266 unique token when separating with /
   *      total string length: 607081
   *        the suffix array would be around:
   *          48k * (ptr + string block header) + 607kb
   *          + 607k * 2 * 8b ~= 10 Mb
   *        the path set would be around 2.4Mb + linked list overhead
   *        the token to path map would be around 48k
   *)

  type stringview = {
    s : string ;
    o : int ;
  }

  type t = {
    entries           : string array ;  (* All string entries in the suffix array *) (* TODO: rename as keys *)
    substrings        : int array ;     (* A packed flattened (int, int) array of (entries index, entries offset) for defining all substrings in entries *)
    substrings_index  : int array ;     (* array of indexes into substrings, intended to be sorted with library sort *)
    entry_to_values   : (string, string list) Hashtbl.t
  }

  type range = {
    suffixarray : t ;
    start       : int ;
    stop        : int ;
  }

  (* TODO: cleanup all names to clarify what is entry and what is key/token *)

  let get_entry_index { entries ; substrings } i =
    array_get substrings (2 * i)

  let get_entry { entries ; substrings } i =
    2 * i |> array_get substrings |> array_get entries

  let get_offset { entries ; substrings } i =
    2 * i + 1 |> array_get substrings

  (* PERF: use native strcmp instead ! *)
  let rec compare_substrings ~left:left ~left_offset:i ~right:right ~right_offset:j =
    string_compare_fast left i right j

  let mk_suffixarray entries_original entry_to_values =
    let entries = Array.copy entries_original in
    let substrings_buffer = Arraybuffer.empty 0 in
    for i = 0 to astop entries do
      for j = 0 to (slen entries.(i)) - 1 do
        Arraybuffer.append substrings_buffer i ;
        Arraybuffer.append substrings_buffer j
      done
    done ;
    let substrings = Arraybuffer.to_array substrings_buffer in
    let substrings_index = Array.init ((alen substrings) / 2) id in
    let compare_suffixes i j =
      compare_substrings
        ~left:entries.(substrings.(2*i)) ~left_offset:substrings.(2*i + 1)
        ~right:entries.(substrings.(2*j)) ~right_offset:substrings.(2*j + 1)
    in
    (* PERF: toooooo sloooooow *)
    let t1 = Sys.time () in
    Array.sort compare_suffixes substrings_index ;
    Printf.printf "suffixarray(len:%d) sort time %f\n" (alen substrings_index) ((Sys.time ()) -. t1) ;
    { entries ; substrings ; substrings_index ; entry_to_values }

  let prepare suffixarray entrie = ()

  let refine_range prefix { suffixarray ; start ; stop } =
    let start' = ref start in
    let stop' = ref stop in
    if slen prefix > 0 then (
      let compare_with_prefix i =
        compare_substrings
          ~left:(get_entry suffixarray i) ~left_offset:(get_offset suffixarray i)
          ~right:prefix ~right_offset:0
      in
      (* TODO: replace by proper binary search *)
      while !start' <= !stop' && compare_with_prefix !start' < 0 do incr start' done ;
      while !start' <= !stop' && compare_with_prefix !stop' > 0 do decr stop' done
    ) ;
    { suffixarray ; start = !start' ; stop = !stop' }

  let mk_range suffixarray prefix =
    refine_range prefix { suffixarray ; start = 0 ; stop = alen suffixarray.substrings_index - 1 }

  let range_to_array { suffixarray ; start ; stop } =
    (* TODO: use standard Set instead *)
    let entry_set = Hashtbl.create (min (stop - start) 32) in
    for i = start to stop do
      get_entry suffixarray start
        |> Hashtbl.find suffixarray.entry_to_values
        |> List.iter (fun entry -> Hashtbl.replace entry_set entry true)
    done ;
    Hashtbl.remove entry_set "" ;
    let entries = keys entry_set in
    Array.sort String.compare entries ;
    entries

  (* This still does not return the original entries, only the token to the keys !! *)
  let range_to_array_old { suffixarray ; start ; stop } =
    let dup_entries = Array.init (stop - start) ((+) start >> get_entry suffixarray) in
    Array.sort String.compare dup_entries ;
    let cursor = ref 0 in
    for i = 1 to astop dup_entries do
      if array_get dup_entries !cursor <> array_get dup_entries i then (
        array_set dup_entries (!cursor + 1) (array_get dup_entries i) ;
        incr cursor
      )
    done ;
    Array.init !cursor (array_get dup_entries)
(* This still does not return the original entries, only the token to the keys !! *)

end

let insert_time = ref 0.0

module Navigator = struct
  (* Note on different interface into range search:
   *  - Takes 5 seconds to index Linux, 99% of it is file system exploration
   *      total_entries=72169     total_entries_length=3279113
   *      total_tokens=50022      total_tokens_length=637316
   *  - is it worthwhile to try to do iterative search narrowing ?
   *    - requires keeping some kind of cursor and update it, use code has to care
   *    - try remove visibility into Suffixarray
   *  - could base function just be find find_all_paths_by_token: string -> string iter / string array
   *  - actually, at which point is an index necessary in the first place ?
   *  - just having all paths in memory and brute force searching them might be just good enough
   *)

  type filter_fn = string -> string -> bool

  (* TODO: should directories be handled separately ? should they be included at all ? *)
  type file_index = {
    entries       : string array ;
    token_map     : (string, index_entry list) Hashtbl.t ;
  }

  type stats = {
    total_entries         : int ;
    total_tokens          : int ;
    total_entries_length  : int ;
    total_tokens_length   : int ;
  }

  type readdir_rec_state = {
    path_buffer   : string Arraybuffer.t ;
    visit_queue   : string Queue.t ;
    token_map     : (string, index_entry list) Hashtbl.t ;
    filter        : string -> string -> bool ;
    buffer        : Buffer.t ;
  }

  let mk_readdir_rec_state path filter =
    let path_buffer = Arraybuffer.empty "?" in
    let visit_queue = Queue.create () in
    let token_map = Hashtbl.create 128 in
    let buffer = Buffer.create 1024 in
    Queue.push path visit_queue ;
    { path_buffer ; visit_queue ; token_map ; filter ; buffer }

  let nofilter anydir anyname = true

  let token_index_insert tbl entry token =
    if slen token > 0 then (
      insert_time -=. Sys.time () ;
      Hashtbl.find_opt tbl token
        |> Options.get_or []
        |> List.cons entry
        |> Hashtbl.replace tbl token ;
      insert_time +=. Sys.time ()
    )

  (* depth first directory walk. Caveats: can stack overflow or exhaust fds for very deep hierarchies. *)
  let rec readdir state tokens path =
    match Unix.opendir path with
      | dir ->
          let buffer_n = Buffer.length state.buffer in
          let go_on = ref true in
          while !go_on do
            match readdir_t dir with
              | ("", _) -> go_on := false
              | (".", _)
              | ("..", _) -> ()
              | (item, _) when not (state.filter path item) -> ()
              | (item, d_type) when d_type = DT_DIR || d_type = DT_REG ->
                  begin
                    Buffer.add_char state.buffer '/' ;
                    Buffer.add_string state.buffer item ;
                    let new_path = Buffer.contents state.buffer in
                    Arraybuffer.append state.path_buffer new_path ;
                    let tokens' = item :: tokens in
                    let entry = {
                      dtype     = d_type ;
                      path      = new_path ;
                      tokens    = tokens' ;
                    } in
                    (* PERF: how to create the token -> path list cheaply without a hashtbl *)
                    List.iter (token_index_insert state.token_map entry) tokens' ;
                    if d_type == DT_DIR then
                      readdir state tokens' new_path ;
                    Buffer.truncate state.buffer buffer_n
                  end
              | (_, _) -> ()
        done ;
        Unix.closedir dir
    | exception Unix.Unix_error (Unix.EACCES, _, _)   -> ()
    | exception Unix.Unix_error (Unix.ENOTDIR, _, _)  -> ()
    | exception Unix.Unix_error (Unix.ENOENT, _, _)   -> ()

  let mk_file_index ?filter:(filter=nofilter) path =
    let state = mk_readdir_rec_state path filter in
    Buffer.add_string state.buffer path ;
    readdir state [] path ;
    {
      entries = Arraybuffer.to_array state.path_buffer ;
      token_map = state.token_map ;
    }

  let index_to_entries { entries } = entries

  let string_byte_adder byte_count path = byte_count + (slen path)
  let string_byte_adder2 token _ byte_count = string_byte_adder byte_count token

  let file_index_stats { entries ; token_map } =
  {
    total_entries         = alen entries ;
    total_tokens          = Hashtbl.length token_map ;
    total_entries_length  = Array.fold_left string_byte_adder 0 entries ;
    total_tokens_length   = Hashtbl.fold string_byte_adder2 token_map 0 ;
  }

  let rec append_all buffer =
    function
      | [] -> ()
      | entry :: t -> Arraybuffer.append buffer entry ; append_all buffer t

  let starts_with prefix str =
    let rec loop i stop str_a str_b =
      i = stop || (String.get str_a i) = (String.get str_b i) && loop (i+1) stop str_a str_b
    in
    loop 0 (slen prefix) prefix str

  let find_match { entries ; token_map } pattern =
    let buffer = Arraybuffer.empty zero_index_entry in
    let regexp = Str.regexp pattern in
    Hashtbl.iter (fun token entry_list ->
      if Str.string_match regexp token 0 then
      (*
      if string_starts_with pattern token then
      *)
        append_all buffer entry_list
    ) token_map ;
    let matches = Array.map (fun { path } -> path) (Arraybuffer.to_array buffer) in
    Array.sort String.compare matches ;
    matches

  let rec refine_match entry_buffer =
    function
      | [] -> ()
      | pattern :: pattern_tail ->
          let match_pattern token = Str.string_match (Str.regexp pattern) token 0 in
          let i = ref 0 in
          while !i < Arraybuffer.len entry_buffer do
            let { tokens } = Arraybuffer.get entry_buffer !i in
            if List.exists match_pattern tokens
              then incr i
              else Arraybuffer.del entry_buffer !i
          done ;
          refine_match entry_buffer pattern_tail

  let find_multi_match { entries ; token_map } patterns =
    let buffer = Arraybuffer.empty zero_index_entry in
    let regexp = Str.regexp (List.hd patterns) in
    Hashtbl.iter (fun token entry_list ->
      if Str.string_match regexp token 0 then
        append_all buffer entry_list
    ) token_map ;
    refine_match buffer (List.tl patterns) ;
    let matches = Array.map (fun { path } -> path) (Arraybuffer.to_array buffer) in
    Array.sort String.compare matches ;
    matches

end

let print_entries = Array.iter print_stringln

let print_frame framebuffer path entries input =
  let open Term in
  let x_offset = 1 in (* BUG: why do I need a +1 offset !!??!! *)
  let fb_height = (Framebuffer.framebuffer_size framebuffer).Vec.y in
  let max_entry = min (alen entries) (fb_height - 1 (* header *) - 1 (* input *)) in
  Framebuffer.clear framebuffer ;
  for i = 0 to max_entry - 1 do
    Framebuffer.put_line framebuffer ~y:(i+1) ~x:x_offset (array_get entries i)
  done ;
  Framebuffer.put_line framebuffer ~y:(fb_height-1) ~x:x_offset ("input: " ^ input) ;
  Framebuffer.put_line framebuffer ~y:0 ~x:x_offset ~len:(slen path) path ;
  (*
  *)
  Framebuffer.render framebuffer

let path_normalize path =
  let last = (slen path) - 1 in
  if String.get path last = '/'
    then String.sub path 0 last
    else path

let navigation_test1 () =
  let gc_stat = Gc.quick_stat () in
  let base_path = path_normalize (if alen Sys.argv > 1 then Sys.argv.(1) else "/etc")
  in
  let filter anydir item = item <> ".git" in
  print_string base_path ; print_newline () ;
  let t1 = Sys.time () in
  let file_index = Navigator.mk_file_index ~filter:filter base_path in
  Printf.printf "exploration done: %f\n" ((Sys.time ()) -. t1) ;
  (*
  print_entries (Navigator.index_to_entries file_index) ;
  print_newline () ;
  *)
  print_string "insert: " ; print_float !insert_time ; print_newline () ;
  let gc_stat2 = Gc.quick_stat () in
  Printf.printf "minor_col:%d major_col:%d\n"
    (gc_stat2.minor_collections - gc_stat.minor_collections)
    (gc_stat2.major_collections - gc_stat.major_collections) ;
  let {
    Navigator.total_entries         ;
    Navigator.total_tokens          ;
    Navigator.total_entries_length  ;
    Navigator.total_tokens_length   ;
  } = Navigator.file_index_stats file_index in
  Printf.printf
    "entries=%d tokens=%d entries_length=%d tokens_length=%d\n"
    total_entries
    total_tokens
    total_entries_length
    total_tokens_length ;
  let a1 = Sys.time () in
  let pattern = if alen Sys.argv > 2 then Sys.argv.(2) else "xfrm" in
  Navigator.find_match file_index pattern |> print_entries ;
  Printf.printf "find_time: %f\n" ((Sys.time ()) -. a1)

let navigation_test2 () =
  let open Term in
  let path = path_normalize (if alen Sys.argv > 1 then Sys.argv.(1) else "/etc") in
  let filter anydir item = item <> ".git" in
  let file_index = Navigator.mk_file_index ~filter:filter path in
  let next_key = Keys.make_next_key_fn () in
  let rec loop framebuffer path index pattern =
    next_key ()
      |> (function
        | Key c when c = '\x03'   -> None
        | Key c when c = '\x7f'   -> Some (String.sub pattern 0 (max 0 ((slen pattern) - 1)))
        | Key c                   -> Some (pattern ^ (string_of_char c))
        | _                       -> Some pattern)
      |> (function
        | None -> () (* exit *)
        | Some same_pattern when pattern = same_pattern ->
              loop framebuffer path index pattern
        | Some new_pattern ->
              let entries = Navigator.find_multi_match file_index (String.split_on_char ' '  new_pattern) in
              print_frame framebuffer path entries new_pattern ;
              loop framebuffer path index new_pattern)
  in
  try
    Term.terminal_set_raw () ;
    let term_dim = Term.terminal_dimensions () in
    let framebuffer = Framebuffer.mk_framebuffer term_dim in
    print_frame framebuffer path (Navigator.index_to_entries file_index) "" ;
    loop framebuffer path file_index "" ;
    Term.terminal_restore ()
  with
    e ->  Term.terminal_restore () ;
          Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
          Printexc.print_backtrace stdout

let navigation_test = navigation_test2
