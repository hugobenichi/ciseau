open Util
open Util.Arrays

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
    token_map     : (string, string list) Hashtbl.t ;
    token_index   : Suffixarray.t ;
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
    token_map     : (string, string list) Hashtbl.t ;
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

  let readdir_foreach path fn =
    let rec loop dir_handle fn =
      match Unix.readdir dir_handle with
        | y                     -> (fn y) ; loop dir_handle fn
        | exception End_of_file -> Unix.closedir dir_handle
    in
    match Unix.opendir path with
      | dir_handle -> loop dir_handle fn
      | exception Unix.Unix_error (Unix.EACCES, _, _)   -> ()
      | exception Unix.Unix_error (Unix.ENOTDIR, _, _)  -> ()
      | exception Unix.Unix_error (Unix.ENOENT, _, _)   -> ()

  (* breath first search *)
  let readdir_recursive { path_buffer ; visit_queue ; token_map ; filter } =
    let fn path item =
      if  item <> "." && item <> ".." && filter path item
        then (
          let new_entry = path ^ "/" ^ item in
          Arraybuffer.append path_buffer new_entry ;
          Queue.push new_entry visit_queue ;
        )
    in
    while not (Queue.is_empty visit_queue) do
      let path = Queue.pop visit_queue in
      readdir_foreach path (fn path) ;
    done

  let nofilter anydir anyname = true

  let token_index_insert tbl path token =
    if slen token > 0 then (
      insert_time -=. Sys.time () ;
      Hashtbl.find_opt tbl token
        |> Options.get_or []
        |> List.cons path
        |> Hashtbl.replace tbl token ;
      insert_time +=. Sys.time ()
    )

  (* depth first search. Caveats: can stack overflow or exhaust fds for very deep hierarchies. *)
  let rec readdir_recursive2 state tokens path =
    match Unix.opendir path with
      | dir ->
        (try
          let buffer_n = Buffer.length state.buffer in
          while true do
            let item = Unix.readdir dir in
            if  item <> "." && item <> ".." && state.filter path item then (
              Buffer.add_char state.buffer '/' ;
              Buffer.add_string state.buffer item ;
              let new_path = Buffer.contents state.buffer in
              Arraybuffer.append state.path_buffer new_path ;
              let tokens' = item :: tokens in
              (* PERF: how to create the token -> path list cheaply without a hashtbl *)
              List.iter (token_index_insert state.token_map new_path) tokens' ;
              readdir_recursive2 state tokens' new_path ;
              Buffer.truncate state.buffer buffer_n
            )
          done
        with End_of_file -> Unix.closedir dir)
      | exception Unix.Unix_error (Unix.EACCES, _, _)   -> ()
      | exception Unix.Unix_error (Unix.ENOTDIR, _, _)  -> ()
      | exception Unix.Unix_error (Unix.ENOENT, _, _)   -> ()

  let mk_file_index ?recursive:(recur=false) ?filter:(filter=nofilter) path =
    let t1 = Sys.time () in
    let state = mk_readdir_rec_state path filter in
    if recur
      then (Buffer.add_string state.buffer path ; readdir_recursive2 state [] path )
      (* TODO: eliminate second branch by putting recurence flag in readdir_rec_state *)
      else readdir_foreach path (Arraybuffer.append state.path_buffer) ;
    Printf.printf "exploration done: %f\n" ((Sys.time ()) -. t1) ;
    let entries = Arraybuffer.to_array state.path_buffer in
    Array.sort String.compare entries ;
    Printf.printf "sort done: %f\n" ((Sys.time ()) -. t1) ;
    (* let token_index = Suffixarray.mk_suffixarray (keys token_map) token_map in *)
    let token_index = Suffixarray.mk_suffixarray [||] state.token_map in
    Printf.printf "index done: %f\n" ((Sys.time ()) -. t1) ;
    { entries ; token_map = state.token_map ; token_index }

  let index_to_entries { entries } = entries

  let mk_range { token_index } = Suffixarray.mk_range token_index ""

  let string_byte_adder byte_count path = byte_count + (slen path)
  let string_byte_adder2 token _ byte_count = string_byte_adder byte_count token

  let file_index_stats { entries ; token_map ; token_index } =
  {
    total_entries         = alen entries ;
    total_tokens          = Hashtbl.length token_map ;
    total_entries_length  = Array.fold_left string_byte_adder 0 entries ;
    total_tokens_length   = Hashtbl.fold string_byte_adder2 token_map 0 ;
  }

  let rec append_all buffer =
    function
      | [] -> ()
      | h :: t -> Arraybuffer.append buffer h ; append_all buffer t

  let starts_with prefix str =
    let rec loop i stop str_a str_b =
      i = stop || (String.get str_a i) = (String.get str_b i) && loop (i+1) stop str_a str_b
    in
    loop 0 (slen prefix) prefix str

  let find_match { entries ; token_map ; token_index } pattern =
    let buffer = Arraybuffer.empty "?" in
    let regexp = Str.regexp pattern in
    Hashtbl.iter (fun token path_list ->
      if Str.string_match regexp token 0 then (
      (*
      if string_starts_with pattern token then (
      *)
        append_all buffer path_list
      )
    ) token_map ;
    let matches = Arraybuffer.to_array buffer in
    Array.sort String.compare matches ;
    matches
end

let print_entries = Array.iter print_stringln

let navigation_test () =
  let gc_stat = Gc.quick_stat () in
  let base_path = if alen Sys.argv > 1 then Sys.argv.(1) else "/etc" in
  let filter anydir item = item <> ".git" in
  print_string base_path ; print_newline () ;
  let file_index = Navigator.mk_file_index ~recursive:true ~filter:filter base_path in
  (*
  print_entries (Navigator.index_to_entries file_index) ;
  print_newline () ;
  *)
  Navigator.mk_range file_index
    |> Suffixarray.range_to_array
    |> ignore ;
    (*
    |> print_entries ;
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


