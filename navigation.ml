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

let index_entry_compare entry1 entry2 = String.compare entry1.path entry2.path

let zero_index_entry = { path = "" ; dtype = DT_UNKNOWN ; tokens = [] }

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
    entries2      : index_entry array ;
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
    index_entry_buffer   : index_entry Arraybuffer.t ;
    visit_queue   : string Queue.t ;
    token_map     : (string, index_entry list) Hashtbl.t ;
    filter        : string -> string -> bool ;
    buffer        : Buffer.t ;
  }

  let mk_readdir_rec_state path filter =
    let path_buffer = Arraybuffer.empty "?" in
    let index_entry_buffer = Arraybuffer.empty zero_index_entry in
    let visit_queue = Queue.create () in
    let token_map = Hashtbl.create 128 in
    let buffer = Buffer.create 1024 in
    Queue.push path visit_queue ;
    { path_buffer ; index_entry_buffer ; visit_queue ; token_map ; filter ; buffer }

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
                    if d_type == DT_REG then
                      Arraybuffer.append state.path_buffer new_path ;
                    let tokens' = item :: tokens in
                    let entry = {
                      dtype     = d_type ;
                      path      = new_path ;
                      tokens    = tokens' ;
                    } in
                    Arraybuffer.append state.index_entry_buffer entry ;
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
    let entries = Arraybuffer.to_array state.path_buffer in
    Array.sort String.compare entries ;
    let entries2 = Arraybuffer.to_array state.index_entry_buffer in
    Array.sort index_entry_compare entries2 ;
    {
      entries ;
      entries2 ;
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

  let rec make_matcher pattern =
    match string_first pattern with
      | '!' when pattern = "!"  ->  const true
      | '!'   ->  neg (make_matcher (string_drop 1 pattern))
      | '='   ->  (=) (string_drop 1 pattern) (* TODO: only match directory *)
      | '/'   ->  string_starts_with (string_drop 1 pattern)
      | '~'   ->
          begin try
            let regexp = Str.regexp (string_drop 1 pattern) in
            fun token ->
              begin match Str.search_forward regexp token 0 with
                | _           -> true
                | exception _ -> false
              end
          with
            (* TODO: for incorrect regexps, show the pattern in red *)
            e -> const true
          end
      | _     ->  string_is_substring pattern

  (* patterns must not be empty *)
  let find_matches { entries2 } patterns =
    let buffer = Arraybuffer.empty "" in
    let matchers = patterns |> List.filter ((<>) "") |> List.map make_matcher in
    Array.iter (fun { path ; dtype ; tokens } ->
      if dtype = DT_REG && List.for_all (fun matcher -> List.exists matcher tokens) matchers
        then Arraybuffer.append buffer path
    ) entries2 ;
    Arraybuffer.to_array buffer

  let start_matcher_thread file_index patterns_channel match_channel =
    Thread.create (fun () ->
      while true do
        patterns_channel
          |> Event.receive
          |> Event.sync
          |> find_matches file_index
          |> Event.send match_channel
          |> Event.sync
      done) ()
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
  Navigator.find_matches file_index [pattern] |> print_entries ;
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
        | Some "" ->
              print_frame framebuffer path file_index.entries (Printf.sprintf " (found %d)" (alen file_index.entries));
              loop framebuffer path index ""
        | Some new_pattern ->
              let t1 = Sys.time () in
              let entries = Navigator.find_matches file_index (String.split_on_char ' '  new_pattern) in
              let t2 = Sys.time () in
              let delta = t2 -. t1 in
              let footer = Printf.sprintf "%s (time %f, found %d)" new_pattern delta (alen entries) in
              (*
              let entries = Navigator.find_matches file_index (String.split_on_char ' '  new_pattern) in
              *)
              print_frame framebuffer path entries footer ;
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

let navigation_test3 () =
  let open Term in
  let path = path_normalize (if alen Sys.argv > 1 then Sys.argv.(1) else "/etc") in
  let filter anydir item = item <> ".git" in
  let file_index = Navigator.mk_file_index ~filter:filter path in
  let next_key = Keys.make_next_key_fn () in
  let patterns_channel = Event.new_channel () in
  let match_channel = Event.new_channel () in
  let matcher_thread = Navigator.start_matcher_thread file_index patterns_channel match_channel in
  let _ = matcher_thread in
  let patterns_event = ref (Event.always ()) in
  let rec loop framebuffer path index pattern last_match =
    Event.poll !patterns_event |> ignore ;
    Event.receive match_channel
      |> Event.poll
      |> function
        | Some entries ->
            print_frame framebuffer path entries (Printf.sprintf "input: %s (found %d)" pattern (alen entries));
            loop framebuffer path index pattern entries
        | None -> () ;
    next_key ()
      |> (function
        | Key c when c = '\x03'   -> None
        | Key c when c = '\x7f'   -> Some (String.sub pattern 0 (max 0 ((slen pattern) - 1)))
        | Key c                   -> Some (pattern ^ (string_of_char c))
        | _                       -> Some pattern)
      |> (function
        | None -> () (* exit *)
        | Some same_pattern when pattern = same_pattern ->
              loop framebuffer path index pattern last_match
        | Some "" ->
              print_frame framebuffer path file_index.entries (Printf.sprintf " (found %d)" (alen file_index.entries));
              loop framebuffer path index "" file_index.entries
        | Some new_pattern ->
              patterns_event := Event.send patterns_channel (String.split_on_char ' '  new_pattern) ;
              print_frame framebuffer path file_index.entries (Printf.sprintf " (found %d)" (alen file_index.entries));
              loop framebuffer path index new_pattern file_index.entries)
  in
  try
    Term.terminal_set_raw () ;
    let term_dim = Term.terminal_dimensions () in
    let framebuffer = Framebuffer.mk_framebuffer term_dim in
    print_frame framebuffer path (Navigator.index_to_entries file_index) "" ;
    loop framebuffer path file_index "" [||];
    Term.terminal_restore ()
  with
    e ->  Term.terminal_restore () ;
          Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
          Printexc.print_backtrace stdout

(* pipe output into | sort | uniq -c *)
let index_histogram_test () =
  let base_path = path_normalize (if alen Sys.argv > 1 then Sys.argv.(1) else "/etc") in
  let filter anydir item = item <> ".git" in
  let { Navigator.entries ; Navigator.token_map } = Navigator.mk_file_index ~filter:filter base_path in
(*
  (* token length *)
  Hashtbl.iter (fun token entries -> Printf.printf "%d\n" (slen token)) token_map
  (* path length *)
  Array.iter (fun path -> Printf.printf "%d\n" (slen path)) entries
  (* tokens per path *)
  Array.iter (fun path -> Printf.printf "%d\n" (path |> String.split_on_char '/' |> List.length)) entries
*)
  (* paths per token *)
  Hashtbl.iter (fun token entries -> Printf.printf "%d\n" (List.length entries)) token_map

let navigation_test =
  (*
  navigation_test1
  index_histogram_test
  *)
  navigation_test2
