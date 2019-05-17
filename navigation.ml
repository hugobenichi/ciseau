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

let index_entry_path { path } = path

let index_entry_compare entry1 entry2 = String.compare entry1.path entry2.path

let zero_index_entry = { path = "" ; dtype = DT_UNKNOWN ; tokens = [] }

let insert_time = ref 0.0

type filter_fn = string -> string -> bool

type file_index = {
  entries       : index_entry array ;
}

type stats = {
  total_entries         : int ;
  total_entries_length  : int ;
}

type readdir_state = {
  index_entry_buffer   : index_entry Arraybuffer.t ;
  visit_queue   : string Queue.t ;
  filter        : string -> string -> bool ;
  buffer        : Buffer.t ;
}

let mk_readdir_rec_state path filter =
  let index_entry_buffer = Arraybuffer.empty zero_index_entry in
  let visit_queue = Queue.create () in
  let buffer = Buffer.create 1024 in
  Queue.push path visit_queue ;
  { index_entry_buffer ; visit_queue ; filter ; buffer }

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
                  let tokens' = item :: tokens in
                  let entry = {
                    dtype     = d_type ;
                    path      = new_path ;
                    tokens    = tokens' ;
                  } in
                  Arraybuffer.append state.index_entry_buffer entry ;
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
  let index = { entries = Arraybuffer.to_array state.index_entry_buffer } in
  Array.sort index_entry_compare index.entries ;
  index

let index_to_entries { entries } = Array.map index_entry_path entries

let file_index_stats { entries } = {
  total_entries         = alen entries ;
  total_entries_length  = Array.fold_left (fun bytecount { path } -> bytecount + (slen path)) 0 entries ;
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
let find_matches { entries } patterns =
  let buffer = Arraybuffer.empty zero_index_entry in
  let matchers = patterns |> List.filter ((<>) "") |> List.map make_matcher in
  Array.iter (fun entry ->
    if entry.dtype = DT_REG && List.for_all (fun matcher -> List.exists matcher entry.tokens) matchers
      then Arraybuffer.append buffer entry
  ) entries ;
  Arraybuffer.to_array buffer

let print_entries = Array.iter print_stringln

let print_frame framebuffer path entries input =
  let open Term in
  let x_offset = 1 in (* BUG: why do I need a +1 offset !!??!! *)
  let fb_height = (Framebuffer.framebuffer_size framebuffer).Vec.y in
  let max_entry = min (alen entries) (fb_height - 1 (* header *) - 1 (* input *)) in
  Framebuffer.clear framebuffer ;
  for i = 0 to max_entry - 1 do
    Framebuffer.put_line framebuffer ~y:(i+1) ~x:x_offset (array_get entries i).path
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
  let file_index = mk_file_index ~filter:filter base_path in
  Printf.printf "exploration done: %f\n" ((Sys.time ()) -. t1) ;
  print_string "insert: " ; print_float !insert_time ; print_newline () ;
  let gc_stat2 = Gc.quick_stat () in
  Printf.printf "minor_col:%d major_col:%d\n"
    (gc_stat2.minor_collections - gc_stat.minor_collections)
    (gc_stat2.major_collections - gc_stat.major_collections) ;
  let {
    total_entries         ;
    total_entries_length  ;
  } = file_index_stats file_index in
  Printf.printf "entries=%d entries_length=%d\n" total_entries total_entries_length ;
  let a1 = Sys.time () in
  let pattern = if alen Sys.argv > 2 then Sys.argv.(2) else "xfrm" in
  find_matches file_index [pattern] |> Array.map index_entry_path |> print_entries ;
  Printf.printf "find_time: %f\n" ((Sys.time ()) -. a1)

let navigation_test2 () =
  let open Term in
  let path = path_normalize (if alen Sys.argv > 1 then Sys.argv.(1) else "/etc") in
  let filter anydir item = item <> ".git" in
  let file_index = mk_file_index ~filter:filter path in
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
              let entries = find_matches file_index (String.split_on_char ' '  new_pattern) in
              let t2 = Sys.time () in
              let delta = t2 -. t1 in
              let footer = Printf.sprintf "%s (time %f, found %d, insert time %f)" new_pattern delta (alen entries) !insert_time in
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
    print_frame framebuffer path file_index.entries "" ;
    loop framebuffer path file_index "" ;
    Term.terminal_restore ()
  with
    e ->  Term.terminal_restore () ;
          Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
          Printexc.print_backtrace stdout

let navigation_test =
  navigation_test2
