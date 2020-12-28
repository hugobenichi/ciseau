open Util

type text_op_type = TextAdd | TextDel

type text_op = {
  lineno : int ;          (* lineno *)
  line_index : int ;      (* line buffer index of the block being manipulated *)
  line_n     : int ;      (* number of lines being manipulated *)
  op  : text_op ;         (* type of operation *)
}

type text = {
  mutable text_bytes  : Bytes.t ;             (* Append-only buffer for raw file data and insert data *)
  line_starts         : int Arraybuffer.t ;   (* Append-only buffer for storing 'text_bytes' indexes of line starts *)
  line_stops          : int Arraybuffer.t ;   (* Append-only buffer for storing 'text_bytes' indexes of line stops (excluding terminating characters *)
  line_charlen        : int Arraybuffer.t ;   (* Append-only buffer for storing the character length of a line *)
  mutable linenos     : int array ;           (* Mapping of logical linenos to index in 'line_starts' *)
  mutable op_undo     : text_op list ;
  mutable op_redo     : text_op list ;
  (* TODO: add current operation *)
}

let make_empty_text byte_size = {
  text_bytes    = Bytes.make byte_size ' ' ;
  line_starts   = Arraybuffer.mk_arraybuffer 16 0 ;
  line_stops    = Arraybuffer.mk_arraybuffer 16 0 ;
  line_charlen  = Arraybuffer.mk_arraybuffer 16 0 ;
  linenos       = Array.make 0 0 ;
  op_undo       = [] ;
  op_redo       = [] ;
}

let rec find_lines text i =
  if i < blen text.text_bytes
    then begin
      Arraybuffer.append text.line_starts i ;
      let j =
        try Bytes.index_from text.text_bytes i '\n'
        with _ -> blen text.text_bytes
      in
      Arraybuffer.append text.line_stops j ;
      Arraybuffer.append text.line_charlen (j - i) ; (* TODO:UTF8 *)
      find_lines text (j + 1)                        (* TODO:SEPARATOR *)
    end

let load path =
  try
    let stats = Unix.stat path in
    let text = make_empty_text stats.st_size  in
    let ch = open_in path in
    let len = input ch text.text_bytes 0 stats.st_size in
    close_in ch ;
    if len <> stats.st_size
      then Error (Printf.sprintf "Failed to read \"%s\": expected %d bytes, got %d" path stats.st_size len)
      else begin
        find_lines text 0 ;
        text.linenos <- Array.init (Arraybuffer.len text.line_starts) id ;
        Ok text
      end
  with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Error "ENOENT"
    | _ -> Error "unknown exception"

let save path text =
  try
    let ch =  open_out path in
    output ch text.text_bytes 0 (Bytes.length text.text_bytes) ;
    close_out ch ;
    Ok ()
  with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Error "ENOENT"
    | _ -> Error "unknown exception"

let to_strings { text_bytes ; line_starts ; linenos } =
  let strings = Array.make (alen linenos) "" in
  for i = 0 to Arrays.astop linenos do
    let linestart_no = Array.get linenos i in
    let linestart = Arraybuffer.get line_starts linestart_no in
    let linestop = Arraybuffer.get line_starts (linestart_no + 1) in
    Array.set strings i (Bytes.sub_string text_bytes linestart (linestop - linestart))
  done ;
  strings

let text_test () =
  let f = if alen Sys.argv > 1 then Array.get Sys.argv 1 else "./ciseau.ml" in
  let tb =
    match load f with
      | Ok tb -> tb
      | Error e -> begin Printf.printf "Error loading %s: %s\n" f e ; exit 0 end
  in
    match save (f ^ ".bkp" ) tb with
      | Ok () -> ()
      | Error e -> begin Printf.printf "Error saving %s: %s\n" f e end
