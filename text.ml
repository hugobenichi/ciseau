open Util

type text = {
  buffer        : Bytes.t ;         (* Append buffer for raw file data and insert data *)
  line_starts   : int Arraybuffer.t ;       (* Append buffer for marking line start indexes in 'buffer'.
                                       The next entry is always the start of next line in 'buffer'. *)
  linenos       : int array ;       (* Mapping of logical linenos to index in 'line_starts' *)
}

let rec find_lines buffer lines i =
  if i < blen buffer
    then begin
      Arraybuffer.append lines i ;
      let j =
        try Bytes.index_from buffer i '\n'
        with _ -> blen buffer
      in
      find_lines buffer lines (j + 1)
    end

let from_bytes buffer =
    let line_starts = Arraybuffer.mk_arraybuffer 16 0 in
    find_lines buffer line_starts 0 ;
    {
        buffer ;
        line_starts ;
        linenos = Array.init (Arraybuffer.len line_starts) id ;
    }

let load path =
  try
    let stats = Unix.stat path in
    let buffer = Bytes.make stats.st_size ' ' in
    let ch = open_in path in
    let len = input ch buffer 0 stats.st_size in
    close_in ch ;
    if len <> stats.st_size
      then Error (Printf.sprintf "Failed to read \"%s\": expected %d bytes, got %d" path stats.st_size len)
      else Ok (from_bytes buffer)
  with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Error "ENOENT"
    | _ -> Error "unknown exception"

let save path tb =
  try
    let ch =  open_out path in
    output ch tb.buffer 0 (Bytes.length tb.buffer) ;
    close_out ch ;
    Ok ()
  with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Error "ENOENT"
    | _ -> Error "unknown exception"

let to_strings { buffer ; line_starts ; linenos } =
  let strings = Array.make (alen linenos) "" in
  for i = 0 to Arrays.astop linenos do
    let linestart_no = Array.get linenos i in
    let linestart = Arraybuffer.get line_starts linestart_no in
    let linestop = Arraybuffer.get line_starts (linestart_no + 1) in
    Array.set strings i (Bytes.sub_string buffer linestart (linestop - linestart))
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
