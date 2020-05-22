open Util

type text = {
  buffer        : Bytes.t ;         (* Append buffer for raw file data and insert data *)
  line_starts   : int array ;       (* Append buffer for marking line start indexes in 'buffer'.
                                       The next entry is always the start of next line in 'buffer'. *)
  linenos       : int array ;       (* Mapping of logical linenos to index in 'line_starts' *)
}

let load path =
  let stats = Unix.stat path in
  let text = {
      buffer = Bytes.make stats.st_size ' ' ;
      line_starts = Array.make 0 0 ;
      linenos = Array.make 0 0 ;
  } in
  let ch = open_in path in
  let len = input ch text.buffer 0 stats.st_size in
  if len <> stats.st_size
  then Error (Printf.sprintf "Failed to read \"%s\": expected %d bytes, got %d" path stats.st_size len)
  else
    Ok text


let text_test () =
  let f = "./ciseau.ml" in
  let tb =
    match load f with
      | Ok tb -> tb
      | Error e -> fail e
  in
  exit 0
