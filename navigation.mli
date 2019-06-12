type file_index_entry =
  | File of string
  | Dir of string
  | UnixSocket of string
  | Link of string * string

(* Filter function for excluding directories or files from listing
 * 1st arg: directory being listed
 * 2nd arg: item found in the directory
 * return value: true if the item should be listed *)
type filter_fn = string -> string -> bool

type file_index

type stats = {
  total_entries               : int ;
  total_entries_length        : int ;
  gc_minor_collections        : int ;
  gc_major_collections        : int ;
  construction_time           : float ;
}

val mk_file_index             : ?filter:filter_fn -> basedir:string -> file_index
val mk_file_index_empty       : ?filter:filter_fn -> basedir:string -> file_index
val file_index_continue       : ?duration:float -> file_index -> file_index
val file_index_has_pending    : file_index -> bool
(* TODO:  1) consider returning a thing that can be iterated instead of an array
          2) return something better than strings
*)
val file_index_entries        : file_index -> string array
val file_index_stats          : file_index -> stats
(* TODO: add query interface *)

val navigation_test           : unit -> unit

(* TODO: Add a navigator type that:
 *  - manages indexes, can refresh indexes, ...
 *  - accept search queries
 *  - track open/closed files, can manage files by subgroups
 *)
