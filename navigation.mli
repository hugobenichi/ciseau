(* TODO: flatten into main namespace *)
module Navigator : sig
  (* Filter function for excluding directories or files from listing
   * 1st arg: directory being listed
   * 2nd arg: item found in the directory
   * return value: true if the item should be listed *)
  type filter_fn = string -> string -> bool

  type file_index

  type stats = {
    total_entries             : int ;
    total_tokens              : int ;
    total_entries_length      : int ;
    total_tokens_length       : int ;
  }

  val mk_file_index           : ?filter:filter_fn -> string -> file_index
  val index_to_entries        : file_index -> string array
  val file_index_stats        : file_index -> stats
end

val navigation_test           : unit -> unit
