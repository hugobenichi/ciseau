(* TODO: remove visibility into that module *)
module Suffixarray : sig
  (* array of string suffixes *)
  type t
  (* range in a Suffixarray.t of all entries matching a prefix *)
  type range
  val mk_suffixarray          : string array -> (string, string list) Hashtbl.t -> t
  val mk_range                : t -> string -> range
  val refine_range            : string -> range -> range
  val range_to_array          : range -> string array
end

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

  val mk_file_index           : ?recursive:bool -> ?filter:filter_fn -> string -> file_index
  val index_to_entries        : file_index -> string array
  val mk_range                : file_index -> Suffixarray.range
  val file_index_stats        : file_index -> stats
end
