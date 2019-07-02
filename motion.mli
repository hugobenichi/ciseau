(* 4-directional text cursor *)
module Cursor : sig
  open Util

  (* TODO: hide this subtype *)
  type step = Nomore
            | Continue

  type t

  val mk_cursor       : string array -> int -> int -> t
  val x               : t -> int        (* current column index *)
  val y               : t -> int        (* current line index *)
  val xmem            : t -> int
  val pos             : t -> Vec.vec2   (* current column and line indexes as a vec *)
  val save            : t -> t
  val goto            : ?x:int -> ?y:int -> t -> unit
  val xmem_set        : t -> int -> unit

  val line_get        : t -> string
  val line_len        : t -> int
  val line_is_empty   : t -> bool
  val line_not_empty  : t -> bool
  val line_next       : t -> step
  val line_prev       : t -> step
  val line_first      : t -> unit
  val line_last       : t -> unit
  val line_is_last    : t -> bool

  val char_is_first   : t -> bool
  val char_is_last    : t -> bool
  val char_get        : t -> char       (* TODO: what to do for empty lines ?? *)
  val char_next       : t -> step       (* move to next char, or return Nomore if cursor is at end of line *)
  val char_prev       : t -> step       (* move to previous char, or return Nomore if cursor is at beginning of line *)
  val char_zero       : t -> unit
  val char_first      : t -> unit       (* go to first non-space character *)
  val char_last       : t -> unit

  val prev            : t -> step
  val next            : t -> step

end

module Movement : sig
  (* TODO: a selected area cannot be a rect all the time when it covers multiple lines *)
  type selection_context = Util.Rec.rec2 array

  type mode = Blocks
            | Words
            | Digits
            | Lines
            | Chars
            | Paragraphs
            | Parens
            | Brackets
            | Braces
            | Selection

  type movement = Left
                | Right
                | Up
                | Down
                | Start
                | End
                | PageUp
                | PageDown
                | FileStart
                | FileEnd

  val mode_to_string      : mode -> string
  val movement_to_string  : movement -> string
  val apply_movement      : selection_context -> mode -> movement -> Cursor.t -> unit

end
