(* main module for interacting with the terminal *)

(* Basic terminal manipulation funcions*)
val terminal_set_raw          : unit -> unit
val terminal_restore          : unit -> unit
val terminal_dimensions       : unit -> Util.Vec.vec2

module Keys : sig
  open Util

  type click =
      Left
    | Right
    | Middle
    | Release

  type key =
      Key of char
    | Click of Vec.vec2 * click
    | Escape_Z
    | ArrowUp
    | ArrowDown
    | ArrowRight
    | ArrowLeft
    | EINTR

  val key_to_string           : key -> string
  (* Returns a function that will block until the next input is available
   * That function is not reentrant and keeps state between input in order to *)
  val make_next_key_fn        : Unix.file_descr -> unit -> key  (* created unit -> key function is not reentrant and keeps state between inputs *)

  (* Stateful function for getting inputs from stdin. Not reentrant. *)
  val get_next_key : unit -> key
end

module Color : sig
  type color  = (* First 8 ansi colors *)
                Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenta
              | Cyan
              | White
                (* High contract 8 ansi colors *)
              | Bold_Black
              | Bold_Red
              | Bold_Green
              | Bold_Yellow
              | Bold_Blue
              | Bold_Magenta
              | Bold_Cyan
              | Bold_White
                (* Remaining colors from extended 256 colors mode *)
              | RGB216 of int * int * int
              | Gray of int

  type color_layer = Foreground | Background

  type color_cell = {
    fg : color ;
    bg : color ;
  }
end

module Framebuffer : sig
  open Util
  type t
  val mk_framebuffer    : Vec.vec2 -> t
  val framebuffer_size  : t -> Vec.vec2
  val render            : t -> unit
  val clear             : t -> unit
  (* Clear rectangle in framebuffer inclusive borders: left and top, exclusive borders: right and bottom *)
  val clear_rect        : t -> Rec.rec2 -> unit
  val clear_line        : t -> x:int -> y:int -> len:int -> unit
  val put_cursor        : t -> Vec.vec2 -> unit
  val put_line          : t -> x:int -> y:int -> ?offset:int -> ?len:int -> string -> unit
  (* Color rectangle in framebuffer,  inclusive borders: left and top, exclusive borders: right and bottom *)
  val put_color_rect    : t -> Color.color_cell -> Rec.rec2 -> unit
end

module Source : sig
  type t = {
    origin                : Util.Vec.vec2 ;
    size                  : Util.Vec.vec2 ;
    cursors               : Util.Vec.vec2 list ;
    lineno                : int ;
    get_line_length       : int -> int ;
    (* TOOD: doc me *)
    fill_line_by_segment  : lineno:int -> lineoffset:int -> byteoffset:int -> segmentlength:int -> Bytes.t -> unit ;
  }

  val draw_sources : Framebuffer.t -> t list -> unit
end
