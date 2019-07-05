
(* main module for interacting with the terminal *)

val terminal_restore          : unit -> unit
val terminal_set_raw          : unit -> unit
val terminal_dimensions       : unit -> Util.Vec.vec2

module Keys : sig
  open Util

  type click =
      Left
    | Right
    | Middle

  type key =
      Key of char
    | Click of Vec.vec2 * click
    | ClickRelease of Vec.vec2
    | Escape_Z
    | ArrowUp
    | ArrowDown
    | ArrowRight
    | ArrowLeft
    | EINTR

  val descr_of                : key -> string
  val make_next_key_fn        : unit -> unit -> key  (* created unit -> key function is not reentrant and keeps state between inputs *)
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

  val color_code              : color_layer -> color -> int
  val color_code_to_string    : int -> string
end

module Framebuffer : sig
  open Util
  type t
  val mk_framebuffer    : Vec.vec2 -> t
  val framebuffer_size  : t -> Vec.vec2
  val clear             : t -> unit
  val clear_rect        : t -> Rec.rec2 -> unit
  val clear_line        : t -> x:int -> y:int -> len:int -> unit
  val render            : t -> unit
  val put_color_rect    : t -> Color.color_cell -> Rec.rec2 -> unit
  (* TODO: add a put_color_segment function *)
  val put_cursor        : t -> Vec.vec2 -> unit
  val put_line          : t -> x:int -> y:int -> ?offset:int -> ?len:int -> string -> unit
  val put_framebuffer   : t -> Rec.rec2 -> t -> unit
end

(* Is this interface useful for drawing ? how to pass colors, lines segments
module Drawview : sig
  type t
  val mk_drawview       : Rec.rec2 -> Framebuffer.t -> t
  val iter_lines        : t -> (i -> string) -> unit
end
*)
