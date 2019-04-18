(* main module for interacting with the terminal *)

val terminal_restore          : unit -> unit
val terminal_set_raw          : unit -> unit
val terminal_dimensions       : unit -> Util.Vec2.v2

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

(* TODO: add Framebuffer here *)
