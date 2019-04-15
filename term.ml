open Util

external get_terminal_size : unit -> (int * int) = "get_terminal_size"

let terminal_dimensions () =
  let (term_rows, term_cols) = get_terminal_size () in
  Vec2.mk_v2 term_cols term_rows

let stdout_write_string s =
  let l = slen s in
  let n = Unix.write_substring Unix.stdout s 0 l in
  if l <> n then fail ("sdtout write failed for " ^ s)

(* Used for restoring terminal state at program exit *)
let terminal_initial = Unix.tcgetattr Unix.stdin

let terminal_restore () =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH terminal_initial ;
  stdout_write_string "\027[?1000l" ; (* mouse event off *)
  stdout_write_string "\027[?1002l" ; (* mouse tracking off *)
  stdout_write_string "\027[?1004l" ; (* switch focus event off *)
  stdout_write_string "\027[?47l" ; (* switch back to main screen *)
  stdout_write_string "\027[u" (* cursor restore *)

let terminal_set_raw () =
  let want = Unix.tcgetattr Unix.stdin in
  want.c_brkint  <- false ;   (* no break *)
  want.c_icrnl   <- false ;   (* no CR to NL *)
  want.c_inpck   <- false ;   (* no parity check *)
  want.c_istrip  <- false ;   (* no strip character *)
  want.c_ixon    <- false ;
  want.c_opost   <- false ;
  want.c_echo    <- false ;
  want.c_icanon  <- false ;
  want.c_isig    <- false ;   (* no INTR, QUIT, SUSP signals *)
  want.c_vmin    <- 0;        (* return each byte one by one, or 0 if timeout *)
  want.c_vtime   <- 1;        (* 1 * 100 ms timeout for reading input *)
  want.c_csize   <- 8;        (* 8 bit chars *)
  stdout_write_string "\027[s" ;      (* cursor save *)
  stdout_write_string "\027[?47h" ;   (* switch offscreen *)
  stdout_write_string "\027[?1000h" ; (* mouse event on *)
  stdout_write_string "\027[?1002h" ; (* mouse tracking on *)
  (* stdout_write_string "\027[?1004h" ; *) (* TODO: enable, switch focus event off *)
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH want

module Color = struct

  type color  = Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenta
              | Cyan
              | White
              | Bold_Black
              | Bold_Red
              | Bold_Green
              | Bold_Yellow
              | Bold_Blue
              | Bold_Magenta
              | Bold_Cyan
              | Bold_White
              | RGB216 of int * int * int
              | Gray of int

  type color_layer = Foreground | Background

  let color_code_raw =
    function
      | Black           -> 0
      | Red             -> 1
      | Green           -> 2
      | Yellow          -> 3
      | Blue            -> 4
      | Magenta         -> 5
      | Cyan            -> 6
      | White           -> 7
      | Bold_Black      -> 8
      | Bold_Red        -> 9
      | Bold_Green      -> 10
      | Bold_Yellow     -> 11
      | Bold_Blue       -> 12
      | Bold_Magenta    -> 13
      | Bold_Cyan       -> 14
      | Bold_White      -> 15
      | Gray g          -> assert_that (0 <= g && g < 24) ;
                           232 + g
      | RGB216 (r,g,b)  -> assert_that (0 <= r && r < 6) ;
                           assert_that (0 <= g && g < 6) ;
                           assert_that (0 <= b && b < 6) ;
                           16 + 36 * r + 6 * g + b

  let color_code =
    function
      | Foreground -> color_code_raw
      | Background -> color_code_raw >> ((+) 0 (*256*))

  type color_cell = {
    fg : color ;
    bg : color ;
  }

  let darkgray  = Gray 2

  let white_code      = color_code Foreground White
  let darkgray_code   = color_code Foreground (Gray 2)
  let black_code      = color_code Foreground Black

  let fg_color_control_strings = Array.init 256 (Printf.sprintf "38;5;%d")
  let bg_color_control_strings = Array.init 256 (Printf.sprintf ";48;5;%dm")
  let fg_color_command = Arrays.array_get fg_color_control_strings
  let bg_color_command = Arrays.array_get bg_color_control_strings
end
