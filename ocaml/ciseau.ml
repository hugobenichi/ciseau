(* TODOs:
 *  - print the 6x6x6 color gradients
 *  - put terminal in raw mode, do stuff, exist
 *  - in raw mode, capture and print input
 *  - get term size with $ stty size or $ tput cols and $ tput lines
 *)

let black = 0 ;;
let red = 1 ;;
let green = 2 ;;
let yellow = 3 ;;
let blue = 4 ;;
let magenta = 5 ;;
let cyan = 6 ;;
let white = 7 ;;

let term_esc = 27 |> Char.chr |> String.make 1;;
let term_esc_start = term_esc ^ "[" ;;
let term_esc_end = term_esc ^ "[0m\n" ;;

let rec term_print_code_seq = function
  | []      -> ()
  | i :: [] -> (print_int i ; print_char 'm' )
  | i :: t  -> (print_int i ; print_char ';' ; term_print_code_seq t)
;;

let term_print codes s =
  print_string term_esc_start ;
  term_print_code_seq codes ;
  print_string s ;
  print_string term_esc_end
;;

let print_color fg bg s =
  term_print [0 ; fg + 30 ; bg + 40] s ;;

let term_rgb (r, g, b) =
  16 + (36 * r) + (6 * g) + b
;;

let color_table = [|
  ("black", black, white) ;
  ("red", red, white) ;
  ("green", green, white) ;
  ("yellow", yellow, black) ;
  ("blue", blue, white) ;
  ("magenta", magenta, white) ;
  ("cyan", cyan, white) ;
  ("white", white, black) ;
|] ;;

let print_color_table () =
  Array.iter (fun (name, bg, fg) -> print_color fg bg name) color_table
;;

let main () =
  print_color_table () ;
  term_print [0 ; 94 ; 42] "something" ;
  ()
;;

main ()
