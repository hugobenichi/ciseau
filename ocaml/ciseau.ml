(* TODOs:
 *  - print the 6x6x6 color gradients
 *  - put terminal in raw mode, do stuff, exist
 *  - in raw mode, capture and print input
 *  - get term size with $ stty size or $ tput cols and $ tput lines
 *)

(* fp utils *)

let (>>) f g x = g (f x) ;;
let (<<) g f x = g (f x) ;;


(* remappings *)

let length = String.length ;;
let a_map = Array.map ;;
let a_fold = Array.fold_left ;;
let a_iter = Array.iter ;;


(* term utils *)

(* turn these into proper enum and put inside module *)
let black = 0 ;;
let red = 1 ;;
let green = 2 ;;
let yellow = 3 ;;
let blue = 4 ;;
let magenta = 5 ;;
let cyan = 6 ;;
let white = 7 ;;

let term_control_sequence_introducer = 27 |> Char.chr |> String.make 1;;
let term_ctrl_start = term_control_sequence_introducer ^ "[" ;;
let term_ctrl_end = term_control_sequence_introducer ^ "[0m" ;;
let term_ctrl_clear = term_control_sequence_introducer ^ "c" ;;

let term_fg c = 30 + c ;;
let term_bg c = 40 + c ;;
let term_rgb (r, g, b) = 16 + (36 * r) + (6 * g) + b ;;

let term_clear () = print_string term_ctrl_clear ;;

let rec term_print_code_seq = function
  | []      -> ()
  | i :: [] -> (print_int i ; print_char 'm' )
  | i :: t  -> (print_int i ; print_char ';' ; term_print_code_seq t)
;;

let term_print codes s =
  print_string term_ctrl_start ;
  term_print_code_seq codes ;
  print_string s ;
  print_string term_ctrl_end
;;

let print_color fg bg s =
  term_print [0 ; term_fg fg ; term_bg bg] s ;;

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

let print_base_color_table () =
  let print_one_color (name, bg, fg) =
    print_color bg black name ;
    print_string " -- " ;
    term_print [1 ; term_fg bg ; term_bg black] name ;
    print_string " -- " ;
    print_color fg bg name ;
    print_string " -- " ;
    term_print [1 ; term_fg fg ; term_bg bg] name ;
    print_newline ()
  in
  let maxlen = color_table
                |> a_map (fun (x, _, _) -> length x)
                |> a_fold max 0
  in
  let pad s = s ^ (String.make (maxlen - (length s)) ' ')
  in
  let padded_color_table = a_map (fun (s, a, b) -> (pad s, a, b)) color_table
  in
  a_iter print_one_color padded_color_table
;;

let main () =
  term_clear () ;
  print_base_color_table () ;
  (* TODO: do 6x6x6 cubes plus grays
   * TODO: do 24bit colors
   *)
  term_print [0 ; 94 ; 42] "something\n" ;
  ()
;;

main ()
