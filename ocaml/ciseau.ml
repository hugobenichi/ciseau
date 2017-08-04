(* TODOs:
 *  - print the 6x6x6 color gradients
 *  - put terminal in raw mode, do stuff, exist
 *  - in raw mode, capture and print input
 *  - get term size with $ stty size or $ tput cols and $ tput lines
 *)


(* remappings *)

let length = String.length ;;
let a_map = Array.map ;;
let a_fold = Array.fold_left ;;
let a_iter = Array.iter ;;

module IO = struct

  (* replacement for input_char which considers 0 as Enf_of_file *)
  let next_char =
    (* WARN not thread safe *)
    let buffer = Bytes.make 1 'c' in
      fun () ->
        Unix.read Unix.stdin buffer 0 1 |> ignore ; (* TODO check return value is 1 ! *)
        Bytes.get buffer 0 ;;

end


module Utils = struct

  (* fp utils *)

  let (>>) f g x = g (f x) ;;
  let (<<) g f x = g (f x) ;;

  (* string utils *)

  let padding l s = (String.make (l - (length s)) ' ') ;;
  let postpad l s = s ^ (padding l s) ;;
  let prepad l s = (padding l s) ^ s ;;

  let char_to_string c = String.make 1 c ;;

end



(* term utils *)

module Term = struct

  (* TODO turn these into proper enum and put inside module *)
  let black = 0 ;;
  let red = 1 ;;
  let green = 2 ;;
  let yellow = 3 ;;
  let blue = 4 ;;
  let magenta = 5 ;;
  let cyan = 6 ;;
  let white = 7 ;;


  let do_safely action =
    try (action () ; None)
    with e -> Some e ;;

  (* TODO: put in Control Sequences module *)
  let control_sequence_introducer = 27 |> Char.chr |> Utils.char_to_string ;;
  let ctrl_start = control_sequence_introducer ^ "[" ;;
  let ctrl_end = control_sequence_introducer ^ "[0m" ;;
  let ctrl_clear = control_sequence_introducer ^ "c" ;;
  let clear () = print_string ctrl_clear ;;
  let newline () = print_string "\r\n" ;;

  let term_fg c = 30 + c ;;
  let term_bg c = 40 + c ;;
  let term_rgb (r, g, b) = 16 + (36 * r) + (6 * g) + b ;;

  let rec term_print_code_seq = function
    | []      -> ()
    | i :: [] -> (print_int i ; print_char 'm' )
    | i :: t  -> (print_int i ; print_char ';' ; term_print_code_seq t)
  ;;

  let term_print codes s =
    print_string ctrl_start ;
    term_print_code_seq codes ;
    print_string s ;
    print_string ctrl_end
  ;;

  let term_print_color fg bg s =
    term_print [0 ; term_fg fg ; term_bg bg] s ;;

  let term_print_color256 fg bg s =
    term_print [38 ; 5 ; fg ; 48 ; 5 ; bg] s ;;

  (* TODO: support hex string like specifications like #ffee44 *)
  let term_print_color24b (fg_r, fg_g, fg_b) (bg_r, bg_g, bg_b) s =
    term_print [38 ; 2 ; fg_r; fg_g; fg_b ; 48 ; 2 ; bg_r; bg_g; bg_b] s ;;

  (* avoid warning #40 *)
  open Unix

  let set_raw_mode action =
    (* because terminal_io is a record of mutable fields, do tcgetattr twice:
       once for restoring later, once for setting the terminal to raw mode *)
    let initial = tcgetattr stdin in
    let want = tcgetattr stdin in
    (
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
      want.c_vtime   <- 100;      (* 100 * 100 ms timeout for reading input *)
                                  (* TODO: how to set a low timeout in order to process async IO results
                                               but not deal with the hassle of End_of_file from input_char ... *)
      want.c_csize   <- 8;        (* 8 bit chars *)

      tcsetattr stdin TCSAFLUSH want ;
      let error = do_safely action in (
        tcsetattr stdin TCSAFLUSH initial ;
        match error with
        | None    ->  ()
        | Some e  ->  e |> Printexc.to_string |> (^) "error: " |> print_string ;
                      print_newline () ;
                      stdout |> out_channel_of_descr |> Printexc.print_backtrace
                      (* TODO: flush me and close me !! *)
      ) ;
    )
  ;;

end


module ColorTable = struct

  open Term

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
      term_print_color bg black name ;
      print_string " -- " ;
      term_print [1 ; term_fg bg ; term_bg black] name ;
      print_string " -- " ;
      term_print_color fg bg name ;
      print_string " -- " ;
      term_print [1 ; term_fg fg ; term_bg bg] name ;
      print_newline ()
    in
    let maxlen = color_table
                  |> a_map (fun (x, _, _) -> length x)
                  |> a_fold max 0
    in
    let padded_color_table = a_map (fun (s, a, b) -> (Utils.postpad maxlen s, a, b)) color_table
    in
    a_iter print_one_color padded_color_table
  ;;

  let print_256_color_table () =
    (* base colors *)
    for c = 0 to 6 do
      term_print_color256 white c ("  " ^ (string_of_int c) ^ "  ")
    done ;
    term_print_color256 black 7 " 7 " ;
    (* base colors *)
    for c = 8 to 15 do
      term_print_color256 white c (" " ^ (string_of_int c) ^ " ")
    done ;
    print_newline () ;

    for c = 16 to 231 do
      if (c - 16) mod 36 = 0 then print_newline () ;
      term_print_color256 white c (" " ^ (Utils.prepad 3 (string_of_int c)) ^ " ")
    done ;

    (* gray scale *)
    print_newline () ;
    print_newline () ;
    for c = 232 to 255 do
      if c = 244 then print_newline () ;
      term_print_color256 white c (" " ^ (string_of_int c) ^ " ")
    done ;
    print_newline ()
  ;;

  let main () =
    clear () ;
    print_base_color_table () ;
    print_newline () ;
    print_256_color_table () ;
    print_newline () ;
    term_print_color24b (0, 204, 153) (242, 230, 255) " something in 24b colors " ;
    print_newline ()
  ;;

end


module RawModeExperiment = struct

  let action () =
      let rec loop () =
        let c = IO.next_char () in
        if Char.code c = 27 (* Escape *)
        then
            ()
        else
          print_char c ;
          print_string " " ;
          print_int (Char.code c) ;
          Term.newline () ;
          (* loop () ; *)
      in (
        (* clear () ; *)
        print_string "hello raw terminal" ;
        Term.newline () ;
        loop ()
      )
  ;;

  let main () =
    Term.set_raw_mode action ;;

end


let () =
  ColorTable.main () ;;
