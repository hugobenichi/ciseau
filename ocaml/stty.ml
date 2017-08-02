(* compile with $ ocamlc unix.cma stty.ml -o teststty *)



(* works, but does not really work: I can get "size" to be passed in *)
(* let x = Unix.execv "/bin/stty" [| "size" |] *)

let stdin_chan  = stdin ;;
let stdout_chan = stdout ;;

let read_next_char =
  (* replacement for input_char which considers 0 as Enf_of_file *)
  (* WARN not thread safe *)
  let buffer = Bytes.make 1 'c' in
    fun () ->
      Unix.read Unix.stdin buffer 0 1 |> ignore ; (* TODO check return value is 1 ! *)
      Bytes.get buffer 0 ;;

let do_safely action =
  try (action () ; None)
  with e -> Some e ;;


let char_to_string c = String.make 1 c ;;
let term_control_sequence_introducer = 27 |> Char.chr |> char_to_string ;;
let term_ctrl_clear = term_control_sequence_introducer ^ "c" ;;
let term_clear () = print_string term_ctrl_clear ;;
let term_newline () = print_string "\r\n" ;;

(* avoid warning #40 *)
open Unix

let term_set_raw_mode action =
  (* because terminal_io is a record of mutable fields, do tcgetattr twice:
     once for restoring later, once for setting the terminal to raw mode *)
  let term_initial = tcgetattr stdin in
  let term_want = tcgetattr stdin in
  (
    term_want.c_brkint  <- false ;  (* no break *)
    term_want.c_icrnl   <- false ;  (* no CR to NL *)
    term_want.c_inpck   <- false ;  (* no parity check *)
    term_want.c_istrip  <- false ;  (* no strip character *)
    term_want.c_ixon    <- false ;
    term_want.c_opost   <- false ;
    term_want.c_echo    <- false ;
    term_want.c_icanon  <- false ;
    term_want.c_isig    <- false ;  (* no INTR, QUIT, SUSP signals *)
    term_want.c_vmin    <- 0;       (* return each byte one by one, or 0 if timeout *)
    term_want.c_vtime   <- 100;     (* 100 * 100 ms timeout for reading input *)
                                    (* TODO: how to set a low timeout in order to process async IO results
                                             but not deal with the hassle of End_of_file from input_char ... *)
    term_want.c_csize   <- 8;       (* 8 bit chars *)

    print_string "before raw mode" ;
    print_newline () ;
    tcsetattr stdin TCSAFLUSH term_want ;
    let error = do_safely action in (
      tcsetattr stdin TCSAFLUSH term_initial ;
      match error with
      | None    ->  ()
      | Some e  ->  e |> Printexc.to_string |> (^) "error: " |> print_string ;
                    print_newline () ;
                    stdout |> out_channel_of_descr |> Printexc.print_backtrace
                    (* TODO: flush me and close me !! *)
    ) ;
    print_string "after raw mode" ;
    print_newline ()
  )
;;

let action () =
    let rec loop () =
      let c = read_next_char () in
      if Char.code c = 27 (* Escape *)
      then
          ()
      else
        print_char c ;
        print_string " " ;
        print_int (Char.code c) ;
        term_newline () ;
        (* loop () *)
    in (
      (* term_clear () ; *)
      print_string "hello raw terminal" ;
      term_newline () ;
      loop ()
    )
;;

term_set_raw_mode action ;;
