(* compile with $ ocamlc unix.cma stty.ml -o teststty *)



(* works, but does not really work: I can get "size" to be passed in *)
(* let x = Unix.execv "/bin/stty" [| "size" |] *)

let stdin_chan  = stdin ;;
let stdout_chan = stdout ;;

let read_next_char () = input_char stdin_chan ;;

let read_next_char2 =
  (* not thread safe *)
  let buffer = Bytes.make 1 'c' in
    fun () ->
      Unix.read Unix.stdin buffer 0 1 |> ignore ; (* TODO check return value is 1 ! *)
      Bytes.get buffer 0 ;;

let do_safely action =
  try (action () ; None)
  with e -> Some e ;;

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

    print_string "before raw mode\n" ;
    tcsetattr stdin TCSAFLUSH term_want ;
    let error = do_safely action in (
      tcsetattr stdin TCSAFLUSH term_initial ;
      match error with
      | None    ->  ()
      | Some e  ->  e |> Printexc.to_string |> (^) "error: " |> print_string ;
                    print_newline () ;
                    stdout |> out_channel_of_descr |> Printexc.print_backtrace
                    (* flush me *)
                    (* TODO: close me !! *)
    ) ;
    print_string "after raw mode\n"
  )
;;

let action () =
    print_string "hello raw terminal\n" ;
    (* stdin |> in_channel_of_descr |> input_char |> print_char ; *)
    (* probably needs to do my own lowlvl read myself *)
    (* () |> read_next_char |> print_char ; *)
    () |> read_next_char2 |> print_char ;
    print_newline () ;;

term_set_raw_mode action ;;
