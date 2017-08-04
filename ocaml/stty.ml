(* this file was just a minimal one-off experiment for setting the terminal in raw mode *)

module IO = struct

  (* replacement for input_char which considers 0 as Enf_of_file *)
  let next_char =
    (* WARN not thread safe *)
    let buffer = Bytes.make 1 'c' in
      fun () ->
        Unix.read Unix.stdin buffer 0 1 |> ignore ; (* TODO check return value is 1 ! *)
        Bytes.get buffer 0 ;;

end


module StringUtils = struct

  let char_to_string c = String.make 1 c ;;

end


module Term = struct

  let do_safely action =
    try (action () ; None)
    with e -> Some e ;;

  let control_sequence_introducer = 27 |> Char.chr |> StringUtils.char_to_string ;;
  let ctrl_clear = control_sequence_introducer ^ "c" ;;
  let clear () = print_string ctrl_clear ;;
  let newline () = print_string "\r\n" ;;

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

main () ;;
