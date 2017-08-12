(* TODOs:
 *  - implement terminal save and restore
 *  - map character values to character table
 *  - get term size with $ stty size or $ tput cols and $ tput lines
 *
 *  - primitive for bliting some text in a rectangle somewhere
 *)


(* remappings *)

let length = String.length ;;
let a_map = Array.map ;;
let a_fold = Array.fold_left ;;
let a_iter = Array.iter ;;

let l_iter = List.iter ;;


module IO = struct

  exception Timeout ;;
  exception IOError ;;
  exception ReadTooMuch of int ;;
  exception WroteNotEnough of int ;;

  (* replacement for input_char which considers 0 as Enf_of_file *)
  let next_char =
    (* WARN not thread safe *)
    let buffer = Bytes.make 1 'z' in
    let rec one_byte_reader () =
      match Unix.read Unix.stdin buffer 0 1 with
      | 1   -> Bytes.get buffer 0
      | 0   -> one_byte_reader ()     (* timeout *)
      | -1  -> raise IOError          (* TODO: errno *)
      | n   -> raise (ReadTooMuch n)  (* cannot happen since we ask for 1 byte only *)
    in one_byte_reader ;;

  let write fd buffer len =
    match Unix.write fd buffer 0 len with
    | n when n = len  -> ()
    | -1              -> raise IOError
    | n               -> raise (WroteNotEnough n)
  ;;

  let write_string fd s =
    let buffer = (Bytes.of_string s) in
    write fd buffer (Bytes.length buffer)
  ;;

  (* TODO: exception handling *)
  let do_with_input_file chan fn =
    let r = fn chan in (
      close_in chan ;
      r
    ) ;;

  let do_with_output_file chan fn =
    let r = fn chan in (
      close_out chan ;
      r
    ) ;;

  let slurp f =
    let rec loop lines ch = match input_line ch with
    | s -> loop (s :: lines) ch
    | exception End_of_file -> List.rev lines
    in
    do_with_input_file (open_in f) (loop []) ;;

  let save f lines = "todo"

end


module Utils = struct

  type 'a either = Left of 'a | Right of exn ;;

  let try_to action =
    try let x = action () in Left x
    with e -> Right e
  ;;

  let try_finally action cleanup =
    let rez = try_to action in
    cleanup () ;
    match rez with
    | Left success -> success
    | Right error -> raise error
  ;;

  (* fp utils *)

  let (>>) f g x = g (f x) ;;
  let (<<) g f x = g (f x) ;;

  (* string utils *)

let padding l s = (String.make (l - (length s)) ' ') ;;
let postpad l s = s ^ (padding l s) ;;
let prepad l s = (padding l s) ^ s ;;

let string_of_char c = String.make 1 c ;;

end


module Bytevector = struct

  type t = {
    bytes : bytes ;
    len : int ;
  } ;;

  let zero = Char.chr 0 ;;

  let init len = {
    bytes = Bytes.make len zero ;
    len   = 0 ;
  } ;;

  let reset bytevec = { bytevec with len = 0 } ;;

  let scale size = size |> float |> ( *. ) 1.45 |> ceil |> truncate ;;

  let rec next_size needed_size size =
    if needed_size <= size then size else next_size needed_size (scale size) ;;

  let grow new_size bytes = Bytes.extend bytes 0 (new_size - (Bytes.length bytes)) ;;

  let ensure_size needed_size bytes =
    let current_size = (Bytes.length bytes) in
    if (needed_size <= current_size)
      then bytes
      else grow (next_size needed_size current_size) bytes
  ;;

  let append s t =
    let new_length = (String.length s) + t.len in
    let new_bytes = ensure_size new_length t.bytes in
      Bytes.blit_string s 0 new_bytes t.len (String.length s) ;
      {
        bytes = new_bytes ;
        len   = new_length ;
      }
  ;;

  let cat t bvec =
    let new_length = t.len + bvec.len in
    let new_bytes = ensure_size new_length t.bytes in
      Bytes.blit bvec.bytes 0 new_bytes t.len bvec.len ;
      {
        bytes = new_bytes ;
        len   = new_length ;
      }
  ;;

  let to_string bvec =
    Bytes.sub_string bvec.bytes 0 bvec.len ;;

  let write fd bytev =
    IO.write fd bytev.bytes bytev.len ;;

end

(* main module for interacting with the terminal *)
module Term = struct

  open Utils

  (* bypass buffered output to the stdout *FILE, use direct write() instead *)
  let print_string = IO.write_string Unix.stdout ;;
  let print_int = string_of_int >> print_string ;;
  let print_char = string_of_char >> print_string ;;

  (* TODO turn these into proper enum and put inside module *)
  let black = 0 ;;
  let red = 1 ;;
  let green = 2 ;;
  let yellow = 3 ;;
  let blue = 4 ;;
  let magenta = 5 ;;
  let cyan = 6 ;;
  let white = 7 ;;


  (* TODO: put in Control Sequences module *)
  let control_sequence_introducer = 27 |> Char.chr |> string_of_char ;;
  let ctrl_start = control_sequence_introducer ^ "[" ;;
  let ctrl_end = control_sequence_introducer ^ "[0m" ;;
  let ctrl_clear = control_sequence_introducer ^ "c" ;;
  let clear () = print_string ctrl_clear ;;
  let newline () = print_string "\r\n" ;;
  let ctrl_cursor_hide = ctrl_start ^ "?25l" ;;
  let ctrl_cursor_show = ctrl_start ^ "?25h" ;;

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

  type terminal = {
    buffer : Bytevector.t ;
  } ;;

  let term_init len = {
    buffer = Bytevector.init len ;
  } ;;

  let term_reset term = {
    buffer = Bytevector.reset term.buffer ;
  }

  let term_append s term = {
    buffer = Bytevector.append s term.buffer;
  } ;;

  let term_clear term =
    term |> term_reset |> term_append ctrl_cursor_show |> term_append ctrl_clear ;;

  let term_set_cursor x y term =
    let cursor_ctrl_string = (ctrl_start ^ (string_of_int y) ^ ";" ^ (string_of_int x) ^ "H") in
    term_append cursor_ctrl_string term
  ;;

  let term_newline term = term_append "\r\n" term ;;

  let term_flush term =
    term |> term_append ctrl_cursor_show |> (fun bvec -> bvec.buffer) |> Bytevector.write Unix.stdout ;
    term ;;

(* avoid warning #40 *)
open Unix

  let do_with_raw_mode action =
    (* because terminal_io is a record of mutable fields, do tcgetattr twice:
       once for restoring later, once for setting the terminal to raw mode *)
    let initial = tcgetattr stdin in
    let want    = tcgetattr stdin in
    (
      want.c_brkint  <- false ;   (* no break *)
      want.c_icrnl   <- false ;   (* no CR to NL *)
      want.c_inpck   <- false ;   (* no parity check *)
      want.c_istrip  <- false ;   (* no strip character *)
      want.c_ixon    <- false ;
      want.c_opost   <- false ;
      want.c_echo    <- false ;
      want.c_icanon  <- false ;
      (* for the time being, we let SIGINT kill the program *)
      (* want.c_isig    <- false ;   (* no INTR, QUIT, SUSP signals *) *)
      want.c_vmin    <- 0;        (* return each byte one by one, or 0 if timeout *)
      want.c_vtime   <- 100;      (* 100 * 100 ms timeout for reading input *)
                                  (* TODO: how to set a low timeout in order to process async IO results
                                               but not deal with the hassle of End_of_file from input_char ... *)
      want.c_csize   <- 8;        (* 8 bit chars *)

      tcsetattr stdin TCSAFLUSH want ;
      Utils.try_finally action (fun () -> tcsetattr stdin TCSAFLUSH initial)
    ) ;;

end


(* demo for printing color tables *)
(* TODO: move to separate file *)
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


(* TODO: move to separate file *)
module RawModeExperiment = struct

  let rec loop () =
    let c = IO.next_char () in (
    (* if Char.code c != 27 (* Escape *) *)
    (* then ( *)
      Term.print_char c ;
      Term.print_string " " ;
      Term.print_int (Char.code c) ;
      Term.newline ()
      ; loop ()
    (* ) *)
    ) ;;

  let action () =
    (* clear () ; *)
    Term.print_string "hello raw terminal" ;
    Term.newline () ;
    (* not flushed here *)
    loop ()
  ;;

  let main () =
    Term.do_with_raw_mode action ;;

end


module Files = struct

  let main () =
    let lines = IO.slurp __FILE__ in
    let println s = (print_string s ; print_newline () ) in
    l_iter println lines ;;

end


module CiseauPrototype = struct

  (* TODO: cursor position, window size *)
  type editor = {
    term : Term.terminal ;

    file_buffer : string list ; (* TODO replace with array ? *)
    file_offset : int ;

    cursor_x : int ;
    cursor_y : int ;

    header : string ;
    status : string ;
    running : bool ;

    (* TODO: queyr using call into C for direct access to ioctl ... *)
    width : int;
    height : int;
  } ;;

  let default_status = "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find (not implemented)" ;;

  let init () : editor = {
    term = Term.term_init 0x1000 ;

    file_buffer   = IO.slurp __FILE__ ;
    file_offset = 0 ;

    (* TODO: instead of taking cursor position relative to screen, take it relative to the file_buffer *)
    cursor_x = 0 ;
    cursor_y = 1 ;

    (* file_buffer   = IO.slurp Sys.argv.(1) ; *)
    header  = "Ciseau editor -- version 0" ;
    status  = default_status ;
    running  = true ;

    width = 118 ;
    height = 59 ;
  } ;;

  (* one line for header, one line for status, one line for user input *)
  let usage_screen_height editor = editor.height - 3 ;;

  let rec skip n ls = match (n, ls) with
  | (0, _)      -> ls
  | (n, [])     -> []
  | (n, _ :: t) -> skip (n - 1) t
  ;;

  let rec print_file_buffer max_len lines term = match (max_len, lines) with
  | (0, _)      ->  Term.term_newline term
  | (_, [])     ->  Term.term_newline term
  | (n, h :: t) ->  term |> Term.term_newline |> Term.term_append h |> print_file_buffer (n - 1) t
  ;;

  (* TODO: setting cursor position causes flickering *)
  let refresh_screen editor =
    let new_term = editor.term |> Term.term_clear
                               |> Term.term_append editor.header
                               |> print_file_buffer (usage_screen_height editor)
                                                    (skip editor.file_offset editor.file_buffer)
                                  (* skip remaining space, or fill with void *)
                               |> Term.term_append editor.status
                               |> Term.term_set_cursor editor.cursor_x editor.cursor_y
                               |> Term.term_flush
    in
      { editor with term = new_term }
  ;;

  let clamp (min, max) x =
    if x < min then min else if max < x then max else x ;;

  let process_key editor key =
    match key with
    | 65 (* up arrow *)     -> { editor with cursor_y = clamp (0, editor.height - 1) (editor.cursor_y - 1) }
    | 66 (* down arrow *)   -> { editor with cursor_y = clamp (0, editor.height - 1) (editor.cursor_y + 1) }
    | 67 (* right arrow *)  -> { editor with cursor_x = clamp (0, editor.width - 1) (editor.cursor_x + 1) }
    | 68 (* left arrow *)   -> { editor with cursor_x = clamp (0, editor.width - 1) (editor.cursor_x - 1) }
    | _ -> editor (* ignore for now *)
  ;;

  let process_events editor =
    let c = IO.next_char () in
    let editor' = process_key editor (Char.code c) in
    { editor' with
      status = default_status
             ^ "  last input: "
             ^ (Utils.string_of_char c)
             ^ " " ^ (c |> Char.code |> string_of_int) ;
    } ;;

  let rec loop editor =
    if editor.running then
      editor |> refresh_screen |> process_events |> loop
    ;;

  let run_loop editor () = loop editor ;;

  let main () =
    () |> init |> run_loop |> Term.do_with_raw_mode
  ;;

end

let () =
  (* ColorTable.main () ; *)
  (* RawModeExperiment.main () *)
  (* Files.main () *)
  CiseauPrototype.main ()
  (* () *)
;;
