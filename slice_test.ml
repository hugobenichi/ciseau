  let test _ =
    try
      Printexc.record_backtrace true ;

      let to_string fn slice =
        slice |> map fn |> to_array |> Array.to_list |> String.concat " ; "
      in
      let println s =
        print_string s ;
        print_newline ()
      in

      let a = [| 1 ; 2 ; 3 ; 4 ; 5 ; 6 |] in
      let s1 = wrap_array a in
      s1 |> len |> Printf.printf "%d\n" ;
      s1 |> reslice (3, 4) |> len |> Printf.printf "%d\n" ;
      s1 |> slice_left 3 |> len |> Printf.printf "%d\n" ;
      s1 |> slice_left 6 |> len |> Printf.printf "%d\n" ;

      print_newline () ;

      s1 |> iter print_int ; print_newline () ;
      s1 |> reslice (3, 4) |> iter print_int ; print_newline () ;
      s1 |> reslice (1, 4) |> iter print_int ; print_newline () ;
      s1 |> slice_left 3 |> iter print_int ; print_newline () ;
      s1 |> slice_left 6 |> iter print_int ; print_newline () ;

      print_newline () ;

      s1 |> map (fun x -> x * 2) |> to_string string_of_int |> println ;
      s1 |> reslice (3, 4) |> map (fun x -> x * 2) |> to_string string_of_int |> println ;
      s1 |> reslice (1, 4) |> map (fun x -> x * 2) |> to_string string_of_int |> println ;

      print_newline () ;

      s1 |> filter (fun x -> x mod 2 == 0) |> to_string string_of_int |> println ;
      s1 |> filter (fun x -> x < 4) |> to_string string_of_int |> println ;
      s1 |> filter (fun x -> x < 9) |> to_string string_of_int |> println ;
      s1 |> filter (fun x -> x > 9) |> to_string string_of_int |> println ;
      s1 |> reslice (1, 4) |> filter (fun x -> x mod 2 = 0) |> to_string string_of_int |> println ;

      print_newline () ;

      let s2 = clone s1 in
      set (slice_right 2 s2) 3 42 ;
      s1 |> to_string string_of_int |> println ;
      s2 |> to_string string_of_int |> println ;

      print_newline () ;

      let s3 = wrap_array [| 0 ; 0 ; 0 ; 0 ; 0 |] in
      s3 |> to_string string_of_int |> println ;
      copy s3 s1 ;
      s3 |> to_string string_of_int |> println ;
      [| 0 ; 0 ; 0 ; 0 ; 0 ; 0|] |> wrap_array |> copy s3 ;
      s3 |> to_string string_of_int |> println ;
      copy s3 (reslice (0, 2) s1) ;
      copy (slice_right 3 s3) (slice_right 3 s1) ;
      s3 |> to_string string_of_int |> println ;

      print_newline () ;

      [| 0 ; 0 ; 0 |] |> wrap_array |> append 1 |> append 2 |> append 3 |> to_string string_of_int |> println  ;
      [| 20 ; 30 ; 40 |] |> wrap_array |> reslice (1, 1) |> append 1 |> to_string string_of_int |> println  ;
      [| 20 ; 30 ; 40 |] |> wrap_array |> reslice (1, 2) |> append 1 |> to_string string_of_int |> println  ;
      [| 20 ; 30 ; 40 |] |> wrap_array |> reslice (1, 2) |> append 1 |> append 2 |> append 3 |> append 3 |> to_string string_of_int |> println  ;
      print_newline () ;

      [| 0 |]                 |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 |]             |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 ; 2 |]         |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 ; 2 ; 3 |]     |> wrap_array |> rev |> to_string string_of_int |> println ;
      [| 0 ; 1 ; 2 ; 3 ; 4 |] |> wrap_array |> rev |> to_string string_of_int |> println ;

      ()
  with
    e ->
        e |> Printexc.to_string |> Printf.printf "\nerror: %s\n" ;
        Printexc.print_backtrace stdout


