
let fswatch_path = "/usr/bin/fswatch"

let watch_target = "./.ciseaurc"

let (stdin_r, stdin_w) = Unix.pipe ~cloexec:false ()

let (stdout_r, stdout_w) = Unix.pipe ~cloexec:false ()

let (stderr_r, stderr_w) = Unix.pipe ~cloexec:false ()

let fswatch_pid = Unix.create_process
  fswatch_path
  [|"-kr" ; "--event" ; "Updated" ; watch_target |]
  stdin_r
  stdout_w
  stderr_w

let watch_loop () =
  let stdout_ch = Unix.in_channel_of_descr stdout_r in
  set_binary_mode_in stdout_ch false ;
  while true do
    print_string (input_line stdout_ch) ;
    print_newline ()
  done

let watch_loop2 () =
  let watch_fs = [stdout_r, stderr_r] in
  while true do
    let (_, _, _) = Unix.select [] [] [] (-1.0) in
    (* WIP *)
    ()
  done

let _ =
  print_string (string_of_int fswatch_pid) ;
  print_newline () ;
  watch_loop ()
