(* next TODOs:
 *  - do tab expansion and \r\n stripping
 *  - cleanup: remove all the view management code from filebuffer, this should be done somewhere else
 *  - implement nested screens and bounded screens
 *  - do screens + filebuffer swapping (first several screens into the same filebuffer)
 *      data layout would be file_view = struct { screen (* for drawing *) ; filebuffer (* the source *) }
 *      if several screen mutates the same filebuffer content, they need either a handle
 *)


let logs = open_out "/tmp/ciseau.log"

let this_is_a_value2345 = "this is a string" ^ (("this is another string" |> String.sub) 0 5)

let some x = Some x

let alen = Array.length ;;
let blen = Bytes.length ;;
let slen = String.length ;;

let inc x = x + 1 ;;
let dec x = x - 1 ;;

let try_finally action cleanup =
  let rez =
    try Ok (action ()) with e -> Error e
  in
    cleanup () ;
    match rez with
    | Ok success  -> success
    | Error error -> raise error

let (>>) f g x = g (f x)

let string_of_char c = String.make 1 c

let truncate_string l s =
  if slen s > l then String.sub s 0 l else s

let is_empty s = (slen s = 0)

let is_space      chr = (chr = ' ') || (chr = '\t') || (chr = '\r') || (chr = '\n') ;;
let is_letter     chr = (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z')) ;;
let is_digit      chr = ('0' <= chr) && (chr <= '9') ;;
let is_alphanum   chr = (is_digit chr) || (is_letter chr) ;;
let is_printable  chr = (' ' <= chr) && (chr <= '~') ;;

(* replacement for input_char which considers 0 as Enf_of_file *)
let next_char =
  (* WARN not thread safe *)
  let buffer = Bytes.make 1 'z' in
  let rec one_byte_reader () =
    match Unix.read Unix.stdin buffer 0 1 with
    | 1   -> Bytes.get buffer 0 |> Char.code
    | 0   -> one_byte_reader ()     (* timeout *)
    | _   -> raise (Failure "next_char failed")
  in one_byte_reader

let write fd buffer len =
  if Unix.write fd buffer 0 len <> len then raise (Failure "fd write failed")

let slurp f =
  let rec loop lines ch =
    match input_line ch with
    | s                     -> loop (s :: lines) ch
    | exception End_of_file -> List.rev lines
  in
  let ch = open_in f in
  let action () = loop [] ch in
  let cleanup () = close_in ch in
  try_finally action cleanup




(*
 *  -> tokenizer that takes lines as strings and breaks them down in base atoms
 *  -> layering function that takes a list of atoms and returns another list of grouping atoms
 *
 *  possible block operations
 *    insert, delete, get by index
 *    swap, move,
 *    find next, previous,
 *    find first for condition C
 *)

module Atom = struct

  type atom_kind = Text | Digit | Spacing | Operator | Structure | Line | Control | Other | Ending

  let atom_kind_to_string = function
    | Text      -> "Text"
    | Digit     -> "Digit"
    | Spacing   -> "Spacing"
    | Operator  -> "Operator"
    | Structure -> "Structure"
    | Line      -> "Line"
    | Control   -> "Control"
    | Other     -> "Other"
    | Ending    -> "Ending"

  let atom_kind_to_string_padded = function
    | Text      -> "Text      "
    | Digit     -> "Digit     "
    | Spacing   -> "Spacing   "
    | Operator  -> "Operator  "
    | Structure -> "Structure "
    | Line      -> "Line      "
    | Control   -> "Control   "
    | Other     -> "Other     "
    | Ending    -> "Ending    "

  let atom_kind_table = Array.make 128 Other ;;
  (* control codes *)
  for c = 0 to 31 do Array.set atom_kind_table c Control done ;;
  atom_kind_table.(009 (* horizontal tab *) ) = Spacing ;;
  atom_kind_table.(010 (* line feed *) ) = Line ;;
  atom_kind_table.(013 (* carriage return *) ) = Line ;;
  (* printable codes *)
  atom_kind_table.(032 (* ' ' *) ) <- Spacing ;;
  atom_kind_table.(033 (* '!' *) ) <- Operator ;;
  atom_kind_table.(034 (* '"' *) ) <- Structure ;;
  atom_kind_table.(035 (* '#' *) ) <- Operator ;;
  atom_kind_table.(036 (* '$' *) ) <- Operator ;;
  atom_kind_table.(037 (* '%' *) ) <- Operator ;;
  atom_kind_table.(038 (* '&' *) ) <- Operator ;;
  atom_kind_table.(039 (* ''' *) ) <- Structure ;;
  atom_kind_table.(040 (* '(' *) ) <- Structure ;;
  atom_kind_table.(041 (* ')' *) ) <- Structure ;;
  atom_kind_table.(042 (* '*' *) ) <- Operator ;;
  atom_kind_table.(043 (* '+' *) ) <- Operator ;;
  atom_kind_table.(044 (* ',' *) ) <- Operator ;;
  atom_kind_table.(045 (* '-' *) ) <- Operator ;;
  atom_kind_table.(046 (* '.' *) ) <- Operator ;;
  atom_kind_table.(047 (* '/' *) ) <- Operator ;;
  atom_kind_table.(048 (* '0' *) ) <- Digit ;;
  atom_kind_table.(049 (* '1' *) ) <- Digit ;;
  atom_kind_table.(050 (* '2' *) ) <- Digit ;;
  atom_kind_table.(051 (* '3' *) ) <- Digit ;;
  atom_kind_table.(052 (* '4' *) ) <- Digit ;;
  atom_kind_table.(053 (* '5' *) ) <- Digit ;;
  atom_kind_table.(054 (* '6' *) ) <- Digit ;;
  atom_kind_table.(055 (* '7' *) ) <- Digit ;;
  atom_kind_table.(056 (* '8' *) ) <- Digit ;;
  atom_kind_table.(057 (* '9' *) ) <- Digit ;;
  atom_kind_table.(058 (* ':' *) ) <- Operator ;;
  atom_kind_table.(059 (* ';' *) ) <- Operator ;;
  atom_kind_table.(060 (* '<' *) ) <- Operator ;;
  atom_kind_table.(061 (* '=' *) ) <- Operator ;;
  atom_kind_table.(062 (* '>' *) ) <- Operator ;;
  atom_kind_table.(063 (* '?' *) ) <- Operator ;;
  atom_kind_table.(064 (* '@' *) ) <- Operator ;;
  atom_kind_table.(065 (* 'A' *) ) <- Text ;;
  atom_kind_table.(066 (* 'B' *) ) <- Text ;;
  atom_kind_table.(067 (* 'C' *) ) <- Text ;;
  atom_kind_table.(068 (* 'D' *) ) <- Text ;;
  atom_kind_table.(069 (* 'E' *) ) <- Text ;;
  atom_kind_table.(070 (* 'F' *) ) <- Text ;;
  atom_kind_table.(071 (* 'G' *) ) <- Text ;;
  atom_kind_table.(072 (* 'H' *) ) <- Text ;;
  atom_kind_table.(073 (* 'I' *) ) <- Text ;;
  atom_kind_table.(074 (* 'J' *) ) <- Text ;;
  atom_kind_table.(075 (* 'K' *) ) <- Text ;;
  atom_kind_table.(076 (* 'L' *) ) <- Text ;;
  atom_kind_table.(077 (* 'M' *) ) <- Text ;;
  atom_kind_table.(078 (* 'N' *) ) <- Text ;;
  atom_kind_table.(079 (* 'O' *) ) <- Text ;;
  atom_kind_table.(080 (* 'P' *) ) <- Text ;;
  atom_kind_table.(081 (* 'Q' *) ) <- Text ;;
  atom_kind_table.(082 (* 'R' *) ) <- Text ;;
  atom_kind_table.(083 (* 'S' *) ) <- Text ;;
  atom_kind_table.(084 (* 'T' *) ) <- Text ;;
  atom_kind_table.(085 (* 'U' *) ) <- Text ;;
  atom_kind_table.(086 (* 'V' *) ) <- Text ;;
  atom_kind_table.(087 (* 'W' *) ) <- Text ;;
  atom_kind_table.(088 (* 'X' *) ) <- Text ;;
  atom_kind_table.(089 (* 'Y' *) ) <- Text ;;
  atom_kind_table.(090 (* 'Z' *) ) <- Text ;;
  atom_kind_table.(091 (* '[' *) ) <- Structure ;;
  atom_kind_table.(092 (* '\' *) ) <- Operator ;;
  atom_kind_table.(093 (* ']' *) ) <- Structure ;;
  atom_kind_table.(094 (* '^' *) ) <- Operator ;;
  atom_kind_table.(095 (* '_' *) ) <- Text ;;
  atom_kind_table.(096 (* '`' *) ) <- Operator ;;
  atom_kind_table.(097 (* 'a' *) ) <- Text ;;
  atom_kind_table.(098 (* 'b' *) ) <- Text ;;
  atom_kind_table.(099 (* 'c' *) ) <- Text ;;
  atom_kind_table.(100 (* 'd' *) ) <- Text ;;
  atom_kind_table.(101 (* 'e' *) ) <- Text ;;
  atom_kind_table.(102 (* 'f' *) ) <- Text ;;
  atom_kind_table.(103 (* 'g' *) ) <- Text ;;
  atom_kind_table.(104 (* 'h' *) ) <- Text ;;
  atom_kind_table.(105 (* 'i' *) ) <- Text ;;
  atom_kind_table.(106 (* 'j' *) ) <- Text ;;
  atom_kind_table.(107 (* 'k' *) ) <- Text ;;
  atom_kind_table.(108 (* 'l' *) ) <- Text ;;
  atom_kind_table.(109 (* 'm' *) ) <- Text ;;
  atom_kind_table.(110 (* 'n' *) ) <- Text ;;
  atom_kind_table.(111 (* 'o' *) ) <- Text ;;
  atom_kind_table.(112 (* 'p' *) ) <- Text ;;
  atom_kind_table.(113 (* 'q' *) ) <- Text ;;
  atom_kind_table.(114 (* 'r' *) ) <- Text ;;
  atom_kind_table.(115 (* 's' *) ) <- Text ;;
  atom_kind_table.(116 (* 't' *) ) <- Text ;;
  atom_kind_table.(117 (* 'u' *) ) <- Text ;;
  atom_kind_table.(118 (* 'v' *) ) <- Text ;;
  atom_kind_table.(119 (* 'w' *) ) <- Text ;;
  atom_kind_table.(120 (* 'x' *) ) <- Text ;;
  atom_kind_table.(121 (* 'y' *) ) <- Text ;;
  atom_kind_table.(122 (* 'z' *) ) <- Text ;;
  atom_kind_table.(123 (* '{' *) ) <- Structure ;;
  atom_kind_table.(124 (* '|' *) ) <- Operator ;;
  atom_kind_table.(125 (* '}' *) ) <- Structure ;;
  atom_kind_table.(126 (* '~' *) ) <- Operator ;;

  let atom_kind_of = Char.code >> Array.get atom_kind_table

  let atom_kind_at s i =
    if i < slen s then (String.get s i) |> atom_kind_of else Ending

  type atom = {
    kind  : atom_kind;
    line  : string;
    start : int;
    stop  : int;
  }

  let atom_to_string a =
    String.sub a.line a.start (a.stop - a.start)

  let atom_to_pretty_string a =
    Printf.sprintf "%s:'%s'" (atom_kind_to_string_padded a.kind) (atom_to_string a)

  let should_continue_atom = function
    | (_, Ending) -> false
    | (Text , Digit ) -> true
    | (Digit , Text) -> true
    | (Structure , _ ) -> false
    | (current , next ) when current = next -> true
    | _ -> false

  let rec tokenize_atoms all_atoms kind start index line =
    if kind = Ending
    then List.rev all_atoms
    else
      let next_kind = atom_kind_at line index in
      if should_continue_atom (kind, next_kind)
      then tokenize_atoms all_atoms kind start (index + 1) line
      else
        let a = {
          kind  = kind ;
          line  = line ;
          start = start ;
          stop  = index ;
        } in
        tokenize_atoms (a :: all_atoms) next_kind index (index + 1) line

  let generic_atom_parser line =
    tokenize_atoms [] (atom_kind_at line 0) 0 1 line
end


module Vec2 = struct

  type v2 = {
    x : int ;
    y : int ;
  }

  let v2_zero = {
    x = 0 ;
    y = 0 ;
  }

  let v2_of_xy x y = {
    x = x ;
    y = y ;
  }

  let (<+>) t1 t2 = {
    x = t1.x + t2.x ;
    y = t1.y + t2.y ;
  }

  let (<->) t1 t2 = {
    x = t1.x - t2.x ;
    y = t1.y - t2.y ;
  }

  let v2_to_string t = Printf.sprintf "%d,%d" t.y t.x

  let v2_to_offset stride vec2 = vec2.y * stride + vec2.x

  let offset_to_v2 stride offset = v2_of_xy (offset mod stride) (offset / stride)

end


open Vec2


(* Mappings of character codes
 * char code -> char enum
 * char code -> char string repr
 *)
module Keys = struct

  type key_symbol = Unknown
                  | Ctrl_c
                  | Ctrl_d
                  | Ctrl_j
                  | Ctrl_k
                  | Ctrl_u
                  | Ctrl_z
                  | Alt_h
                  | Alt_j
                  | Alt_k
                  | Alt_l
                  | Space
                  | ArrowUp
                  | ArrowDown
                  | ArrowRight
                  | ArrowLeft
                  | Lower_h
                  | Lower_j
                  | Lower_k
                  | Lower_l
                  | Lower_b
                  | Lower_w
                  | Upper_b
                  | Upper_w
                  | Digit_0
                  | Digit_1
                  | Digit_2
                  | Digit_3
                  | Digit_4
                  | Digit_5
                  | Digit_6
                  | Digit_7
                  | Digit_8
                  | Digit_9
                  | Backslash

  type key = {
    symbol  : key_symbol ;
    repr    : string ;
    code    : int ;
  }

  let unknown_key = {
    symbol  = Unknown ;
    repr    = "unknown" ;
    code    = 0 ;
  }

  let make_key s r c = { symbol = s; repr = r; code = c }

  let code_to_key_table = Array.make 256 unknown_key ;;
  code_to_key_table.(3)   <- make_key Ctrl_c      "Ctrl_c"        3 ;;
  code_to_key_table.(4)   <- make_key Ctrl_d      "Ctrl_d"        4 ;;
  code_to_key_table.(10)  <- make_key Ctrl_j      "Ctrl_j"        10 ;;
  code_to_key_table.(11)  <- make_key Ctrl_k      "Ctrl_k"        11 ;;
  code_to_key_table.(21)  <- make_key Ctrl_u      "Ctrl_u"        21 ;;
  code_to_key_table.(26)  <- make_key Ctrl_z      "Ctrl_z"        26 ;;
  code_to_key_table.(32)  <- make_key Space       "Space"         32 ;;
  code_to_key_table.(65)  <- make_key ArrowUp     "ArrowUp"       65 ;;
  code_to_key_table.(66)  <- make_key ArrowDown   "ArrowDown"     66 ;;
  code_to_key_table.(67)  <- make_key ArrowRight  "ArrowRight"    67 ;;
  code_to_key_table.(68)  <- make_key ArrowLeft   "ArrowLeft"     68 ;;
  code_to_key_table.(66)  <- make_key Upper_b     "W"             66 ;;
  code_to_key_table.(87)  <- make_key Upper_w     "B"             87 ;;
  code_to_key_table.(98)  <- make_key Lower_b     "w"             98 ;;
  code_to_key_table.(104) <- make_key Lower_h     "h"             104 ;;
  code_to_key_table.(106) <- make_key Lower_j     "j"             106 ;;
  code_to_key_table.(107) <- make_key Lower_k     "k"             107 ;;
  code_to_key_table.(108) <- make_key Lower_l     "l"             108 ;;
  code_to_key_table.(119) <- make_key Lower_w     "w"             119 ;;
  code_to_key_table.(153) <- make_key Alt_h       "Alt_h"         153 ;;
  code_to_key_table.(134) <- make_key Alt_j       "Alt_j"         134 ;;
  code_to_key_table.(154) <- make_key Alt_k       "Alt_k"         154 ;;
  code_to_key_table.(172) <- make_key Alt_l       "Alt_l"         172 ;;
  code_to_key_table.(48)  <- make_key Digit_0     "0"             48 ;;
  code_to_key_table.(49)  <- make_key Digit_1     "1"             49 ;;
  code_to_key_table.(50)  <- make_key Digit_2     "2"             50 ;;
  code_to_key_table.(51)  <- make_key Digit_3     "3"             51 ;;
  code_to_key_table.(52)  <- make_key Digit_4     "4"             52 ;;
  code_to_key_table.(53)  <- make_key Digit_5     "5"             53 ;;
  code_to_key_table.(54)  <- make_key Digit_6     "6"             54 ;;
  code_to_key_table.(55)  <- make_key Digit_7     "7"             55 ;;
  code_to_key_table.(56)  <- make_key Digit_8     "8"             56 ;;
  code_to_key_table.(57)  <- make_key Digit_9     "9"             57 ;;
  code_to_key_table.(92)  <- make_key Backslash   "\\"            92 ;;

  let code_to_key code =
    match code_to_key_table.(code) with
    | unknown when unknown.symbol = Unknown -> { unknown with code = code }
    | found                                 -> found

end


module Bytevector = struct

  type t = {
    bytes : bytes ;
    len : int ;
  }

  module Priv = struct

    let scale size =
      size |> float |> ( *. ) 1.45 |> ceil |> truncate

    let rec next_size needed_size size =
      if needed_size <= size then size else next_size needed_size (scale size)

    let grow new_size bytes =
      Bytes.extend bytes 0 (new_size - (blen bytes))

    let ensure_size needed_size bytes =
      let current_size = (blen bytes) in
      if (needed_size <= current_size)
        then bytes
        else grow (next_size needed_size current_size) bytes

    let grow added_length t =
      let new_length = added_length + t.len in
      let new_bytes = ensure_size new_length t.bytes
      in {
        bytes = new_bytes ;
        len   = new_length ;
      }

  end

  let init_bytevector len = {
    bytes = Bytes.make len (Char.chr 0) ;
    len   = 0 ;
  }

  let reset bytevec = {
    bytes = bytevec.bytes ;
    len   = 0 ;
  }

  let append s t =
    let sl = slen s in
    let t' = Priv.grow sl t in
      Bytes.blit_string s 0 t'.bytes t.len sl ;
      t'

  let append_bytes srcbytes srcoffset len t =
    let t' = Priv.grow len t in
      Bytes.blit srcbytes srcoffset t'.bytes t.len len ;
      t'

  let write fd t =
    write fd t.bytes t.len
end


(* main module for interacting with the terminal *)
module Term = struct

  module Color = struct

    type base = Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenta
              | Cyan
              | White

    let base_code = function
      | Black   -> 0
      | Red     -> 1
      | Green   -> 2
      | Yellow  -> 3
      | Blue    -> 4
      | Magenta -> 5
      | Cyan    -> 6
      | White   -> 7

    let bold_code = function
      | Black   -> 8
      | Red     -> 9
      | Green   -> 10
      | Yellow  -> 11
      | Blue    -> 12
      | Magenta -> 13
      | Cyan    -> 14
      | White   -> 15

    type t = Normal of base
           | Bold of base
           | Gray of int
           | RGB216 of int * int * int
           (* | RGB24b of int * int * int *) (* TODO: support 24b colors, support rgb hex string *)

    let color_control_code = function
      | Normal c        -> base_code c
      | Bold c          -> bold_code c
      | Gray g          -> 232 + g                  (* TODO: clamp to [0,23] *)
      | RGB216 (r,g,b)  -> 16 + 36 * r + 6 * g + b  (* TODO: clamp to [0, 5] ^ 3 *)

    type color_cell = {
      fg : t ;
      bg : t ;
    }

    let color_control_string { fg ; bg } =
      Printf.sprintf "38;5;%d;48;5;%dm" (color_control_code fg) (color_control_code bg)

    let black   = Normal Black ;;
    let red     = Normal Red ;;
    let green   = Normal Green ;;
    let yellow  = Normal Yellow ;;
    let blue    = Normal Blue ;;
    let magenta = Normal Magenta ;;
    let cyan    = Normal Cyan ;;
    let white   = Normal White ;;

  end

  module Control = struct
    let escape                = 27 |> Char.chr |> string_of_char ;;
    let start                 = escape ^ "[" ;;
    let finish                = escape ^ "[0m" ;;
    let clear                 = escape ^ "c" ;;
    let newline               = "\r\n"  ;;
    let cursor_hide           = start ^ "?25l" ;;
    let cursor_show           = start ^ "?25h" ;;
    let switch_offscreen      = start ^ "?47h" ;;
    let switch_mainscreen     = start ^ "?47l" ;;
    let gohome                = start ^ "H" ;;

    let cursor_offset = v2_of_xy 1 1

    (* ANSI escape codes weirdness: cursor positions are 1 based in the terminal referential *)
    let cursor_control_string vec2 =
      let {x ; y } = cursor_offset <+> vec2 in
      Printf.sprintf "%s%d;%dH" start y x
  end

  external get_terminal_size : unit -> (int * int) = "get_terminal_size"

  let do_with_raw_mode action =
    let open Unix in
    let stdout_write_string s =
      if (write_substring stdout s 0 (slen s)) <> slen s then raise (Failure "sdtout write failed")
    in
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
      want.c_isig    <- false ;   (* no INTR, QUIT, SUSP signals *)
      want.c_vmin    <- 0;        (* return each byte one by one, or 0 if timeout *)
      want.c_vtime   <- 100;      (* 100 * 100 ms timeout for reading input *)
                                  (* TODO: how to set a low timeout in order to process async IO results
                                               but not deal with the hassle of End_of_file from input_char ... *)
      want.c_csize   <- 8;        (* 8 bit chars *)

      (* TODO: save cursor position and screen state *)
      stdout_write_string Control.switch_offscreen ;
      tcsetattr stdin TCSAFLUSH want ;
      try_finally action (fun () ->
        tcsetattr stdin TCSAFLUSH initial ;
        stdout_write_string Control.switch_mainscreen
        (* TODO: restore cursor position and screen state *)
      )
    )

end


(* this is a config module for storing all parameters *)
module Config = struct
  open Term.Color

  type colors = {
    operator      : color_cell ;
    structure     : color_cell ;
    string        : color_cell ;
    spacing       : color_cell ;
    numbers       : color_cell ;
    default       : color_cell ;
    cursor_line   : color_cell ;
    line_numbers  : color_cell ;
    header        : color_cell ;
    status        : color_cell ;
    user_input    : color_cell ;
    default_fill  : color_cell ;
  }

  type cfg = {
    colors : colors ;
  }

  let default : cfg = {
    colors = {
      operator = {
        fg    = green ;
        bg    = black ;
      } ;
      structure = {
        fg    = red ;
        bg    = black ;
      } ;
      string  = {
        fg    = yellow ;
        bg    = black ;
      } ;
      spacing = {
        fg    = black ;
        bg    = black ;
      } ;
      numbers = {
        fg    = magenta ;
        bg    = black ;
      } ;
      default = {
        fg    = white ;
        bg    = black ;
      } ;
      cursor_line = {
        fg    = white ;
        bg    = Gray 4 ;
      } ;
      line_numbers = {
        fg    = green ;
        bg    = black ;
      } ;
      header = {
        fg    = black ;
        bg    = yellow ;
      } ;
      status = {
        fg    = black ;
        bg    = white ;
      } ;
      user_input = {
        fg    = white ;
        bg    = black ;
      } ;
      default_fill = {
        fg    = blue ;
        bg    = black ;
      } ;
    } ;
  }
end

open Config


module CompositionBuffer = struct
  (* TODO: define types for bounding box, area, wrapping mode for text, blending mode for color, ...
   *       and use them for set_text and set_color *)

  module Default = struct
    let fg    = Term.Color.white ;;
    let bg    = Term.Color.black ;;
    let z     = 0 ;;
    let text  = ' ' ;;
  end

  type t = {
    text        : Bytes.t ;
    fg_colors   : Term.Color.t array ;
    bg_colors   : Term.Color.t array ;
    z_index     : int array ;
    len         : int ;
    window      : v2 ;
  }

  module Priv = struct
    let colors_at t offset =
      let open Term.Color in {
        fg = t.fg_colors.(offset) ;
        bg = t.bg_colors.(offset) ;
      }

    let next_contiguous_color_section t start =
      let colors_to_match = colors_at t start in
      let rec loop stop =
        if stop < t.len && (colors_at t stop) = colors_to_match
        then loop (stop + 1)
        else stop
      in loop (start + 1)

    let render_section color_cell start stop t bvec =
      (* append lines one at a time starting from start offset, ending at stop offset *)
      let next_line_len t start stop =
        min (t.window.x - (start mod t.window.x)) (stop - start)
      in
      let append_newline_if_needed t position bvec =
        let is_end_of_line        = (position mod t.window.x) = 0 in
        let is_not_end_of_buffer  = position < t.len in (* Do not append newline at the very end *)
        if is_end_of_line && is_not_end_of_buffer
        then Bytevector.append Term.Control.newline bvec
        else bvec
      in
      let rec loop start stop bvec =
        if start < stop
        then
          let len = next_line_len t start stop in
          bvec
               |> Bytevector.append Term.Control.start
               |> Bytevector.append (Term.Color.color_control_string color_cell)
               |> Bytevector.append_bytes t.text start len
               |> Bytevector.append Term.Control.finish
               (* Last newline need to be appened *after* the terminating control command for colors *)
               |> append_newline_if_needed t (start + len)
               |> loop (start + len) stop
        else
          bvec
      in
       bvec
            |> loop start stop

    let render_all_sections t bvec =
      let rec loop start bvec =
        if start < t.len
        then
          let stop = next_contiguous_color_section t start in
          bvec |> render_section (colors_at t start) start stop t
               |> loop stop
        else
          bvec
      in
        loop 0 bvec
  end

  let init vec2 =
    let len = vec2.x * vec2.y
    in {
      text        = Bytes.make len Default.text ;
      fg_colors   = Array.make len Default.fg ;
      bg_colors   = Array.make len Default.bg ;
      z_index     = Array.make len Default.z ;
      len         = len ;
      window      = vec2 ;
    }

  let clear t =
    Bytes.fill t.text 0 t.len Default.text ;
    Array.fill t.fg_colors 0 t.len Default.fg ;
    Array.fill t.bg_colors 0 t.len Default.bg ;
    Array.fill t.z_index 0 t.len Default.z

  let render cursor composition_buffer render_buffer =
    render_buffer |> Bytevector.reset
                  |> Bytevector.append Term.Control.cursor_hide
                  |> Bytevector.append Term.Control.gohome
                  |> Priv.render_all_sections composition_buffer
                  |> Bytevector.append (Term.Control.cursor_control_string cursor)
                  |> Bytevector.append Term.Control.cursor_show
                  |> Bytevector.write Unix.stdout

  (* TODO: strip \r\n, do tab to space expansion *)
  let set_text vec2 s t =
    let start = v2_to_offset t.window.x vec2 in
    if start < t.len then
      let len = slen s in
      let maxlen = t.len - start in
      let stoplen = min len maxlen in
      Bytes.blit_string s 0 t.text start stoplen

  let set_color vec2 len { Term.Color.fg ; Term.Color.bg } t =
    let start = v2_to_offset t.window.x vec2 in
    if start < t.len then
      let maxlen = t.len - start in
      let stoplen = min len maxlen in
      Array.fill t.fg_colors start stoplen fg ;
      Array.fill t.bg_colors start stoplen bg
end


module Screen = struct

  type t = {
    size                : v2 ;
    screen_offset       : v2 ;
    cursor_position     : v2 ;                    (* Note: cursor_position is not used now, but will be later on *)
    composition_buffer  : CompositionBuffer.t ;

    (* In the future, this will may have subscreens nested to this screen, sharing the same composition buffer.
     * Overlap, tiling and rendering will be managed by using the offset, size, and z value per screen,
     * using the z_buffer of the backing composition buffer *)
  }

  let init_screen cb offset size = {
    size                = size ;
    screen_offset       = offset ;
    cursor_position     = v2_zero ;
    composition_buffer  = cb ;
  }

  let line_size_vec screen =
    v2_of_xy screen.size.x 0

  let shift_left vec2 =
    v2_of_xy 0 vec2.y

  let line_offset =
    v2_of_xy 0 1

  (* TODO; line up and line down should return an Option to indicate if the result is inside the screen *)
  let line_up vec2 =
    vec2 <-> line_offset

  let line_down vec2 =
    vec2 <+> line_offset

  let last_line screen =
    screen.size |> line_up

  let last_last_line screen =
    screen.size |> line_up |> line_up

  let stop_of screen start s =
    let stride = screen.composition_buffer.CompositionBuffer.window.x in
    start |> v2_to_offset stride
          |> (+) (slen s)
          |> offset_to_v2 stride

  let next_line screen vec2 =
    if vec2.y < screen.size.y
    then v2_of_xy 0 (vec2.y + 1) |> some
    else None

  (* Write a string to the screen starting at the given position.
   * If the string is longer then the remaining space on the line, then wraps the string to the next line. *)
  let put_string start s screen =
    CompositionBuffer.set_text start s screen.composition_buffer

  (* Set the foreground and background colors of a given segment of the screen delimited by the given start
   * and end positions. *)
  let put_color_segment colors start stop screen =
    let cb = screen.composition_buffer  in
    let stride = cb.CompositionBuffer.window.x in
    let start_p = v2_to_offset stride start in
    let stop_p  = v2_to_offset stride stop in
    let len = stop_p - start_p in
    if len > 0 then
      CompositionBuffer.set_color start len colors cb

  (* Set the foreground and background colors of the line pointed to by the current position. *)
  (* TODO: migrate all callers to put_line and kill this *)
  let put_color_line colors vec2 screen =
    let start = shift_left vec2 in
    let stop = start <+> (line_size_vec screen) in
    put_color_segment colors start stop screen

  let put_color_string colors start s screen =
    let stop = stop_of screen start s
    in
      put_string start s screen ;
      put_color_segment colors start stop screen ;
      stop

  let put_line colors start line screen =
    let stop = stop_of screen start line in
    let line_stop = v2_of_xy screen.size.x stop.y
    in
      put_string start line screen ;
      put_color_segment colors start line_stop screen ;
      line_stop
end


(* This represents a file currently edited
 * It contains both file information, and windowing information
 * TODO: to properly support multiple editing views into the same file, I need to split these into two
 * TODO: handle long lines: need to wrap line correctly, but need to detect in advance at creation and track correspondly *)
(* TODO: add line number *)
(* TODO: Some of the first cracks in this reprensentation are already showing up.
         For instance, empty lines are empty strings, but in the file they take characters
         this requires special handling in the editor, because we still need to restore these empty lines at save
         and need to display them.
         The naive version of move_next_word therefore fails on empty lines without special handling.
         Similarly move_next_paragraph requires special cursor advance function, because it must not ignore
         empty line, while move_next_word must absolutely do.
         Furthermore, next word, next line, end-of-line, and so on should be first class concept in this
         representation. *)
module Filebuffer = struct

  type numbering_mode = Absolute | CursorRelative

  type t = {
      buffer      : string array ;  (* the file data, line per line *)
      atom_buffer : Atom.atom list array ;  (* parsed atoms from the file data *)
      buflen      : int ;           (* number of lines in the buffer, may be less than buffer array length *)

      cursor      : v2 ;           (* current position string array: y = index array (rows), x = string array (cols) *)

  (* TODO: move to FileView *)
      view_start  : int ;      (* index of first row in view *)
      view_diff   : int ;      (* additional rows in the view after the first row = total_rows_in_view - 1 *)
                              (* index of last row in view is view_start + view_diff *)

  (* TODO: move to FileView *)
      line_number_m : numbering_mode ;
  }

  let init_filebuffer lines view_h =
    let buffer = Array.of_list lines in
    let atoms = Array.map Atom.generic_atom_parser buffer in {
      buffer        = buffer ;
      atom_buffer   = atoms ;
      buflen        = alen buffer ;
      cursor        = v2_zero ;
      view_start    = 0 ;
      view_diff     = view_h - 1;
      line_number_m = CursorRelative ;
    }

  let is_current_char_valid t = t.cursor.x < (slen t.buffer.(t.cursor.y)) ;;
  let current_line t = t.buffer.(t.cursor.y)
  let current_char t = String.get (current_line t) t.cursor.x ;;
  let cursor t = t.cursor ;;

  let saturate_up length x = min (max (length - 1) 0) x

  let adjust_view t =
    if t.cursor.y < t.view_start then
      { t with
        view_start  = t.cursor.y ;
      }
    else if t.cursor.y > t.view_start + t.view_diff then
      { t with
        view_start  = t.cursor.y - t.view_diff ;
      }
    else t

  let adjust_cursor vec2 t = { t with cursor = vec2 }

  let apply_movement fn t =
    t |> adjust_cursor (fn t) |> adjust_view

  let swap_line_number_mode t =
    let new_mode = match t.line_number_m with
    | Absolute        -> CursorRelative
    | CursorRelative  -> Absolute
    in { t with line_number_m = new_mode }

  let recenter_view t =
    let new_start = t.cursor.y - t.view_diff / 2 in
    { t with view_start = max new_start 0 }

  (* TODO: regroup movement functions that fit the Ciseau.command type into a specific submodule. *)
  (* move_* commands saturates at 0 and end of line *)
  let move_cursor_left t = {
    x = t.cursor.x |> dec |> max 0 ;
    y = t.cursor.y ;
  }

  let move_cursor_left2 t = {
    x = t.cursor.x |> dec |> max 0 ;
    y = t.cursor.y ;
  }

  let move_cursor_right t = {
    x = t.cursor.x |> inc |> saturate_up (slen t.buffer.(t.cursor.y)) ;
    y = t.cursor.y ;
  }

  let move_n_up n t = {
    x = t.cursor.x ;
    y = t.cursor.y |> fun x -> x - n |> max 0 ;
  }

  let move_n_down n t = {
    x = t.cursor.x ;
    y = t.cursor.y |> (+) n |> saturate_up t.buflen ;
  }

  let move_cursor_up   = move_n_up 1 ;;
  let move_cursor_down = move_n_down 1 ;;
  let move_page_up   t = move_n_up (t.view_diff + 1) t ;;
  let move_page_down t = move_n_down (t.view_diff + 1) t ;;

  let cursor_next_char_proto t vec2 =
    (* BUG: infinite loop on file where the matcher never return true *)
    let rec first_non_empty y =
      match y with
      | _ when y = t.buflen           -> first_non_empty 0
      | _ when 0 = slen t.buffer.(y)  -> first_non_empty (y + 1)
      | _                             -> y
    in
      if vec2.x + 1 < slen (current_line t)
      then { x = vec2.x + 1; y = vec2.y}
      else { x = 0; y = first_non_empty (vec2.y + 1) } (* skip empty lines *)

  let cursor_next_char t =
    cursor_next_char_proto t t.cursor

  let cursor_prev_char t =
    (* BUG: infinite loop on file where the matcher never return true *)
    let rec last_non_empty y =
      match y with
      | _ when y = -1                 -> last_non_empty (t.buflen - 1)
      | _ when 0 = slen t.buffer.(y)  -> last_non_empty (y - 1)
      | _                             -> y
    in
      if t.cursor.x - 1 > 0
      then { x = t.cursor.x - 1 ; y = t.cursor.y }
      else
        let y' = last_non_empty (t.cursor.y - 1) in
        { x = (slen t.buffer.(y')) - 1 ; y = y'}

  let cursor_next_line t = {
      x = t.cursor.x ;
      y = (t.cursor.y + 1) mod t.buflen ;
    }

  let cursor_prev_line t =
    let y' = if t.cursor.y - 1 >= 0 then t.cursor.y - 1 else t.buflen - 1 in {
      x = t.cursor.x ;
      y = y' ;
    }

  (* TODO: refactor to remove the use of adjust cursor *)
  let rec cursor_move_while u f t =
    if f t
    then t |> adjust_cursor (u t) |> cursor_move_while u f
    else t

  (* BUG ? '_' is considered to be a word separation *)
  let move_next_word t =
    t |> cursor_move_while cursor_next_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_next_char (current_char >> is_alphanum)
      |> cursor_move_while cursor_next_char (current_char >> is_alphanum >> not)
      |> cursor

  (* BUG: when starting from an empty line, the first previous word is skipped and the cursor goes to the second previous word *)
  let move_prev_word t =
    t |> cursor_move_while cursor_prev_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_prev_char (current_char >> is_alphanum)
      |> cursor_move_while cursor_prev_char (current_char >> is_alphanum >> not)
      |> cursor_move_while cursor_prev_char (current_char >> is_alphanum)
      |> cursor_next_char

  let move_next_space t =
    t |> cursor_move_while cursor_next_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_next_char (current_char >> is_space)
      |> cursor_move_while cursor_next_char (current_char >> is_space >> not)
      |> cursor

  (* BUG: this always skips a single leading space at beginning of a line, but does not skip more than one leading space *)
  let move_prev_space t =
    t |> cursor_move_while cursor_prev_char (is_current_char_valid >> not)
      |> cursor_move_while cursor_prev_char (current_char >> is_space)
      |> cursor_move_while cursor_prev_char (current_char >> is_space >> not)
      |> cursor

  (* BUG when wrapping over the end of a file, last paragraph and first paragraph are see as one paragraph only *)
  let move_next_paragraph t =
    t |> cursor_move_while cursor_next_line (current_line >> is_empty)
      |> cursor_move_while cursor_next_line (current_line >> is_empty >> not)
      |> cursor_move_while cursor_next_line (current_line >> is_empty)
      |> cursor

  let move_prev_paragraph t =
    t |> cursor_move_while cursor_prev_line (current_line >> is_empty)
      |> cursor_move_while cursor_prev_line (current_line >> is_empty >> not)
      |> cursor_move_while cursor_prev_line (current_line >> is_empty)
      |> cursor

  let move_line_start t = { x = 0 ; y = t.cursor.y } ;;
  let move_line_end t   = { x = max 0 ((slen t.buffer.(t.cursor.y)) - 1) ; y = t.cursor.y } ;;
  let move_file_start t = { x = t.cursor.x ; y = 0 } ;;
  let move_file_end t   = { x = t.cursor.x ; y = t.buflen - 1 } ;;

  let cursor_relative_to_view t = t.cursor <-> { x = 0; y = t.view_start } ;;
  let file_length_string t = (string_of_int t.buflen) ^ "L" ;;

  (* Represents the result of projecting a line of text inside a drawing view rectangle *)
  type line_info = {
    number      : int ;
    text        : string ;
    colors      : Term.Color.color_cell ; (* currently unused *)
    atoms        : Atom.atom list ;
  }

  type projected_view = {
    lines       : line_info list ;
  }

  let color_for_atom { Atom.kind ; _ } =
    let open Atom in
      match kind with
      | Text      -> Config.default.colors.default
      | Digit     -> Config.default.colors.numbers
      | Spacing   -> Config.default.colors.spacing
      | Operator  -> Config.default.colors.operator
      | Structure -> Config.default.colors.structure
      | Line      -> Config.default.colors.default
      | Control   -> Config.default.colors.default
      | Other     -> Config.default.colors.default
      | Ending    -> Config.default.colors.default

  let apply_view_frustrum max_line t =
    let view_size = min (t.view_diff + 1) max_line in
    let stop = min t.buflen (t.view_start + view_size) in
    let offset = match t.line_number_m with
    | Absolute        -> 1 ;
    | CursorRelative  -> -t.cursor.y
    in let rec get_line i =
      let colors =
        if (i = t.cursor.y)
        then Config.default.colors.cursor_line
        else Config.default.colors.default
      in
      if i < stop
      then {
        number  = i + offset ;
        text    = t.buffer.(i) ;
        colors  = colors ;
        atoms   = t.atom_buffer.(i) ;
      } :: get_line (i + 1)
      else []
    in {
      lines         = get_line t.view_start ;
    }

end


(* FileView wraps a Filebuffer and represents an open view into a file
 * it manages drawing the lines of the file into the backing composition buffer
 * and handles line overwrapping, line numbering, view centering *)
module FileView = struct

  type t = {
    background:   Screen.t ;  (* Area given for displaying the file content, including header and line numbers *)
    text_area:    Screen.t ;  (* subarea for drawing the text *)
    cursor:       v2 ;        (* position of the cursor relative to that screen coordinates system *)

    composition:  CompositionBuffer.t ;
    file_buffer:  Filebuffer.t ;
  }

  (* TODOs: - move show_header and print_file_buffer here as is
   *        - remove all view offset from main Editor struct
   *        - redirect View commands to FileView
   *        - take into account overwrapping lines for managing diff between FileView and FileBuffer cursors
   *)

  (* How to deal with different FileViews sharing the sale Filebuffer ?
   *  whenever a view changes the Filebuffer, all other Fileviews needs to be updated with the new Filebuffer *)

end

module Ciseau = struct

  type pending_command_atom = Digit of int

  type pending_command = None
                       | Number of int list

  (* Represents an editor command *)
  type command = Noop
               | Stop
               | Move of (Filebuffer.t -> v2)
               | View of (Filebuffer.t -> Filebuffer.t)
               | Pending of pending_command_atom

  let max_repetition = 10000

  let enqueue_digit d = function
    | None      -> Number [d]
    | Number ds -> Number (d :: ds)

  let dequeue_digits ds =
    let rec loop acc = function
      | []      -> acc
      | d :: t  -> loop (acc * 10 + d) t
    in min (loop 0 (List.rev ds)) max_repetition

  let pending_command_to_string = function
    | None      -> ""
    | Number ds -> Printf.sprintf "Repetition(%d) " (dequeue_digits ds)

  type editor = {
    screen : Screen.t ;
    (* Fold width and height into Screen.t *)
    width : int;
    height : int;
    running : bool ;

    file : string ;
    filebuffer : Filebuffer.t ;
    view_offset : v2 ;

    header : string ;
    user_input : string ;

    pending_input : pending_command ;

    (* TODO: add every X a full stats collection for printing total footprint *)
    (* TODO: keep a rolling buffer of alloc diff per frame and show quantiles *)
    (* TODO: export stats to some log files for doing more offline statistics *)
    gc_stats : Gc.stat ;
    gc_stats_diff : float * float ;

    timestamp           : float ;
    last_input_duration : float ;
    last_cycle_duration : float ;

    render_buffer       : Bytevector.t ;
    composition_buffer  : CompositionBuffer.t ;
  }


  let init_editor file : editor =
    let (term_rows, term_cols) = Term.get_terminal_size () in
    let term_dim = v2_of_xy term_cols term_rows in
    let composition_buffer = CompositionBuffer.init term_dim in
    let lines = slurp file in
    {
      screen          = Screen.init_screen composition_buffer v2_zero term_dim ;
      width           = term_cols ;
      height          = term_rows ;
      running         = true ;

      file            = file ;
      filebuffer      = Filebuffer.init_filebuffer lines (term_rows - 3) ;   (* 3 lines for header, status, input *)
      view_offset     = v2_of_xy 5 1 ; (* +5 for line numbers, +1 for header *)

      header          = (Sys.getcwd ()) ^ "/" ^ file ;
      user_input      = "" ;

      pending_input   = None;

      gc_stats        = Gc.quick_stat () ;
      gc_stats_diff   = (0., 0.) ;

      timestamp           = Sys.time() ;
      last_input_duration = 0. ;
      last_cycle_duration = 0. ;

      render_buffer       = Bytevector.init_bytevector 0x1000 ;
      composition_buffer  = composition_buffer ;
    }

  let queue_pending_command editor = function
    | Digit n -> { editor with pending_input = enqueue_digit n editor.pending_input }

  let apply_command command editor =
    match command with
    | Noop    -> editor
    | Stop    -> { editor with running = false }
    | Move fn -> { editor with filebuffer = Filebuffer.apply_movement fn editor.filebuffer }
    | View fn -> { editor with filebuffer = fn editor.filebuffer }
      (* cannot happen ?? *)
    | Pending ((Digit n) as d)  -> queue_pending_command editor d

  let apply_command_with_repetition n command editor =
    match command with
    | Noop    -> editor
    | Stop    -> { editor with running = false }
    | View fn -> { editor with filebuffer = fn editor.filebuffer }
    | Move fn ->
      let rec loop n fb =
        if (n > 0) then loop (n - 1) (Filebuffer.apply_movement fn fb) else fb
      in {
        editor with
        filebuffer    = loop n editor.filebuffer ;
        pending_input = None ;
      }
    | Pending ((Digit n) as d)  -> queue_pending_command editor d

  let update_stats now input_duration editor =
    let open Gc in
    (* let _ = Bytes.make (1024 * 512) '0' in (* DEBUG uncomment me to pressure the gc *) *)
    let new_gc_stats = quick_stat () in
    let minor_diff = new_gc_stats.minor_words -. editor.gc_stats.minor_words in
    let major_diff = new_gc_stats.major_words -. editor.gc_stats.major_words in
    { editor with
      gc_stats            = new_gc_stats ;
      gc_stats_diff       = (minor_diff, major_diff) ;
      timestamp           = now ;
      last_input_duration = input_duration ;
      last_cycle_duration = now -. editor.timestamp ;
    }

  let word_byte_size = float (Sys.word_size / 8)

  let format_memory_counter word_count = match word_count *. word_byte_size with
    | x when x < 1024.          -> Printf.sprintf "%.2fB" x
    | x when x < 1024. *. 1024. -> Printf.sprintf "%.2fkB" (x /. 1024.)
    | x                         -> Printf.sprintf "%.2fMB" (x /. 1024. /. 1024.)

  let format_memory_counters (minor, major) =
    Printf.sprintf "%s/%s" (format_memory_counter major)
                           (format_memory_counter minor)

  let format_memory_stats editor =
    let open Gc in
    Printf.sprintf "  mem total = (%s)  mem delta = (%s)  gc = (%d,%d)"
      (format_memory_counters (editor.gc_stats.minor_words, editor.gc_stats.major_words))
      (format_memory_counters editor.gc_stats_diff)
      editor.gc_stats.major_collections
      editor.gc_stats.minor_collections

  let format_time_stats editor =
    Printf.sprintf "  time = %.3f ms" (1000. *. (editor.last_cycle_duration -. editor.last_input_duration))

  let window_size editor =
    "(" ^ (string_of_int editor.width) ^ " x " ^ (string_of_int editor.height) ^ ")"

  let show_header editor screen vec2 =
    let s = editor.header
          ^ "  " ^ (Filebuffer.file_length_string editor.filebuffer)
          ^ "  " ^ (editor.filebuffer |> Filebuffer.cursor |> v2_to_string)
    in
      Screen.put_line Config.default.colors.header vec2 s screen |> ignore

  let show_status editor screen vec2 =
    (* BUG: fix this -1 offset issue *)
    let vec2' = Screen.line_up vec2 in
    let s = "Ciseau stats: win = "
          ^ (window_size editor)
          ^ (format_memory_stats editor)
          ^ (format_time_stats editor)
    in
      Screen.put_line Config.default.colors.status vec2' s screen |> ignore

  let show_user_input editor screen vec2 =
    (* BUG: fix this -1 offset issue *)
    let vec2' = Screen.line_up vec2 in
    let s =  editor.user_input
    in
      Screen.put_line Config.default.colors.user_input vec2' s screen |> ignore

  let print_atoms screen colors index atoms =
    let rec loop index = function
      | []      -> index
      | a :: t  ->
          let text = Atom.atom_to_string a in
          let colors = (Filebuffer.color_for_atom a) in
          let next = Screen.put_color_string colors index text screen in
          loop next t
    in
      loop index atoms

  let print_file_buffer max_line filebuffer screen =
    (* TODO: handle view_offset inside nested screen and remove max_line, y_offset, and stop_offset *)
    let y_offset = 1 in
    let stop_offset = y_offset + max_line in
    let open Filebuffer in
    let print_line start { text ; number ; colors ; atoms } =
      let num = Printf.sprintf "%4d " number in
      let next = Screen.put_color_string Config.default.colors.line_numbers start num screen in
      let stop = print_atoms screen colors next atoms in
      Screen.next_line screen stop
    in
    let rec loop lines line_start =
    match (lines, line_start) with
    | (info :: rest_lines, Some start) ->
      (* BUG: this stop_offset guard is still necessary because the filebuffer
       * has no way currently to tell which lines are going to overflow.
       *  -> this causes problems when the cursors are in the last lines of the pseudo view
       *     inside the filebuffer. For instance if cursor is on last line and one previous line overflow,
       *     the cursor should be hidden.
       *     -> the conclusion is that the Filebuffer (or the ting aware of cursor into the file and doing
       *        view adjust) needs to know about which line overflows.
       *        -> the view could be actually calculated here and passed back to the filebuffer.
       *           this would require removing all the view adjust code from the filebuffer.
       *)
      if start.y < stop_offset
      then print_line start info |> loop rest_lines
    | _ -> ()
    in
      let { lines } = apply_view_frustrum max_line filebuffer in
      loop lines (v2_of_xy 0 y_offset |> some)

  let default_fill_screen screen =
    let rec loop = function
      | Some line ->  Screen.put_color_string Config.default.colors.default_fill line "~" screen
                        |> Screen.next_line screen
                        |> loop
      | None      ->  ()
    in
      v2_zero |> some |> loop

  (* BUGS:  - last_line and last_last_line have +1 offset ! *)
  let refresh_screen editor =
    (* Note: when multiple screen are on, there needs to be cursor selection from active screen *)
    let cursor_position =
      (editor.filebuffer |> Filebuffer.cursor_relative_to_view |> (<+>) editor.view_offset)
    in
    let screen = editor.screen in (
      CompositionBuffer.clear editor.composition_buffer ;
      default_fill_screen screen ;
      show_header editor screen v2_zero ;
      show_status editor screen (Screen.last_last_line screen) ;
      show_user_input editor screen (Screen.last_line screen) ;
      print_file_buffer (editor.height - 3) editor.filebuffer screen ;
      CompositionBuffer.render cursor_position editor.composition_buffer editor.render_buffer ;
      editor
    )


  let key_to_command = function
    | Keys.Ctrl_c       -> Stop
    | Keys.Backslash    -> View Filebuffer.swap_line_number_mode
    | Keys.Ctrl_z       -> View Filebuffer.recenter_view
    | Keys.Ctrl_d       -> Move Filebuffer.move_page_down
    | Keys.Ctrl_j       -> Move Filebuffer.move_next_paragraph
    | Keys.Ctrl_k       -> Move Filebuffer.move_prev_paragraph
    | Keys.Ctrl_u       -> Move Filebuffer.move_page_up
    | Keys.Alt_k        -> Move Filebuffer.move_file_start
    | Keys.Alt_j        -> Move Filebuffer.move_file_end
    | Keys.Alt_l        -> Move Filebuffer.move_line_end
    | Keys.Alt_h        -> Move Filebuffer.move_line_start
    | Keys.Space        -> View Filebuffer.recenter_view
    | Keys.ArrowUp      -> Move Filebuffer.move_cursor_up
    | Keys.ArrowDown    -> Move Filebuffer.move_cursor_down
    | Keys.ArrowRight   -> Move Filebuffer.move_cursor_right
    | Keys.ArrowLeft    -> Move Filebuffer.move_cursor_left
    | Keys.Lower_k      -> Move Filebuffer.move_cursor_up
    | Keys.Lower_j      -> Move Filebuffer.move_cursor_down
    | Keys.Lower_l      -> Move Filebuffer.move_cursor_right
    | Keys.Lower_h      -> Move Filebuffer.move_cursor_left
    | Keys.Lower_w      -> Move Filebuffer.move_next_word
    | Keys.Lower_b      -> Move Filebuffer.move_prev_word
    | Keys.Upper_w      -> Move Filebuffer.move_next_space
    | Keys.Upper_b      -> Move Filebuffer.move_prev_space
    | Keys.Digit_0      -> Pending (Digit 0)
    | Keys.Digit_1      -> Pending (Digit 1)
    | Keys.Digit_2      -> Pending (Digit 2)
    | Keys.Digit_3      -> Pending (Digit 3)
    | Keys.Digit_4      -> Pending (Digit 4)
    | Keys.Digit_5      -> Pending (Digit 5)
    | Keys.Digit_6      -> Pending (Digit 6)
    | Keys.Digit_7      -> Pending (Digit 7)
    | Keys.Digit_8      -> Pending (Digit 8)
    | Keys.Digit_9      -> Pending (Digit 9)
    | Keys.Unknown      -> Noop (* ignore for now *)

  let process_command editor =
    match editor.pending_input with
    | None          -> apply_command
    | Number digits -> apply_command_with_repetition (max 1 (dequeue_digits digits))

  let process_key editor = key_to_command >> (process_command editor)

  (* TODO: replace by a proper history of previous inputs *)
  let make_user_input key editor =
    let new_head = key.Keys.repr ^ "(" ^ (string_of_int key.Keys.code) ^ ")" in
    let new_user_input = (pending_command_to_string editor.pending_input)
                       ^  new_head
                       ^ " " ^ editor.user_input
    in {
      editor with
      user_input = truncate_string editor.width new_user_input ;
    }

  let process_events editor =
    let before = Sys.time () in
    let key = () |> next_char |> Keys.code_to_key in
    let after = Sys.time () in
      editor |> process_key editor key.Keys.symbol
             |> make_user_input key
             |> update_stats (Sys.time ()) (after -. before)

  let rec loop editor =
    if editor.running then
      editor |> refresh_screen |> process_events |> loop

  let run_loop editor () = loop editor

  let main () =
    (if alen Sys.argv > 1 then Sys.argv.(1) else __FILE__)
      |> init_editor
      |> run_loop
      |> Term.do_with_raw_mode

end

let () =
  Ciseau.main () ;
  close_out logs
