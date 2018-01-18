module Atom = struct

  type atom_kind = Text | Digit | Spacing | Operator | Structure | Line | Control | Other | Ending | Tab

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
    | Tab       -> "Tab"

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
    | Tab       -> "Tab       "

  let atom_kind_table = Array.make 128 Other ;;
  (* control codes *)
  for c = 0 to 31 do Array.set atom_kind_table c Control done ;;
  atom_kind_table.(009 (* horizontal tab *) ) = Tab ;;
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

  let len { kind ; start ; stop } =
    match kind with
    | Tab -> slen tab_to_spaces
    | _   -> stop - start

  let atom_to_string a =
    String.sub a.line a.start (a.stop - a.start)

  let atom_to_pretty_string a =
    Printf.sprintf "%s:'%s'" (atom_kind_to_string_padded a.kind) (atom_to_string a)

  let should_continue_atom = function
    | (Tab, _) -> false
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


let count_tabs s start stop =
  let rec loop acc i l =
    if i < l
      then loop (acc + if String.get s i = '\t' then 1 else 0) (i + 1) l
      else acc
  in
    loop 0 start stop


let color_for_atom cfg kind =
  let open Config in
  let open Atom in
    match kind with
    | Text      -> cfg.colors.default
    | Digit     -> cfg.colors.numbers
    | Spacing   -> cfg.colors.spacing
    | Operator  -> cfg.colors.operator
    | Structure -> cfg.colors.structure
    | Line      -> cfg.colors.default
    | Control   -> cfg.colors.default
    | Other     -> cfg.colors.default
    | Ending    -> cfg.colors.default
    | Tab       -> cfg.colors.default

let atom_to_block { Atom.kind ; Atom.line ; Atom.start ; Atom.stop } =
  let colors = color_for_atom Config.default kind in
  match kind with
  | Atom.Tab -> Block.mk_block tab_to_spaces colors
  | _   -> {
      Block.text    = line ;
      Block.offset  = start ;
      Block.len     = stop - start ;
      Block.colors  = colors ;
    }
