open Util
open Util.Arrays
open Util.Rec
open Util.Vec

module Cursor = struct
  type step = Nomore
            | Continue

  type t = {
    text          : string array ;  (* TODO: change to Filebuffer *)
    mutable x     : int ;
    mutable y     : int ;
    mutable xmem  : int ;           (* Ideal 'x' the cursor wants to be at when moving up/down.
                                     * Interpretation depends on current movement mode. *)
  }

  let mk_cursor t x y = {
    text  = t ;
    x     = x ;
    y     = y ;
    xmem  = x ;
  }

  let x { x } = x

  let y { y } = y

  let xmem { xmem } = xmem

  let pos { x ; y } = Vec.mk_v2 x y

  let save { text ; x ; y ; xmem } = { text ; x ; y ; xmem }

  let goto_y y cursor =
    let y' = min y ((alen cursor.text) - 1) in
    cursor.y <- y'

  let goto_x x cursor =
    let x' = min x ((slen (array_get cursor.text cursor.y)) - 1) in
    let x'' = max 0 x' in (* empty lines: put cursor on 0 *)
    cursor.x <- x''

  let goto ?x:(x_want = -1) ?y:(y_want = -1) cursor =
    let x_want' = if x_want < 0 then cursor.x else x_want in
    let y_want' = if y_want < 0 then cursor.y else y_want in
    goto_y y_want' cursor ;
    goto_x x_want' cursor

  let xmem_set cursor xmem =
    cursor.xmem <- xmem

  let line_get { text ; y } = array_get text y

  let line_len cursor = slen (line_get cursor)

  let line_is_empty cursor = line_len cursor = 0

  let line_not_empty cursor = line_len cursor > 0

  let line_next cursor =
    let y' = cursor.y + 1 in
    if y' < alen cursor.text
      then (
        cursor.y <- y' ;
        goto_x cursor.x cursor ;
        Continue
      )
      else Nomore

  let line_prev cursor =
    let y' = cursor.y - 1 in
    if y' >= 0
      then (
        cursor.y <- y' ;
        goto_x cursor.x cursor ;
        Continue
      )
      else Nomore

  let char_is_first { x } = x = 0

  (* For empty line, the cursor is never on the "last" char *)
  let char_is_last cursor =
    line_len cursor = cursor.x + 1

  let char_get { text ; x ; y } = string_at (array_get text y) x

  let char_next cursor =
    let len = cursor |> line_get |> slen in
    let x' = cursor.x + 1 in
    if x' < len
      then (
        cursor.x <- cursor.x + 1 ;
        Continue
      )
      else Nomore

  let char_prev cursor =
    let len = cursor |> line_get |> slen in
    let x' = cursor.x - 1 in
    if len > 0 && x' >= 0
      then (
        cursor.x <- x' ;
        Continue
      )
      else Nomore

  let char_zero cursor = goto ~x:0 cursor
  let char_last cursor = goto ~x:max_int cursor

  let char_first cursor =
    goto ~x:0 cursor ;
    if not (line_is_empty cursor) then
      while cursor |> char_get |> is_space && char_next cursor = Continue
      do () done

  let line_first cursor = goto ~y:0 cursor
  let line_last cursor = goto ~y:max_int cursor
  let line_is_last { text ; y } = alen text = y + 1

  let do_if_no_more fn cursor =
    function
      | Nomore    -> fn cursor
      | Continue  -> Continue

  (* These two next functions are ugly ! Isn't there a better way ?? *)

  let line_prev_non_empty cursor =
    if line_prev cursor = Nomore
      then Nomore
      else (
        let step = ref Continue in
        while line_is_empty cursor && !step = Continue do
          step := line_prev cursor
        done ;
        if !step = Continue
          then char_last cursor ;
        !step
      )

  let line_next_non_empty cursor =
    if line_next cursor = Nomore
      then Nomore
      else (
        let step = ref Continue in
        while line_is_empty cursor && !step = Continue do
          step := line_next cursor
        done ;
        if !step = Continue
          then char_zero cursor ;
        !step
      )

  let prev cursor =
    char_prev cursor
      |> do_if_no_more line_prev_non_empty cursor

  let next cursor =
    char_next cursor
      |> do_if_no_more line_next_non_empty cursor

  (* Conversion plan for introducing cursors little by little:
   *  1) add a vec -> cursor and cursor -> vec conversion fns
   *  2) translate little by little every movement module to use cursors internally but return vec as currently
   *  3) change the outer impl
   *)
end


module Move = struct
  type t  = Left
          | Right
          | Up
          | Down
          | Start
          | End
end


module type IsBlock = sig
  val is_char_inside_block : char -> bool
end

module BlockMovement(B : IsBlock) = struct

  let is_block cursor =
    B.is_char_inside_block (Cursor.char_get cursor)

  let is_not_block = is_block >> not

  let go_block_start cursor =
    if Cursor.line_not_empty cursor && Cursor.x cursor > 0 && is_block cursor
    then
      let continue = ref true in
      (* Cursor is inside a block and there is one or more char on the left: detect the edge. *)
      while is_block cursor && !continue do
        continue := Cursor.char_prev cursor = Continue
      done ;
      (* Adjust one char to the right unless cursor hit the beginning of line *)
      if !continue
        then Cursor.char_next cursor |> ignore

  let go_block_end cursor =
    if Cursor.line_not_empty cursor && not (Cursor.char_is_last cursor) && is_block cursor
    then
      let continue = ref true in
      (* Cursor is inside a block and there is one or more char on the left: detect the edge. *)
      while is_block cursor && !continue do
        continue := Cursor.char_next cursor = Continue
      done ;
      (* Adjust one char to the left unless cursor hit the end of line *)
      if !continue
        then Cursor.char_prev cursor |> ignore

  let go_block_right cursor =
    go_block_end cursor ;
    (* If cursor is on very last block of the file, go instead to that block beginning *)
    if Cursor.line_is_last cursor && Cursor.char_is_last cursor
      then go_block_start cursor
      else
        while Cursor.next cursor = Continue && is_not_block cursor do
          ()
        done

  let go_block_left cursor =
    go_block_start cursor ;
    while Cursor.prev cursor = Continue && is_not_block cursor do
      ()
    done ;
    go_block_start cursor

  (* CLEANUP: this is a reimplementation of token_up for blocks, but should the behavior be different:
   *          when the line above is not empty, then go to the rightnmost block that match the same position
   *          and remember the "wished" position
   *          this also deals with saving the wished char position when jumping across short line
   *)
  let go_block_up cursor =
    if is_block cursor && Cursor.y cursor > 0
    then
      let x = Cursor.x cursor in
      let y = Cursor.y cursor in
      let n = ref 0 in
      go_block_start cursor ;
      while Cursor.y cursor = y do        (* count the number of block to skip until current cursor block *)
        go_block_left cursor ;
        incr n
      done ;
      Cursor.goto ~x:0 ~y:y cursor ;      (* readjust cursor to start of line *)
      let found = ref false in            (* find the first line with at least as many blocks *)
      let yseek = ref y in
      while not !found && !yseek > 0 do
        decr yseek ;
        Cursor.goto ~x:0 ~y:!yseek cursor ;
        let m = ref !n in
        while !m > 0 && Cursor.y cursor = !yseek do
          go_block_right cursor ;
          decr m
        done ;
        found := Cursor.y cursor = !yseek
      done ;
      if not !found                       (* reset cursor if no block was found *)
      then
        Cursor.goto ~x:x ~y:y cursor

  let movement =
    let open Move in
    function
      | Left    -> go_block_left
      | Right   -> go_block_right
      | Start   -> go_block_start
      | End     -> go_block_end
      | Up      -> go_block_up
      | Down    -> ignore (* TODO *)
end

(* TODO: migrate to Cursor *)
module type DelimiterKind = sig
  (* Returns:
   *    +1 for left delimiter: '(', '[', '{'
   *    -1 for right delimiter: ')', ']', '}'
   *     0 for others *)
  val get_kind : char -> int
end


module DelimMovement(K : DelimiterKind) = struct
  let is_left = (=) 1
  let is_right = (=) (-1)

  let get_kind_at = Cursor.char_get >> K.get_kind

  (* Go to first 'left' delimiter on the left of current position. *)
  let rec go_first_left skip cursor =
    if not skip && cursor |> get_kind_at |> is_left || Cursor.prev cursor = Nomore
      then ()
      else go_first_left false cursor

  (* Go to first 'left' delimiter on the right of current position *)
  let rec go_first_right skip cursor =
    if not skip && cursor |> get_kind_at |> is_left || Cursor.next cursor = Nomore
      then ()
      else go_first_right false cursor

  (* Go to first 'right' delimiter on the left of current position *)
  let rec go_first_end_left skip cursor =
    if not skip && cursor |> get_kind_at |> is_right || Cursor.prev cursor = Nomore
      then ()
      else go_first_end_left false cursor

  (* Move cursor left until balance is 0 *)
  let rec go_left cursor b =
    let b' = b + (get_kind_at cursor) in
    if b' = 0 || Cursor.prev cursor = Nomore
      then ()
      else go_left cursor b'

  (* Move cursor right until balance is 0 *)
  let rec go_right cursor b =
    let b' = b + (get_kind_at cursor) in
    if b' = 0 || Cursor.next cursor = Nomore
      then ()
      else go_right cursor b'

  let go_delim_start cursor =
    let k = get_kind_at cursor in
    if not (is_left k)
      then go_left cursor (0 - k - 1)

  let go_delim_end cursor =
    let k = get_kind_at cursor in
    if not (is_right k)
      then go_right cursor (1 - k)

  let go_delim_up cursor =
    go_delim_start cursor ;
    go_first_end_left true cursor ;
    go_delim_start cursor

  let go_delim_down cursor =
    go_delim_start cursor ;
    go_delim_end cursor ;
    go_first_right true cursor

  let movement : Move.t -> Cursor.t -> unit =
    let open Move in
    function
      | Left    -> go_first_left true
      | Right   -> go_first_right true
      | Up      -> go_delim_up
      | Down    -> go_delim_down
      | Start   -> go_delim_start
      | End     -> go_delim_end
end


module SelectionMovement = struct

  let is_v2_less_or_equal va vb =
    let ay = Vec.y va in
    let by = Vec.y vb in
    ay < by || ay = by && (Vec.x va) <= (Vec.x vb)
  let is_v2_less va vb =
    let ay = Vec.y va in
    let by = Vec.y vb in
    ay < by || ay = by && (Vec.x va) < (Vec.x vb)

  (* PERF: do binary search instead *)
  let selection_prev selection_context cursor =
    let rec loop s c i =
      if i = alen s || is_v2_less_or_equal c (array_get s i |> rect_offset)
        then (i - 1 + (alen s)) mod alen s (* mod == remainder *)
        else loop s c (i + 1)
    in
    let v2 = Cursor.pos cursor in
    if alen selection_context = 0
      then v2
      else loop selection_context v2 0
            |> array_get selection_context
            |> rect_offset

  let selection_next selection_context cursor =
    let rec loop s c i =
      if i = alen s || is_v2_less c (array_get s i |> rect_offset)
        then i mod alen s
        else loop s c (i + 1)
    in
    let v2 = Cursor.pos cursor in
    if alen selection_context = 0
      then v2
      else loop selection_context v2 0
            |> array_get selection_context
            |> rect_offset

  let select_current_rect fn selection_context cursor =
    let rec loop s c i =
      if i = alen s
        then c
        else (
          let r = array_get s i in
          if is_v2_less_or_equal (rect_offset r) c && is_v2_less_or_equal c (rect_end r)
            then fn r
            else loop s c (i + 1))
    in
    let v2 = Cursor.pos cursor in
    if alen selection_context = 0
      then v2
      else loop selection_context v2 0

  let selection_start = select_current_rect rect_offset
  let selection_end   = select_current_rect rect_end

  let movement movement_context =
    let open Move in
    function
      | Up
      | Down    -> Cursor.pos
      | Start   -> selection_start movement_context
      | End     -> selection_end movement_context
      | Left    -> selection_prev movement_context
      | Right   -> selection_next movement_context
end

module Movement = struct
  type selection_context = Util.Rec.rec2 array

  (* TODO: do not repeat this type *)
  type mode = Blocks
            | Words
            | Digits
            | Lines
            | Chars
            | Paragraphs
            | Parens
            | Brackets
            | Braces
            | Selection

  type movement = Left
                | Right
                | Up
                | Down
                | Start
                | End
                | PageUp
                | PageDown
                | FileStart
                | FileEnd

  let mode_to_string =
    function
      | Blocks        -> "Blocks"
      | Words         -> "Words"
      | Digits        -> "Digits"
      | Lines         -> "Lines"
      | Chars         -> "Chars"
      | Paragraphs    -> "Paragraphs"
      | Parens        -> "Parens"
      | Brackets      -> "Brackets"
      | Braces        -> "Braces"
      | Selection     -> "Selection"

  let movement_to_string =
    function
      | Left          -> "Left"
      | Right         -> "Right"
      | Up            -> "Up"
      | Down          -> "Down"
      | Start         -> "Start"
      | End           -> "End"
      | PageUp        -> "PageUp"
      | PageDown      -> "PageDown"
      | FileStart     -> "FileStart"
      | FileEnd       -> "FileEnd"

  (* 
  let page_offset = Config.default.page_size
  *)
  let page_offset = 50

  let move_file_start = Cursor.goto ~x:0 ~y:0
  let move_file_end   = Cursor.line_last

  let move_page_up cursor =
    let y' = (Cursor.y cursor) - page_offset in
    let y'' = max 0 y' in
    Cursor.goto ~y:y'' cursor

  let move_page_down cursor =
    let y' = (Cursor.y cursor) + page_offset in
    let y'' = max 0 y' in
    Cursor.goto ~y:y'' cursor

  let move_line_left cursor =
    Cursor.line_prev cursor |> ignore ;
    Cursor.char_first cursor

  let move_line_right cursor =
    Cursor.line_next cursor |> ignore ;
    Cursor.char_last cursor

  let move_char_left cursor =
    Cursor.char_prev cursor |> ignore ;
    Cursor.xmem_set cursor (Cursor.x cursor)

  let move_char_right cursor =
    Cursor.char_next cursor |> ignore ;
    Cursor.xmem_set cursor (Cursor.x cursor)

  let move_char_up cursor =
    let xmem = Cursor.xmem cursor in
    Cursor.line_prev cursor |> ignore ;
    if Cursor.x cursor < xmem
    then
      Cursor.goto ~x:xmem cursor

  let move_char_down cursor =
    let xmem = Cursor.xmem cursor in
    Cursor.line_next cursor |> ignore ;
    if Cursor.x cursor < xmem
    then
      Cursor.goto ~x:xmem cursor

  let move_while cursor_condition cursor_step_fn cursor =
    let c = ref Cursor.Continue in
    let m = ref false in
    while !c = Cursor.Continue && cursor_condition cursor do
      c := cursor_step_fn cursor ;
      m := !m || ( !c = Cursor.Continue )
    done ;
    !m

  let move_para_start cursor =
    if move_while Cursor.line_not_empty Cursor.line_prev cursor
      then Cursor.line_next cursor |> ignore ;
    Cursor.char_first cursor

  let move_para_end cursor =
    if move_while Cursor.line_not_empty Cursor.line_next cursor
      then Cursor.line_prev cursor |> ignore ;
    Cursor.char_last cursor

  let move_para_up cursor =
    move_para_start cursor ;
    Cursor.line_prev cursor |> ignore ;
    if move_while Cursor.line_is_empty Cursor.line_prev cursor
      then move_para_start cursor

  let move_para_down cursor =
    move_para_end cursor ;
    Cursor.line_next cursor |> ignore ;
    if move_while Cursor.line_is_empty Cursor.line_next cursor
      then move_para_start cursor

  let move_para_left cursor =
    move_para_start cursor ;
    Cursor.line_prev cursor |> ignore ;
    if move_while Cursor.line_is_empty Cursor.line_prev cursor
      then move_para_end cursor

  let move_para_right cursor =
    move_para_end cursor ;
    Cursor.line_next cursor |> ignore ;
    if move_while Cursor.line_is_empty Cursor.line_next cursor
      then move_para_end cursor

  let movement_char =
    let open Move in
    function
      | Start
      | End     -> ignore
      | Left    -> move_char_left
      | Right   -> move_char_right
                   (* TODO: consider changing behavior to go to first above/below line with at
                    *       least a length > to cursor.x *)
      | Up      -> move_char_up
      | Down    -> move_char_down

  let movement_line =
    let open Move in
    function
      | Left    -> move_line_left
      | Right   -> move_line_right
      | Up      -> Cursor.line_prev >> ignore
      | Down    -> Cursor.line_next >> ignore
      | Start   -> Cursor.char_zero
      | End     -> Cursor.char_last

  let movement_paragraph =
    let open Move in
    function
      | Start   -> move_para_start
      | End     -> move_para_end
      | Left    -> move_para_left
      | Right   -> move_para_right
      | Up      -> move_para_up
      | Down    -> move_para_down

  module ParenMovement = DelimMovement(struct
    let get_kind =
      function
      | '('   ->  1
      | ')'   -> -1
      | _     ->  0
  end)

  module BracketMovement = DelimMovement(struct
    let get_kind =
      function
      | '['   ->  1
      | ']'   -> -1
      | _     ->  0
  end)

  module BraceMovement = DelimMovement(struct
    let get_kind =
      function
      | '{'   ->  1
      | '}'   -> -1
      | _     ->  0
  end)

  module Block = BlockMovement(struct
    let is_char_inside_block c = c <> ' ' && is_printable c
  end)

  module Word = BlockMovement(struct
    let is_char_inside_block c = is_alphanum c || c = '_'
  end)

  module Digit = BlockMovement(struct
    let is_char_inside_block = is_digit
  end)

  let selection_movement mov_context direction cursor =
    let v = SelectionMovement.movement mov_context direction cursor in
    Cursor.goto ~x:(Vec.x v) ~y:(Vec.y v) cursor

  let move movement_context =
    function
      | Blocks        -> Block.movement
      | Words         -> Word.movement
      | Digits        -> Digit.movement
      | Lines         -> movement_line
      | Chars         -> movement_char
      | Paragraphs    -> movement_paragraph
      | Parens        -> ParenMovement.movement
      | Brackets      -> BracketMovement.movement
      | Braces        -> BraceMovement.movement
      | Selection     -> selection_movement movement_context

  let apply_movement movement_context mode =
    function
      | Left          -> move movement_context mode Move.Left
      | Right         -> move movement_context mode Move.Right
      | Up            -> move movement_context mode Move.Up
      | Down          -> move movement_context mode Move.Down
      | Start         -> move movement_context mode Move.Start
      | End           -> move movement_context mode Move.End
      | PageUp        -> move_page_up
      | PageDown      -> move_page_down
      | FileStart     -> move_file_start
      | FileEnd       -> move_file_end
end


