open Util
open Util.Arrays
open Util.Vec
open Util.Rec
open Term
open Navigation
open Motion

let starttime = Sys.time ()

let logs = open_out "/tmp/ciseau.log"

(* Debugging flags *)
let kLOG_STATS    = true
let kDEBUG        = false


(* this is a config module for storing all parameters *)
module Config = struct
  open Color

  type colors = {
    operator      : color_cell ;
    structure     : color_cell ;
    string        : color_cell ;
    spacing       : color_cell ;
    numbers       : color_cell ;
    default       : color_cell ;
    cursor_line   : color_cell ;
    current_token : color_cell ;
    selection     : color_cell ;
    line_numbers  : color_cell ;
    focus_header  : color_cell ;
    header        : color_cell ;
    status_normal : color_cell ;
    status_input  : color_cell ;
    user_input    : color_cell ;
    border        : color_cell ;
    no_text       : color_cell ;

    leftright_neighbor  : color_cell ;
    updown_neighbor     : color_cell ;
  }

  type cfg = {
    colors    : colors ;
    page_size : int;
  }

  let darkgray = Color.Gray 2

  let default : cfg = {
    colors = {
      operator = {
        fg    = Green ;
        bg    = darkgray ;
      } ;
      structure = {
        fg    = Red ;
        bg    = darkgray ;
      } ;
      string  = {
        fg    = Yellow ;
        bg    = darkgray ;
      } ;
      spacing = {
        fg    = darkgray ;
        bg    = darkgray ;
      } ;
      numbers = {
        fg    = Magenta ;
        bg    = darkgray ;
      } ;
      default = {
        fg    = White ;
        bg    = darkgray ;
      } ;
      cursor_line = {
        fg    = White ;
        bg    = Black ;
      } ;
      current_token = {
        fg    = White ;
        bg    = Color.Gray 4 ;
      } ;
      selection = {
        fg    = White ;
        bg    = Blue ;
      } ;
      leftright_neighbor = {
        fg    = White ;
        bg    = Red ;
      } ;
      updown_neighbor = {
        fg    = White ;
        bg    = Green ;
      } ;
      line_numbers = {
        fg    = Green ;
        bg    = darkgray ;
      } ;
      focus_header = {
        fg    = darkgray ;
        bg    = Yellow ;
      } ;
      header = {
        fg    = darkgray ;
        bg    = Cyan ;
      } ;
      status_normal = {
        fg    = darkgray ;
        bg    = White ;
      } ;
      status_input = {
        fg    = darkgray ;
        bg    = Red ;
      } ;
      user_input = {
        fg    = White ;
        bg    = darkgray ;
      } ;
      border = {
        fg    = White ;
        bg    = White ;
      } ;
      no_text = {
        fg    = Bold_Magenta ;
        bg    = darkgray ;
      }
    } ;
    page_size = 50;
  }
end


(* TODO: it would be nice to pair this with the reverse mapping in Term.Keys.descr_of to have
 * one place for handling input mapping and configuration reading better *)
module Input = struct
  let ctrl_at               = '\x00'
  let ctrl_a                = '\x01'
  let ctrl_b                = '\x02'
  let ctrl_c                = '\x03'
  let ctrl_d                = '\x04'
  let ctrl_e                = '\x05'
  let ctrl_f                = '\x06'
  let ctrl_g                = '\x07'
  let ctrl_h                = '\x08'
  let ctrl_i                = '\x09'
  let ctrl_j                = '\x0a'
  let ctrl_k                = '\x0b'
  let ctrl_l                = '\x0c'
  let ctrl_m                = '\x0d'
  let ctrl_n                = '\x0e'
  let ctrl_o                = '\x0f'
  let ctrl_p                = '\x10'
  let ctrl_q                = '\x11'
  let ctrl_r                = '\x12'
  let ctrl_s                = '\x13'
  let ctrl_t                = '\x14'
  let ctrl_u                = '\x15'
  let ctrl_v                = '\x16'
  let ctrl_w                = '\x17'
  let ctrl_x                = '\x18'
  let ctrl_y                = '\x19'
  let ctrl_z                = '\x1a'
  let ctrl_left_bracket     = '\x1b'
  let ctrl_backslash        = '\x1c'
  let ctrl_right_bracket    = '\x1d'
  let ctrl_caret            = '\x1e'
  let ctrl_underscore       = '\x1f'
  let space                 = '\x20'
  let del                   = '\x7f'
  let esc                   = ctrl_left_bracket
  let backspace             = ctrl_h
  let tab                   = ctrl_i
  let line_feed             = ctrl_j
  let vtab                  = ctrl_k
  let new_page              = ctrl_l
  let enter                 = ctrl_m
end


module ScreenConfiguration = struct

  type orientation = Normal | Mirror
  type layout = Single | Columns | Rows | ColumnMajor | RowMajor

  type t = {
    layout      : layout ;
    orientation : orientation ;
  }

  let mk_config l o = {
    layout      = l ;
    orientation = o ;
  }

  module Configs = struct
    let zero    = mk_config Single  Normal
    let columns = mk_config Columns Normal
    let rows    = mk_config Rows    Normal
  end

  let flip_orientation =
    function
      | Normal -> Mirror
      | Mirror -> Normal

  let cycle_layout_next =
    function
      | Single      -> Columns
      | Columns     -> Rows
      | Rows        -> ColumnMajor
      | ColumnMajor -> RowMajor
      | RowMajor    -> Single

  let cycle_layout_prev =
    function
      | Single      -> RowMajor
      | Columns     -> Single
      | Rows        -> Columns
      | ColumnMajor -> Rows
      | RowMajor    -> ColumnMajor

  let flip_config_orientation { layout ; orientation } = {
    layout      = layout ;
    orientation = flip_orientation orientation ;
  }

  let cycle_config_layout_next { layout ; orientation } = {
    layout      = cycle_layout_next layout ;
    orientation = orientation ;
  }

  let cycle_config_layout_prev { layout ; orientation } = {
    layout      = cycle_layout_prev layout ;
    orientation = orientation ;
  }

  let split l n =
    let a = l / n in
    let r = l mod n in
    let compute_segment i =
      (* For the first 'r' tiles, add a cumulative offset of 1 for using the remainder of 'l / n'.
       * That cumulative offset happens to be min(tile index, remainder) *)
      let k = min i r in
      let l = min (i + 1) r in
      (k + i * a, l + (i + 1) * a)
    in
    Array.init n compute_segment

  let flip_xy_rect r =
    let x = rect_x r in
    let y = rect_y r in
    let w = rect_w r in
    let h = rect_h r in
    mk_rect y x (h + y) (w + x)

  let rec mk_view_ports total_area n_screen =
    function
      | { layout = Single } ->
          Array.make 1 total_area
      | _ when n_screen = 1 ->
          mk_view_ports total_area 1 Configs.zero
      | { layout = Columns ; orientation = Normal } ->
          let xo = rect_x total_area in
          let yo = rect_y total_area in
          split (rect_x_end total_area) n_screen
            |> Array.map (fun (xl, xr) -> mk_rect (xo + xl) yo (xo + xr) (rect_y_end total_area))
      | { layout = Columns ; orientation = Mirror } ->
          let views = mk_view_ports total_area n_screen (mk_config Columns Normal) in
          array_rev views ;
          views
      | { layout = Rows ; orientation } ->
          mk_config Columns orientation
            |> mk_view_ports (flip_xy_rect total_area) n_screen
            |> Array.map flip_xy_rect
      | { layout = ColumnMajor ; orientation } ->
          let halves = mk_view_ports total_area 2 (mk_config Columns orientation) in
          let minors = mk_view_ports (array_get halves 1) (n_screen - 1) Configs.rows in
          let views = Array.make n_screen (array_get halves 0) in
          array_blit minors 0 views 0 (alen minors) ;
          views
      | { layout = RowMajor ; orientation } ->
          mk_config ColumnMajor orientation
            |> mk_view_ports (flip_xy_rect total_area) n_screen
            |> Array.map flip_xy_rect
end


(* A common buffer for filling Fileview content just before bliting into a backend rendering framebuffer *)
(* TODO: Not threadsafe !! find better place to put that *)
let fb = ref (Framebuffer.mk_framebuffer (mk_v2 300 80))
(* let fb = ref (Framebuffer.mk_framebuffer (mk_v2 4000 100)) *)


module Screen : sig
  type t

  val screen_size       : t -> vec2
  val screen_window     : t -> rec2
  val screen_offset     : t -> vec2
  val screen_width      : t -> int
  val screen_height     : t -> int
  val clear             : t -> unit
  val mk_screen         : Framebuffer.t -> rec2 -> t
  val mk_subscreen      : t -> rec2 -> t
  val put_color_rect    : t -> Color.color_cell -> rec2 -> unit
  val put_line          : t -> x:int -> y:int -> ?offset:int -> ?len:int -> string -> unit
  val put_cursor        : t -> vec2 -> unit
  val put_framebuffer   : t -> Framebuffer.t -> unit

end = struct

  type t = {
    size            : vec2 ;
    window          : rec2 ;
    frame_buffer    : Framebuffer.t ;
  }

  let screen_size    { size }    = size
  let screen_window  { window }  = window
  let screen_offset  { window }  = rect_offset window
  let screen_width   { window }  = rect_w window
  let screen_height  { window }  = rect_h window

  (* Makes a screen with 'fb' as the backend framebuffer.
   * Passed in rectangle defines the screen absolute coordinates w.r.t the framebuffer *)
  let mk_screen fb rect = {
    size          = rect_size rect ;
    window        = rect ;
    frame_buffer  = fb ;
  }

  let clear t =
    Framebuffer.clear_rect t.frame_buffer t.window

  (* Makes a subscreen from this screen.
   * Passed in rectangle defines the subscreen absolut coordinates w.r.t the parent screen *)
  let mk_subscreen { size ; window ; frame_buffer } rect =
    assert_rect_inside size rect ;
    mk_screen frame_buffer (rect_mv (rect_offset window) rect)

  let put_color_rect screen colors rect =
    let x_start   = max (rect_x rect) 0 in
    let y_start   = max (rect_y rect) 0 in
    let x_end     = min (screen_width screen) (rect_w rect) in
    let y_end     = min (screen_height screen) (rect_h rect) in
    let x_offset  = rect_x screen.window in
    let y_offset  = rect_y screen.window in
    let rect'     = mk_rect (x_offset + x_start) (y_offset + y_start) (x_offset + x_end) (y_offset + y_end) in
    Framebuffer.put_color_rect
      screen.frame_buffer
      colors
      rect'

  let put_line screen ~x:x ~y:y ?offset:(offset=0) ?len:(len=0-1) line =
    let len' = if len < 0 then slen line else len in
    let blitend = min screen.size.x (offset + len') in
    let blitlen = blitend - offset in
    if 0 <= y && y < screen.size.y then (
      Framebuffer.clear_line
        screen.frame_buffer
        ~x:(rect_x screen.window)
        ~y:((rect_y screen.window) + y)
        ~len:(rect_w screen.window) ;
      Framebuffer.put_line
        screen.frame_buffer
        ~x:((rect_x screen.window) + x)
        ~y:((rect_y screen.window) + y)
        ~offset:offset
        ~len:blitlen
        line
    )

  let put_cursor screen pos =
    screen.window
      |> rect_offset
      |> v2_add pos
      |> Framebuffer.put_cursor screen.frame_buffer

  let put_framebuffer screen src =
    Framebuffer.put_framebuffer screen.frame_buffer screen.window src
end


module Filebuffer : sig
  type t

  val init_filebuffer     : string -> t
  val from_lines          : string -> string array -> t
  val header              : t -> string
  val filename            : t -> string
  val file_length         : t -> int
  val search              : t -> string -> rec2 array
  val cursor              : t -> vec2 -> Cursor.t

  (* TODO: migrate fill_framebuffer to text cursor and eliminate these two *)
  val line_at             : t -> int -> string
  val line_length         : t -> int -> int

end = struct
  type t = {
    filename    : string ;
    filepath    : string ;
    header      : string ;
    buffer      : string array ;          (* the file data, line per line *)
    buflen      : int ;                   (* number of lines in buffer, may be less than buffer array length *)
  }

  let cursor { buffer } { x ; y } =
    Cursor.mk_cursor buffer x y

  let read_file f =
    let rec loop lines ch =
      match input_line ch with
      | s                     -> Arraybuffer.append lines s ; loop lines ch
      | exception End_of_file -> lines
    in
    let ch = open_in f in
    try
      let r = loop (Arraybuffer.mk_arraybuffer 32 "") ch
                |> Arraybuffer.to_array
      in
        close_in ch ;
        r
    with
      e ->
        close_in ch ;
        raise e

  let from_lines file lines =
    let filepath = (Sys.getcwd ()) ^ "/" ^ file in {
      filename      = file ;
      filepath      = filepath ;
      header        = filepath ^ "  " ^ (lines |> alen |> string_of_int) ^ "L " ;
      buffer        = lines ;
      buflen        = alen lines ;
    }

  let init_filebuffer file =
    file |> read_file |> from_lines file

  let line_at { buffer } y = array_get buffer y  (* fully apply function to avoid closure creation *)

  let line_length { buffer } y = array_get buffer y |> slen (* TODO: take into account '\t' *)

  let file_length { buflen } = buflen
  let filename { filename } = filename
  let header { header } = header

  let search filebuffer target =
    let rec loop_in_one_line width r s y xi acc =
      match Str.search_forward r s xi with
        | exception _ -> acc
        | start ->
            let next_start = start + width in
            let found = mk_rect start y (next_start - 1) y in
            loop_in_one_line width r s y next_start (found :: acc)
    in
    let rec loop_file filebuffer width r y acc =
      if y < filebuffer.buflen
        then
          let acc' = loop_in_one_line width r (line_at filebuffer y) y 0 acc in
          loop_file filebuffer width r (y + 1) acc'
        else acc
    in
    loop_file filebuffer (slen target) (Str.regexp_string target) 0 []
        |> List.rev
        |> Array.of_list
end


type redraw_level = Nodraw | FrameDirty | Redraw


(* TextView is an intermediary step in writing a filebuffer section on the screen
 * It helps with layout composition. *)
module TextView : sig
  type t

  val mk_textview               : int -> t
  val fill                      : t -> int -> Cursor.t -> unit
  val relative_lineno           : t -> unit
  val h_offset                  : t -> int -> unit

end = struct

  (* TODO: also count tabs for elastic tabs ! *)
  type t = {
    linenos           : int array ;
    lines             : string array ;
    line_offsets      : int array ;
    line_lens         : int array ;
    mutable cursor_x  : int ;
    mutable cursor_y  : int ;
    (* Consider adding h_offset and v_offset ? *)
  }

  let no_lineno = min_int (* Used to indicate wrapped lines *)

  let mk_textview len = {
    linenos         = Array.make len 0 ;    (* When a line is wrapped, its lineno is 'no_lineno' *)
    lines           = Array.make len "" ;
    line_offsets    = Array.make len 0 ;
    line_lens       = Array.make len 0 ;
    cursor_x        = 0 ;
    cursor_y        = 0 ;
  }

  let rec fill_lines textview cursor i =
    let line = Cursor.line_get cursor in
    let lineno = 1 + (Cursor.y cursor) in
    array_set textview.linenos i lineno ;
    array_set textview.lines i line ;
    array_set textview.line_offsets i 0 ;
    array_set textview.line_lens i (slen line) ;
    (* BUG? is this skipping the last line ? *)
    if Cursor.line_next cursor = Cursor.Continue && i + 1 < alen textview.lines
      then fill_lines textview cursor (i + 1)

  let fill textview v_offset cursor =
    let lineno_zero = textview.cursor_y - v_offset in
    assert_that (0 <= lineno_zero) ;
    assert_that (0 <= v_offset) ;
    assert_that (v_offset < alen textview.lines) ;
    textview.cursor_x <- Cursor.x cursor ;
    textview.cursor_y <- Cursor.y cursor - v_offset ;
    Cursor.goto ~y:lineno_zero cursor ;
    fill_lines textview cursor 0

  let relative_lineno textview =
    for i = 0 to astop textview.linenos do
      let lineno = textview.linenos.(i) in
      if lineno > no_lineno
        then array_set textview.linenos i (lineno - textview.cursor_y)
    done

  let h_offset textview h_offset =
    (* TODO: needs to be added to the position mapping matrix *)
    textview.cursor_x <- textview.cursor_x - h_offset ;
    for i = 0 to astop textview.line_offsets do
      array_set textview.line_offsets i h_offset
    done

  let view_position_to_text_position x y =
    (* This can be computed if I keep the horizontal offset, and the cursor mapping *)
    mk_v2 x y

end


module Fileview : sig
  type t

  val init_fileview           : Filebuffer.t -> t
  val set_mov_mode            : Movement.mode -> t -> t
(* FIXME: remove the v2 for viewport rectangle size used for recomputing the adjusted view rectangle around the cursor
 * Instead, remember in the fileview the view offset of the previous fileview draw and recompute the new view offset when drawing
 * Benefit: this removes the extra argument in apply_movement and will facilitate the refactoring afterwards *)
  val apply_movement          : Movement.movement -> vec2 -> t -> t
  val cursor                  : t -> Cursor.t
  val adjust_view             : vec2 -> t -> t
  val swap_line_number_mode   : t -> t
  val swap_linebreaking_mode  : t -> t
  val toggle_show_token       : t -> t
  val toggle_show_neighbor    : t -> t
  val toggle_show_selection   : t -> t
  val recenter_view           : vec2 -> t -> t
  val draw                    : t -> Screen.t -> redraw_level -> bool -> unit

end = struct

  open Filebuffer

  type filebuffer = Filebuffer.t
  type screen     = Screen.t

  type numbering_mode = Absolute | CursorRelative

  module LineNumberCache = struct
    let line_number_cache_t1 = Sys.time () ;;
    (* TODO: - dynmically populate cache as needed by resizing the cache array if needed *)

    let negative_offset = 200
    let hardcoded_size  = 10000 + negative_offset

    let format_n n =
      Printf.sprintf "%5d " (n - negative_offset)

    let cache =
      Array.init hardcoded_size format_n

    let get base_offset n =
      array_get cache (base_offset + n + negative_offset)

    let line_number_cache_t2 = Sys.time () ;;
    Printf.fprintf logs "cache %f\n" (line_number_cache_t2 -. line_number_cache_t1) ;;
  end

  type t = {
    filebuffer        : filebuffer ;
    cursor            : Cursor.t ;        (* current position in file space: x = column index, y = row index *)
    view              : vec2 ;            (* x,y offset of the rectangle view into the text *)
    numbering         : numbering_mode ;
    mov_mode          : Movement.mode ;
    show_token        : bool ;
    show_neighbor     : bool ;
    show_selection    : bool ;
    selection_context : Movement.selection_context
  }

  let init_fileview filebuffer = {
    filebuffer        = filebuffer ;
    cursor            = Filebuffer.cursor filebuffer v2_zero ;
    view              = v2_zero ;
    numbering         = CursorRelative ;
    mov_mode          = Movement.Chars ;
    show_token        = true ;
    show_neighbor     = true ;
    show_selection    = true ;
    selection_context = Filebuffer.search filebuffer "end" ;
  }

  let set_mov_mode m t = {
    t with
      mov_mode = m ;
  }

  let cursor { cursor } =
    cursor

  let view_height { view ; filebuffer } =
    (Filebuffer.file_length filebuffer) - view.y

  let adjust_view { x ; y } t =
    let text_height = y - 2 in (* -1 for header line, -1 for indexing starting at 0 *)
    let text_width  = x - 6 in (* -6 for line numbering CLEANUP: remove this hardcoded offset *)
    let view_x =
      let x = Cursor.x t.cursor in
      if x < t.view.x then
        0
      else if x > t.view.x + text_width then
        x - text_width
      else
        t.view.x
    in
    let view_y =
      let y = Cursor.y t.cursor in
      if y < t.view.y then
        y
      else if y > t.view.y + text_height then
        y - text_height
      else
        t.view.y
    in {
      t with
        view  = mk_v2 view_x view_y ;
    }

  let apply_movement mov screen_size t =
    let x = Cursor.x t.cursor in
    let y = Cursor.y t.cursor in
    (* CLEANUP: it is mayne not a good diea to have cursor mutation in place right here *)
    let _ = Movement.apply_movement t.selection_context t.mov_mode mov t.cursor in
    let x' = Cursor.x t.cursor in
    let y' = Cursor.y t.cursor in

    if kDEBUG then (
      let msg =
        Printf.sprintf"apply_movement after mode=%s mov=%s %d,%d -> %d,%d\n"
          (Movement.mode_to_string t.mov_mode)
          (Movement.movement_to_string mov)
          y x
          y' x'
      in
      assert_that (y' >= 0) ~msg:msg ;
      assert_that (y' < Filebuffer.file_length t.filebuffer) ~msg:msg
    ) ;

    adjust_view screen_size { t with cursor = t.cursor }

  let swap_line_number_mode t =
    let new_mode = match t.numbering with
    | Absolute        -> CursorRelative
    | CursorRelative  -> Absolute
    in {
      t with
        numbering = new_mode ;
    }

  let swap_linebreaking_mode = id

  let toggle_show_token t = {
    t with
      show_token  = not t.show_token ;
    }

  let toggle_show_neighbor t = {
    t with
      show_neighbor = not t.show_neighbor ;
  }

  let toggle_show_selection t = {
    t with
      show_selection  = not t.show_selection ;
  }

  (* TODO: should y_recenter and x_recenter together or separately ? *)
  let recenter_view { x ; y } t =
    let view_x = (Cursor.x t.cursor) - x / 2 in
    let view_y = (Cursor.y t.cursor) - y / 2 in {
      t with
        view = mk_v2 (max view_x 0) (max view_y 0) ;
    }

  (* TODO: move drawing in separate module ? *)

  (* CLEANUP: probably I should draw the line number separately from the main text: this would have a number
   * of benefits in term of codeflow simplicity and not having to put +6 horizontal offsets everywhere and
   * recreate the same objects. *)

  let frame_default_line      = "~  "

  (* TODO: fileview should remember the last x scrolling offset and make it sticky, like the y scrolling *)
  let fill_framebuffer t is_focused screen framebuffer =
    let screen_size = Screen.screen_size screen in
    let text_width  = screen_size.x in
    let text_height = screen_size.y in
    let text_stop_y = min text_height (view_height t) in
    let text_cursor_x = Cursor.x t.cursor in
    let text_cursor_y = Cursor.y t.cursor in

    let line_number_offset =
      match t.numbering with
        | Absolute        -> 1
        | CursorRelative  -> - (Cursor.y t.cursor)
    in

    let last_x_index = text_width - 6 in
    let base_scrolling_offset = last_x_index - 1 in
    let (screen_cursor_x, scrolling_offset) =
      if text_cursor_x < last_x_index
        then (text_cursor_x, 0)
        else (base_scrolling_offset, t.view.x) (* t.cursor.x - base_scrolling_offset) *)
    in
    let cursor = mk_v2 (screen_cursor_x + 6) (text_cursor_y - t.view.y) in
    if is_focused then
      Screen.put_cursor screen cursor ;

    let x_scrolling_offset = scrolling_offset in

    (* Text area *)
    for i = 0 to text_stop_y - 1 do
      let line_idx = i + t.view.y in
      let l = Filebuffer.line_at t.filebuffer line_idx in
      let x_len = (slen l) - x_scrolling_offset in
      Framebuffer.put_line framebuffer ~x:0 ~y:i ~len:6 (LineNumberCache.get line_number_offset line_idx) ;
      if x_len > 0 then
        Framebuffer.put_line framebuffer ~x:6 ~y:i ~offset:x_scrolling_offset ~len:x_len l
    done ;

    (* Text area color blocks *)
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.line_numbers
      (mk_rect 0 0 6 text_stop_y) ;

    (* Show cursor lines *)
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.cursor_line
      (mk_rect 6 (text_cursor_y - t.view.y) text_width (text_cursor_y - t.view.y)) ;
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.cursor_line
      (mk_rect (text_cursor_x + 6 - x_scrolling_offset) 0 (text_cursor_x + 7 - x_scrolling_offset) text_height) ;

    (* Show token where cursor currently is *)
(*
 * FIXME
    if t.show_token then (
      let token_s = Movement.apply_movement t.context t.mov_mode Movement.Start t.filebuffer t.cursor |> Cursor.pos in
      let token_e = Movement.apply_movement t.context t.mov_mode Movement.End t.filebuffer token_s |> Cursor.pos in
      let y_start = token_s.y - t.view.y in
      let y_end   = token_e.y - t.view.y in
      (* The current token can leak out of the current screen: bound start and stop to the screen *)
      for y = max 0 y_start to min y_end text_stop_y do
        let len = Filebuffer.line_length t.filebuffer (y + t.view.y) in
        let x0 =
          if y = y_start then token_s.x else 0 in       (* start from x=0 for lines after the first *)
        let x1 =
          if y = y_end then (token_e.x + 1) else len in (* ends at end-of-line for lines before the last *)
        let x0' = 6 + (max 0 ((min x0 x1) - x_scrolling_offset)) in
        let x1' = 6 + (max x0 x1) - x_scrolling_offset in
        (* TODO: this should use a put_color_line api to avoid making rect records *)
        (* BUG: add horizontal_scrolling correction ?? *)
        if 6 <= x0' && x0' < x1' then
          Framebuffer.put_color_rect
            framebuffer Config.default.colors.current_token (mk_rect x0' y x1' y)
      done ;
    ) ;
*)

    (* Show selection *)
    if t.show_selection then (
      let show_selection selection_rect =
          let tl_x = 6 + (max 0 ((rect_x selection_rect) - x_scrolling_offset)) in
          let tl_y = (rect_y selection_rect) - t.view.y in
          let br_x = (rect_x_end selection_rect) + 7 - x_scrolling_offset in
          let br_y = (rect_y_end selection_rect) - t.view.y in
          if 0 <= br_y && tl_y < text_height && tl_x < br_x then (
            let r = mk_rect tl_x tl_y br_x br_y in
            Framebuffer.put_color_rect framebuffer Config.default.colors.selection r
          )
      in
      Array.iter show_selection t.selection_context ;
    ) ;

(*
 * FIXME
    (* Show Left/Right/Up/Down tokens relatively to where current token is *)
    if t.show_neighbor then (
      let lightup_pixel t framebuffer colors pos =
          let x = pos.x + 6 - x_scrolling_offset in
          let y = pos.y - t.view.y  in
          if 6 <= x then
            Framebuffer.put_color_rect framebuffer colors (mk_rect x y (x + 1) y)
      in

      Movement.apply_movement t.context t.mov_mode Movement.Left t.filebuffer t.cursor
        |> Cursor.pos
        |> lightup_pixel t framebuffer Config.default.colors.leftright_neighbor ;
      Movement.apply_movement t.context t.mov_mode Movement.Right t.filebuffer t.cursor
        |> Cursor.pos
        |> lightup_pixel t framebuffer Config.default.colors.leftright_neighbor ;
      Movement.apply_movement t.context t.mov_mode Movement.Up t.filebuffer t.cursor
        |> Cursor.pos
        |> lightup_pixel t framebuffer Config.default.colors.updown_neighbor ;
      Movement.apply_movement t.context t.mov_mode Movement.Down t.filebuffer t.cursor
        |> Cursor.pos
        |> lightup_pixel t framebuffer Config.default.colors.updown_neighbor ;
    ) ;
*)

    (* No-text area *)
    for y = text_stop_y to text_height do
      Framebuffer.put_line framebuffer ~x:0 ~y:y frame_default_line
    done ;
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.no_text
      (mk_rect 0 text_stop_y 1 text_height) ;

    (* Cursor vertical line *)
    Framebuffer.put_color_rect
      framebuffer
      Config.default.colors.string
      (mk_rect 0 cursor.y 6 cursor.y)

  let put_border_frame t screen header_color =
    Screen.put_line screen ~x:0 ~y:0
      (Printf.sprintf
        "%s %d,%d  mode=%s"
        (Filebuffer.header t.filebuffer)
        (Cursor.y t.cursor) (Cursor.x t.cursor)
        (Movement.mode_to_string t.mov_mode)) ;
    (* header *)
    Screen.put_color_rect
      screen
      header_color
      (mk_rect 0 0 (Screen.screen_width screen) 0) ;
    (* border *)
    Screen.put_color_rect
      screen
      Config.default.colors.border
      (* BUG: this overlap with first char of top header ! *)
      (mk_rect 0 1 1 (Screen.screen_height screen))

  let draw t screen redraw is_focused =
    let textscreen = Screen.mk_subscreen screen
      (* 1 for border column, 1 for header space *)
      (mk_rect 1 1 (Screen.screen_width screen) (Screen.screen_height screen))
    in
    let header_color =
      if is_focused
        then Config.default.colors.focus_header
        else Config.default.colors.header
    in
    let framebuffer = !fb in
    match redraw with
    | Nodraw ->
        ()
    | FrameDirty ->
        put_border_frame t screen header_color
    | Redraw -> (
        put_border_frame t screen header_color ;
        Screen.clear textscreen ;
        Framebuffer.clear framebuffer ;
        fill_framebuffer t is_focused textscreen framebuffer ;
        Screen.put_framebuffer textscreen framebuffer ;
      )


(* NEW fill_screen function

  features to take into account:
    correctly iterating through the text line per line
    put the cursor at the right vertical offset
    put beginning of lines at the right horizontal offset
    correctly expand tabs
      support fixed tabs columns
      support elastic tabs
    put all color segments
    support mouse cursor to text cursor mapping
    support horizontal and vertical screen dragging

    screen and/or framebuffer should be enhanced with a mouse_cursor _coord_mapping

    possible data transformation flow:

      cursor + view height
        -> text array  // array can be cached for the screen and reused
        -> text array + tab annotations
        -> text array is mutated to account for line overflow, line clipping, horizontal offset
        -> text array is finally blitted onto screen ?

        Q: where to insert selections and color blocks ?
          text array can keep range per line of y offset and x ranges -> this guide the coloring
*)

end


module Stats = struct

  type t = {
    gc_stats            : Gc.stat ;
    last_major_words    : float ;
    last_minor_words    : float ;
    timestamp           : float ;
    last_input_duration : float ;
    last_cycle_duration : float ;
    last_key_input      : Keys.key ;
  }

  let init_stats () = {
    gc_stats            = Gc.quick_stat () ;
    last_major_words    = 0. ;
    last_minor_words    = 0. ;
    timestamp           = Sys.time () ;
    last_input_duration = 0. ;
    last_cycle_duration = 0. ;
    last_key_input      = Keys.Key '?' ;
  }

  let update_stats key now input_duration stats = {
    gc_stats            = Gc.quick_stat () ;
    last_major_words    = stats.gc_stats.Gc.major_words ;
    last_minor_words    = stats.gc_stats.Gc.minor_words ;
    timestamp           = now ;
    last_input_duration = input_duration ;
    last_cycle_duration = now -. stats.timestamp ;
    last_key_input      = key ;
  }

  let word_byte_size =
    float (Sys.word_size / 8)

  let format_memory_counter word_count =
    match word_count *. word_byte_size with
    | x when x < 1024.          -> Printf.sprintf "%.2fB" x
    | x when x < 1024. *. 1024. -> Printf.sprintf "%.2fkB" (x /. 1024.)
    | x                         -> Printf.sprintf "%.2fMB" (x /. 1024. /. 1024.)

  let format_stats stats =
    let open Gc in
    Printf.sprintf "  heap = %s  alloc = (major: %s  minor: %s)  time = %.3fms"
      (format_memory_counter (float stats.gc_stats.Gc.heap_words))
      (format_memory_counter (stats.gc_stats.Gc.major_words -. stats.last_major_words))
      (format_memory_counter (stats.gc_stats.Gc.minor_words -. stats.last_minor_words))
      (1000. *. (stats.last_cycle_duration -. stats.last_input_duration))

  let log_stats f stats =
    output_string f "frame_time_ms " ;
    output_float f stats.last_cycle_duration ;
    output_string f "\n" ;
    output_string f "heap_words " ;
    output_int f stats.gc_stats.Gc.heap_words ;
    output_string f "\n" ;
    output_string f "minor_words " ;
    output_float f (stats.gc_stats.Gc.minor_words -. stats.last_minor_words) ;
    output_string logs "\n" ;
    output_string f "key " ;
    output_string f (Keys.descr_of stats.last_key_input) ;
    let open Keys in
    match stats.last_key_input with
      | Key c -> 
          output_string f " " ;
          output_int f (Char.code c)
      | _ -> () ;
    output_string logs "\n"
end


module Tileset = struct

  type op = Resize of rec2
          | FileviewOp of (vec2 -> Fileview.t -> Fileview.t)
          | RotateViewsLeft
          | RotateViewsRight
          | ScreenLayoutCycleNext
          | ScreenLayoutCyclePrev
          | ScreenLayoutFlip
          | FocusNext
          | FocusPrev
          | FocusMain
          | BringFocusToMain
          | Selection of vec2

  type t = {
    screen_size   : rec2 ;
    screen_config : ScreenConfiguration.t ;
    screen_tiles  : rec2 array ;
    focus_index   : int ;
    fileviews     : Fileview.t array ;
    redrawing     : redraw_level array ;
  }

  let adjust_fileviews screen_tiles fileviews =
    let n_tiles = alen screen_tiles in
    let adjust_fileview i =
      let view = array_get fileviews i in
      (* In Single tile mode, len tiles < len fileviews *)
      if i < n_tiles
        then Fileview.adjust_view (array_get screen_tiles i |> rect_size) view
        else view
    in
    Array.init (alen fileviews) adjust_fileview

  let mk_tileset ?draw:(redraw=Redraw) focus_index size screenconfig fileviews =
    assert (focus_index < alen fileviews) ;
    let screen_tiles  = ScreenConfiguration.mk_view_ports size (alen fileviews) screenconfig
    in {
      screen_size   = size ;
      screen_config = screenconfig ;
      screen_tiles  = screen_tiles ;
      focus_index   = focus_index ;
      fileviews     = adjust_fileviews screen_tiles fileviews ;
      redrawing     = Array.make (alen fileviews) redraw ;
    }

  let set_focus t focus_index =
    let t' = {
      t with
        focus_index = focus_index ;
        redrawing   = Array.make (alen t.fileviews) FrameDirty ;
    } in
    if t.screen_config.layout = ScreenConfiguration.Single then
      array_set t'.redrawing focus_index Redraw ;
    t'

  let next_focus_index t =
    (t.focus_index + 1) mod alen t.fileviews

  let prev_focus_index t =
    (t.focus_index + (alen t.fileviews) - 1) mod alen t.fileviews

  let draw_fileview t frame_buffer view_idx tile_idx is_focused =
    Fileview.draw
        (array_get t.fileviews view_idx)
        (array_get t.screen_tiles tile_idx |> Screen.mk_screen frame_buffer)
        (array_get t.redrawing view_idx)
        is_focused

  let draw_fileviews t frame_buffer =
    let n_screens = alen t.screen_tiles in
    if n_screens = 1
      then
        draw_fileview t frame_buffer t.focus_index 0 true
      else
        for i = 0 to n_screens - 1 do
          draw_fileview t frame_buffer i i (i = t.focus_index)
        done

  let get_focused_tile t =
    if alen t.screen_tiles > 1
      then array_get t.screen_tiles t.focus_index
      else array_get t.screen_tiles 0

  let apply_op t =
    (* Any operation that changes the terminal size has to use mk_tileset for recomputing the tiles
     * Any operation that remaps tiles and fileviews has to use mk_tileset for adusting fileview height *)
    function
      | Resize new_screen_size ->
          mk_tileset
            t.focus_index new_screen_size t.screen_config t.fileviews
      | FileviewOp fileview_op ->
          let fileviews' = Array.copy t.fileviews in
          t.focus_index
            |> array_get fileviews'
            |> fileview_op (t |> get_focused_tile |> rect_size)
            |> array_set fileviews' t.focus_index ;
          let t' = {
            t with
              fileviews = fileviews' ;
              redrawing = Array.make (alen fileviews') Nodraw ;
          } in
            array_set t'.redrawing t'.focus_index Redraw ;
            t'
      | ScreenLayoutCycleNext ->
          let new_config = ScreenConfiguration.cycle_config_layout_next t.screen_config in
          mk_tileset
            t.focus_index t.screen_size new_config t.fileviews
      | ScreenLayoutCyclePrev ->
          let new_config = ScreenConfiguration.cycle_config_layout_prev t.screen_config in
          mk_tileset
            t.focus_index t.screen_size new_config t.fileviews
      | ScreenLayoutFlip ->
          let new_config = ScreenConfiguration.flip_config_orientation t.screen_config in
          mk_tileset
            t.focus_index t.screen_size new_config t.fileviews
      | FocusNext ->
          set_focus t (next_focus_index t)
      | FocusPrev ->
          set_focus t (prev_focus_index t)
      | FocusMain ->
          set_focus t 0
      | BringFocusToMain ->
          let fileviews' = Array.copy t.fileviews in
          array_get t.fileviews t.focus_index |> array_set fileviews' 0 ;
          array_get t.fileviews 0             |> array_set fileviews' t.focus_index ;
          let t' =
            mk_tileset ~draw:Nodraw 0 t.screen_size t.screen_config fileviews'
          in
            array_set t'.redrawing 0 Redraw ;
            array_set t'.redrawing t.focus_index Redraw ;
            t'
      | RotateViewsRight ->
          let fileviews' = Array.copy t.fileviews in
          let focus_index' = next_focus_index t in
          array_swap fileviews' t.focus_index focus_index' ;
          let t' =
            mk_tileset ~draw:Nodraw focus_index' t.screen_size t.screen_config fileviews'
          in
            array_set t'.redrawing t.focus_index Redraw ;
            array_set t'.redrawing focus_index' Redraw ;
            t'
      | RotateViewsLeft ->
          let fileviews' = Array.copy t.fileviews in
          let focus_index' = prev_focus_index t in
          array_swap fileviews' t.focus_index focus_index' ;
          let t' =
            mk_tileset ~draw:Nodraw focus_index' t.screen_size t.screen_config fileviews'
          in
            array_set t'.redrawing t.focus_index Redraw ;
            array_set t'.redrawing focus_index' Redraw ;
            t'
      | Selection pos ->
          let pos_in_tile {x ; y } r =
            let x0 = rect_x r in
            let y0 = rect_y r in
            let x1 = rect_x_end r in
            let y1 = rect_y_end r in
            x0 <= x && x < x1 && y0 <= y && y < y1
          in
          let i = array_find (pos_in_tile pos) t.screen_tiles in
          Printf.fprintf logs "selecting tile %d for pos %d,%d\n" i pos.x pos.y ;
          flush logs ;
          if i < 0
            then t
            else set_focus t i
                  (* TODO: adjust cursor to p inside that tile *)
end


(* Experimenting with parametrizing raw input capture for reusability:
 *   - insert mode
 *   - commands
 *   - search
 * TODO: consider plumbing in token movements ?
 *)
module InputCapture = struct

  type 'a t = {
    cursor:   int ;
    content:  string ;
    endvalue: 'a ;
  }

  type 'a input_parser = {
    next_char : unit -> char ;
    escape    : char -> 'a option ;
  }

  (* Pass in an escape hatch *)
  let capture_input { next_char ; escape } =
    let rec loop cursor content =
      let c = () |> next_char in
      match escape c with
        | Some a  -> {
          cursor    = cursor ;
          content   = content ;
          endvalue  = a ;
        }
        | None    ->
            (match c with
              (* BUG: do inserts and deletes at cursor position *)
              | '\127'  ->
                let content' = String.sub content 0 (max 0 ((slen content) - 1)) in
                let cursor' = max 0 (cursor - 1) in
                loop cursor' content'
              | c       ->
                loop (cursor + 1) (content ^ (Char.escaped c)))
    in
      loop 0 ""
end


module Ciseau = struct

  type mode = Normal
            | RawInput

  let mode_to_string =
    function
      | Normal      ->  "Normal"
      | RawInput    ->  "RawInput"

  let mode_to_color =
    function
      | Normal      ->  Config.default.colors.status_normal
      | RawInput    ->  Config.default.colors.status_input

  type pending_command_atom = Digit of int

  type pending_command = None
                       | Number of int list

  (* Represents an editor command *)
  type command = Noop
               | Stop     (* this is an Editor command *)
               | Resize   (* this is a TilesetOp *)
               | TilesetOp of Tileset.op
               | MoveOp of Movement.movement
               | MoveModeOp of Movement.mode
               | View of (Fileview.t -> Fileview.t) (* View ops should probably be moved to Tileset *)
               | Pending of pending_command_atom
               | Mode of mode

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
    term_dim        : vec2 ;
    term_dim_descr  : string ;
    frame_buffer    : Framebuffer.t ;
    running         : bool ;
    status_screen   : Screen.t ;
    tileset         : Tileset.t ;
    filebuffers     : Filebuffer.t array ; (* TODO: turn this into buffer management layer *)
    user_input      : string ;
    pending_input   : pending_command ;
    stats           : Stats.t ;
    log_stats       : bool ;
    mode            : mode ;
  }

  let main_screen_dimensions term_dim =
    mk_rect 0 0 term_dim.x (term_dim.y - 2)

  let status_screen_dimensions term_dim =
    mk_rect 0 (term_dim.y - 2) term_dim.x term_dim.y

  let mk_status_screen frame_buffer term_dim =
    term_dim |> status_screen_dimensions |> Screen.mk_screen frame_buffer

  let mk_window_size_descr { x = w ; y = h } =
    Printf.sprintf "(%d x %d)" w h

  let test_mk_filebuffers file = [|
      Filebuffer.init_filebuffer file ;
      Filebuffer.init_filebuffer "./start_tmux.sh" ;
      Filebuffer.init_filebuffer "./ioctl.c" ;
      (*
      Filebuffer.init_filebuffer "./Makefile" ; (* FIX tabs *)
      *)
    |]

  let test_mk_fileviews term_dim =
    Array.map Fileview.init_fileview

  let mk_tileset term_dim filebuffers =
    filebuffers
      |> Array.map Fileview.init_fileview
      |> Tileset.mk_tileset 0 (main_screen_dimensions term_dim) ScreenConfiguration.Configs.columns

  let init_editor file =
    let term_dim = Term.terminal_dimensions () in
    let frame_buffer = Framebuffer.mk_framebuffer term_dim in
    let filebuffers = test_mk_filebuffers file in
    {
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      frame_buffer    = frame_buffer ;
      running         = true ;
      status_screen   = mk_status_screen frame_buffer term_dim ;

      tileset         = mk_tileset term_dim filebuffers ;
      filebuffers     = filebuffers ;

      user_input      = "" ;
      pending_input   = None;
      stats           = Stats.init_stats () ;
      log_stats       = kLOG_STATS ;
      mode            = Normal ;
    }

  let resize_editor editor =
    let term_dim = Term.terminal_dimensions () in
    let frame_buffer = Framebuffer.mk_framebuffer term_dim in
    {
      editor with
      term_dim        = term_dim ;
      term_dim_descr  = mk_window_size_descr term_dim ;
      frame_buffer    = frame_buffer ;
      status_screen   = mk_status_screen frame_buffer term_dim ;
      tileset         = Tileset.apply_op editor.tileset (Tileset.Resize (main_screen_dimensions term_dim)) ;
    }

  let stop_editor editor = {
      editor
        with
        running = false
    }

  let apply_operation op editor = {
      editor
        with
        tileset = Tileset.apply_op editor.tileset op
    }

  let queue_pending_command n editor = {
      editor
        with
        pending_input = enqueue_digit n editor.pending_input
    }

  let change_mode mode editor = {
      (* TODO: needs to apply necessary cleanups from outgoing mode *)
      editor
        with
        mode        = mode ;
        user_input  = "" ;
    }

  let rec apply_command =
    function
      | Noop    -> id
      | Stop    -> stop_editor
      | Resize  -> resize_editor
      | TilesetOp op ->
          apply_operation op
      | MoveOp m ->
          let fn = Fileview.apply_movement m in
          let op = Tileset.FileviewOp fn in
          apply_operation op
      | MoveModeOp m ->
          let fn any t = Fileview.set_mov_mode m t in
          let op = Tileset.FileviewOp fn in
          apply_operation op
      | View fn ->
          let fn' any fileview = fn fileview in
          let op = Tileset.FileviewOp fn' in
          apply_operation op
      | Pending (Digit n) ->
          (* cannot happen ?? *)
          queue_pending_command n
      | Mode mode ->
          change_mode mode

  let apply_command_with_repetition n command editor =
    match command with
    | MoveOp m ->
          let rec loop m n viewsize fb =
            if (n > 0)
              then loop m (n - 1) viewsize (Fileview.apply_movement m viewsize fb)
              else fb
          in
          let fileview_op = Tileset.FileviewOp (loop m n) in {
            editor with
            tileset = Tileset.apply_op editor.tileset fileview_op ;
            pending_input = None ;
          }
    | Pending (Digit n) ->
          queue_pending_command n editor
      (* for other command, flush any pending digits *)
    | _ ->
          apply_command command { editor with pending_input = None }

  let show_status editor =
    let status_text1 = "Ciseau stats: win = "
                      ^ editor.term_dim_descr
                      ^ (Stats.format_stats editor.stats)
    in
    let status_text2 = editor.user_input
    in
    Screen.put_line editor.status_screen ~x:0 ~y:0 status_text1 ;
    Screen.put_line editor.status_screen ~x:0 ~y:1 status_text2 ;
    Screen.put_color_rect editor.status_screen (mode_to_color editor.mode) (mk_rect 0 0 editor.term_dim.x 0)

  let refresh_screen editor =
    Tileset.draw_fileviews editor.tileset editor.frame_buffer ;
    show_status editor ;
    Framebuffer.render editor.frame_buffer ;
    editor

  let command_key_table : command array = Array.make 255 Noop

  let _ =
    let open Input in
    Array.iter
    (fun (c, command) -> array_set command_key_table (Char.code c) command)
    [|
      (tab             , Mode RawInput) ;
      (ctrl_c          , Stop) ;
      ('\\'            , View Fileview.swap_line_number_mode) ;
      ('|'             , View Fileview.swap_linebreaking_mode) ;
      (':'             , View Fileview.toggle_show_token) ;
      (';'             , View Fileview.toggle_show_neighbor) ;
      ('\''            , View Fileview.toggle_show_selection) ;
      (* CLEANUP: try to separate TilesetOp and FileviewOp with different variants *)
      ('('             , TilesetOp Tileset.RotateViewsLeft) ;
      (')'             , TilesetOp Tileset.RotateViewsRight) ;
      ('{'             , TilesetOp Tileset.ScreenLayoutCyclePrev) ;
      ('}'             , TilesetOp Tileset.ScreenLayoutCycleNext) ;
      ('['             , TilesetOp Tileset.FocusPrev) ;
      (']'             , TilesetOp Tileset.FocusNext) ;
      ('_'             , TilesetOp Tileset.ScreenLayoutFlip) ;
      ('-'             , TilesetOp Tileset.BringFocusToMain) ;
      ('='             , TilesetOp Tileset.FocusMain) ;
      ('+'             , Resize) ;
      (ctrl_z          , TilesetOp (Tileset.FileviewOp Fileview.recenter_view)) ;
      (' '             , TilesetOp (Tileset.FileviewOp Fileview.recenter_view)) ;
      ('w'             , MoveModeOp Movement.Words) ;
      ('W'             , MoveModeOp Movement.Blocks) ;
      ('v'             , MoveModeOp Movement.Lines) ;
      ('B'             , MoveModeOp Movement.Lines) ;
      ('c'             , MoveModeOp Movement.Chars) ;
      ('s'             , MoveModeOp Movement.Selection) ;
      ('d'             , MoveModeOp Movement.Digits) ;
      ('z'             , MoveModeOp Movement.Paragraphs) ;
      ('x'             , MoveModeOp Movement.Parens) ;
      ('k'             , MoveOp Movement.Up) ;
      ('j'             , MoveOp Movement.Down) ;
      ('l'             , MoveOp Movement.Right) ;
      ('h'             , MoveOp Movement.Left) ;
      ('H'             , MoveOp Movement.Start) ;
      ('L'             , MoveOp Movement.End) ;
      ('J'             , MoveOp Movement.FileEnd) ;
      ('K'             , MoveOp Movement.FileStart) ;
      (ctrl_u          , MoveOp Movement.PageUp) ;
      (ctrl_d          , MoveOp Movement.PageDown) ;
      ('0'             , Pending (Digit 0)) ;
      ('1'             , Pending (Digit 1)) ;
      ('2'             , Pending (Digit 2)) ;
      ('3'             , Pending (Digit 3)) ;
      ('4'             , Pending (Digit 4)) ;
      ('5'             , Pending (Digit 5)) ;
      ('6'             , Pending (Digit 6)) ;
      ('7'             , Pending (Digit 7)) ;
      ('8'             , Pending (Digit 8)) ;
      ('9'             , Pending (Digit 9)) ;
    |]

  let key_to_command =
    let open Keys in
    function
      | Key c               -> array_get command_key_table (Char.code c)
      | Click pos           -> TilesetOp (Tileset.Selection pos)
      | ClickRelease _      -> Noop
      | Escape_Z            -> Noop
      | ArrowUp             -> MoveOp Movement.Up
      | ArrowDown           -> MoveOp Movement.Down
      | ArrowLeft           -> MoveOp Movement.Left
      | ArrowRight          -> MoveOp Movement.Right
      | EINTR               -> Resize

  let process_command editor =
    match editor.pending_input with
    | None          -> apply_command
    | Number digits -> apply_command_with_repetition (max 1 (dequeue_digits digits))

  (* TODO: replace by a proper history of previous inputs *)
  let update_normal_mode_command key editor =
    let user_input = (pending_command_to_string editor.pending_input)
                   ^  Keys.descr_of key
                   ^ " "
                   ^ editor.user_input
    in
    let user_input' =
      if slen user_input > editor.term_dim.x
        then String.sub user_input 0 editor.term_dim.x
        else user_input
    in {
      editor with
      user_input = user_input' ;
    }

  let update_userinput key editor =
    match editor.mode with
      | Normal    -> update_normal_mode_command key editor
      | RawInput  -> editor

  let rawinput_append c editor = {
      editor with
        user_input = editor.user_input ^ (Char.escaped c) ;
    }

  let rawinput_delete editor =
    let newlen = max 0 ((slen editor.user_input) - 1)
    in {
      editor with
        user_input = String.sub editor.user_input 0 newlen
    }

  let rawinput_update =
    let open Keys in
    function
      | ArrowUp
      | ArrowDown
      | ArrowLeft
      | ArrowRight
      | Click _
      | ClickRelease _                    ->  id
      | EINTR                             ->  resize_editor
      | Escape_Z                          ->  change_mode Normal
      | Key c when c = Input.ctrl_c
                                          ->  stop_editor
      (* | Key c when c = return ->  finish_input_sequence TODO: signal end of rawinput *)
      | Key c when c = Input.tab          ->  rawinput_append '\t'
      | Key c when c = Input.backspace    ->  rawinput_delete
      | Key c                             ->  rawinput_append c

  (* TODO: introduce state_machine for decoupling editor struct from input processing *)
  let process_key key editor =
    match editor.mode with
      | Normal ->
          let fn =
            key |> key_to_command
                |> process_command editor
          in
          editor |> fn |> update_userinput key
      | RawInput ->
          rawinput_update key editor

  let update_stats key input_duration editor =
    (* CLEANUP remove 'key' and instead get last action from editor *)
    let now = Sys.time () in
    let stats' = Stats.update_stats key now input_duration editor.stats in
    if editor.log_stats then
      Stats.log_stats logs stats' ;
    {
      editor with
        stats = stats'
    }

  let process_events (next_key_fn : unit -> Keys.key) (editor : editor) : editor =
    let t1 = Sys.time () in
    let key = next_key_fn () in
    let t2 = Sys.time () in
      editor
        |> process_key key
        |> update_stats key (t2 -. t1)

  let rec loop (next_key_fn : unit -> Keys.key) (editor : editor) : unit =
    if editor.running then
      editor
        |> refresh_screen
        |> process_events next_key_fn
        |> loop next_key_fn

  let main () =
    try
      let file =
        if alen Sys.argv > 1
          then Sys.argv.(1)
          else __FILE__
      in
        Term.terminal_set_raw () ;
        file
          |> init_editor
          |> loop (Keys.make_next_key_fn ()) ;
        Term.terminal_restore ()
    with
      e ->  Term.terminal_restore () ;
            Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
            Printexc.print_backtrace stdout

end


module Fuzzer = struct

  (*
   * FIXME: generate real key list
  let fuzz_keys =
    Keys.defined_keys
      |> List.map (fun (k, _, _) -> k)
      |> List.filter ((<>) Keys.Ctrl_c)
      |> List.filter ((<>) Keys.EINTR)
      |> Array.of_list
  *)
  let fuzz_keys = [|
    Keys.Key 'a'
  |]

  let next_key r n =
    let l = alen fuzz_keys in
    let i = ref 0 in
    let rec loop () =
      (*
      Unix.sleepf 0.01 ;
      *)
      incr i ;
      if !i > n
        then Keys.Key Input.ctrl_c
        else
          Random.State.int r l |> array_get fuzz_keys
    in loop

  let main n =
    try
      let file =
        if alen Sys.argv > 1
          then Sys.argv.(1)
          else __FILE__
      in
        Term.terminal_set_raw () ;
        file
          |> Ciseau.init_editor
          |> Ciseau.loop (next_key (Random.State.make [| 0 ; 1 ; 2 |]) n) ;
        Term.terminal_restore ()
    with
      e ->  Term.terminal_restore () ;
            Printf.printf "\nerror: %s\n" (Printexc.to_string e) ;
            Printexc.print_backtrace stdout

end


let () =
  (* Automatic resize support requires requesting SIFWINCH events from the terminal.
   * No handling is needed as SGIWINCH events interrupt blocking reads on input,
   * which is detected and processed in the input parser. *)
  let handler sig_n = () in
  let sigwinch_code = 28 in
  Sys.set_signal sigwinch_code (Sys.Signal_handle handler) ;
  (*
  Fuzzer.main 5000 ;
  Ciseau.main () ;
   *)
  navigation_test () ;
  close_out logs


(* next TODOs:
 *
 * rendering:
 *  - better management of screen dragging for horizontal scrolling
 *
 *  movements:
 *  - implement easymotion
 *  - once I have a proper tokenizer for the text:
 *    - Number tokenizer which recognizes 0xdeadbeef and 6.667e-11
 *    - string, comments, movement
 *  - Proper tree navigation
 *  - bind brackets and braces delim movement
 *
 *  highlightning:
 *  - static syntax coloring based on tokens
 *
 *  hud: put movement/command history per Fileview and show in user input bar
 *
 *  perfs:
 *  - memory optimization for
 *      DelimMovement
 *  - add put_color_segment to avoid many rect creation
 *
 *  next features:
 *  - finish file navigation
 *  - find
 *    - free input search
 *    - add vim's incsearch feature
 *    - from current token
 *)
