let kLOG_STATS    = true
let kDEBUG        = false

open Term.Color

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

let darkgray = Gray 2

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
      bg    = Gray 4 ;
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
