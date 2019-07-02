(* Debugging flags *)
val kLOG_STATS : bool
val kDEBUG : bool

type colors = {
  operator                  : Term.Color.color_cell ;
  structure                 : Term.Color.color_cell ;
  string                    : Term.Color.color_cell ;
  spacing                   : Term.Color.color_cell ;
  numbers                   : Term.Color.color_cell ;
  default                   : Term.Color.color_cell ;
  cursor_line               : Term.Color.color_cell ;
  current_token             : Term.Color.color_cell ;
  selection                 : Term.Color.color_cell ;
  line_numbers              : Term.Color.color_cell ;
  focus_header              : Term.Color.color_cell ;
  header                    : Term.Color.color_cell ;
  status_normal             : Term.Color.color_cell ;
  status_input              : Term.Color.color_cell ;
  user_input                : Term.Color.color_cell ;
  border                    : Term.Color.color_cell ;
  no_text                   : Term.Color.color_cell ;
  leftright_neighbor        : Term.Color.color_cell ;
  updown_neighbor           : Term.Color.color_cell ;
}

type cfg = {
  colors    : colors ;
  page_size : int;
}

val default : cfg
