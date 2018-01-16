let (>>) f g x = g (f x)

let flip f x y = f y x

type size_info =
    Empty           (* an empty iterator *)
  | Exact of int    (* an iterator with exactly n elements, like for an aray iterator *)
  | Bounded of int  (* an iterator with at most n elements *)
  | Finite          (* a finite iterator with unknown size, like for a list iterator *)
  | Unknown         (* an iterator whose size is unknown. May be infinite *)

let size_info_finite x  = Finite
let size_info_unknown x = Unknown
let size_info_empty x   = Empty

(* TODO: use next_info to implement efficient drop and range *)
type next_info = Stop | PullOne | Pull of int | Drop of int | Limit of int

type ('elem , 'cursor) seq =
    End
  | Elem of {
      elem    : 'elem ;
      cursor  : 'cursor ;
    }

(* TODO: change this so that 'first' is just a cursor to the first item ! *)
type ('elem , 'cursor) iter = {
  first : ('elem , 'cursor) seq ;
  next  : 'cursor -> ('elem , 'cursor) seq ;
  size  : 'cursor -> size_info ;
}

let mk_elem e c = Elem {
  elem    = e ;
  cursor  = c ;
}

let mk_iter f n s = {
  first = f ;
  next  = n ;
  size  = s ;
}

let iteri fn { first ; next } =
  let rec loop i =
    function
    | End                     -> ()
    | Elem { elem ; cursor }  -> fn i elem ; loop (i + 1) (next cursor)
  in loop 0 first

let iter fn =
  iteri (fun _ x -> fn x)

let fold fn  zero { first ; next } =
  let rec loop z =
    function
    | End                     -> z
    | Elem { elem ; cursor }  -> loop (fn z elem) (next cursor)
  in loop zero first

let map fn { first ; next ; size } =
  let map_fn =
    function
    | End -> End
    | Elem { elem ; cursor } -> mk_elem (fn elem) cursor
  in mk_iter (map_fn first) (next >> map_fn) size

let filter fn { first ; next ; size } =
  let size' cursor =
    match size cursor with
    | Exact n -> Bounded n
    | other -> other
  in
  let rec loop =
    function
    | End -> End
    | Elem { elem ; cursor } as e ->
        if fn elem then e else cursor |> next |> loop
  in mk_iter (loop first) (next >> loop) size'

let drop n { first ; next ; size } =
 let rec loop n =
   function
   | End -> End
   | e when n = 0 -> e
   | Elem { cursor ; _ } -> loop (n - 1) (next cursor)
 in mk_iter (loop n first) next size

let to_list it =
  it |> fold (flip List.cons) [] |> List.rev

let from_list (ls : 'a list) : ('a, 'a list) iter =
  let next =
    function
    | [] -> End
    | h :: t -> mk_elem h t
  in mk_iter (next ls) next size_info_finite

let to_array_slow it =
  it |> to_list |> Array.of_list

let to_array_fast n default it =
  let a = Array.make n default in
  iteri (Array.set a) it ;
  a

let to_array it =
  match it.first with
  | End -> to_array_slow it (* stupid trick to avoid having to provide a 'a element when there is none *)
  | Elem { elem ; cursor } ->
      match it.size cursor with
      | Exact n -> to_array_fast n elem it
      | Bounded n -> to_array_fast n elem it
      | _ -> to_array_slow it

let from_array_slice start stop a =
  let next i =
    if i < stop then mk_elem a.(i) (i + 1) else End
  in
  let size i = Exact (stop - i)
  in mk_iter (next start) next size

let from_array a =
  from_array_slice 0 (Array.length a) a

let test () =
  let print_iter x =
    iter (fun x -> print_int x ; print_string ", " ) x ; print_newline ()
  in
  let print_list =
    from_list >> print_iter
  in
  let l1 = [1 ; 2 ; 3 ; 5] in
  print_list l1 ;
  l1 |> from_list |> drop 2 |> to_list |> print_list ;
  l1 |> from_list |> filter (fun x -> x < 3) |> to_list |> print_list ;
  l1 |> from_list |> map (fun x -> x * 2) |> to_list |> print_list ;

  print_newline () ;
  let a1 = [| 10 ; 11 ; 12 ; 13 ; 14 |] in
  a1 |> from_array |> print_iter ;
  a1 |> from_array |> drop 2 |> print_iter ;
  a1 |> from_array |> filter (fun x -> x < 12 || x > 13) |> print_iter ;
  a1 |> from_array |> map (fun x -> x / 2) |> print_iter ;
  a1 |> from_array_slice 0 2 |> print_iter ;
  a1 |> from_array_slice 2 5 |> print_iter ;
  ()

  let _ = test ()
