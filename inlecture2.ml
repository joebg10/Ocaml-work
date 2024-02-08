let rec print_list_of_tuple list =
  match list with
    [] -> ()
  | (x,y)::t -> print_char x; print_string "," ; print_int y; print_list_of_tuple t

let rec print_list list =
  match list with
    [] -> ()
  | h::t -> print_int h ; print_string " " ; print_list t

let encode list = 
  let rec aux count accum list = 
    match list with
      [] -> accum 
    | [x] -> (x, count + 1)::accum
    | a::b::_ as t-> if a =b then aux (count + 1) accum t
      else aux 0 ((a, count + 1)::accum) t    
  in List.rev (aux 0 [] list) 


let rec insert e sortedList = 
  match sortedList with
    [] -> [e]
  |h::t -> if e < h then e::sortedList else h::(insert e t)


let insertion_sort list: int list =
  let rec aux (sortedList: int list) (unsortedList: int list) = 
    match unsortedList with 
      [] -> sortedList
    | [x] -> [x]
    | h::t -> aux (insert h sortedList) (t)
  in aux [] list

let rec mergesort  list =
  let rec merge sortedList1 sortedList2=
    match (sortedList1, sortedList2) with 
      ([],[]) -> []
    | (_, []) -> sortedList1
    | ([], _) -> sortedList2
    | (h1::t1, h2::t2) -> if h1 < h2 then h1::merge (t1) sortedList2 else
        h2::(merge sortedList1 t2)
  in 
  let split list = 
    let rec aux list left right =
      match list with 
        [] -> (left, right)
      | h::t -> aux t right (h::left) 
    in aux list [] []

  in
  match list with 
    [] -> []
  | [x] -> list
  | _ ->  let (left, right) = split list 
    in merge (mergesort left) (mergesort right)

let slope p1 p2 = 
  match (p1, p2) with
    ((x1,y1), (x2, y2)) -> if x2-.x1 = 0.0 then None else Some ((y2-.y1)/.(x2-.x1))

let print_float_option x = 
  match x with
    None -> print_string "Undefined Slope"
  | Some v -> print_float v

let main =

  print_list (insertion_sort [5; 0; 4; -1])        ;  

  print_newline();
  print_newline();
  print_newline()              

