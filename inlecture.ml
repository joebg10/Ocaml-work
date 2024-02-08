let rec length l =
  match l with
    [] -> 0
  |
    _::t-> 1 + length t

let rec sum l = 
  match l with
    []-> 0
  |
    h :: t -> h + sum t

let rec range a b = 
  if a > b then []
  else
    a::(range (a + 1) b)



let rec take n list =
  match list with
    [] -> []
  |
    h::t -> if n = 0 then [] else h::(take (n - 1) t)

let rec drop n list =
  match list with
    [] -> []
  |
    h::t -> if n = 0 then [] else drop (n-1) t


let rec duplicate l = 
  match l with
    [] -> []
  |
    h:: t -> h::h::duplicate t

let slice i k list =
  let rec take n list =
    match list with
      [] -> []
    |
      h::t -> if n = 0 then [] else h::(take (n - 1) t)
  in
  let rec drop n list =
    match list with
      [] -> []
    |
      h::t -> if n = 0 then [] else drop (n-1) t
  in take ((k-i)+ 1) (drop i list)

let rec factorial x =
  match x with
    0 -> 1
  |
    _ -> x * factorial (x-1)

let rec factorial' x = 
  let rec aux accum x = 
    match x with
      0 -> accum
    |
      _-> aux (accum * x) (x -1)
  in aux 1 x


let rec print_list (list: int list): unit =
  match list with
    [] -> ()
  | 
    e::l -> print_int e ; print_string " " ; print_list l

let rec mystery list =
  match list with
    []->()
  |
    h::t -> mystery t; print_int h; print_string " "

let encode l = 

  let rec aux l accum = 
    match l with 
      [] -> 
    |
      [x] ->
    |
      a::(b::_)
let main = 
  print_int (factorial 8);
  print_newline();
  print_newline();
  print_newline()