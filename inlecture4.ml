type 'a stack = Empty| Cons of 'a * 'a stack


let head l =
  match l with
    Empty -> None
  |
    Cons (h, tail) -> Some h 

let rec sum l =
  match l with
    Empty -> 0
  | Cons (h,tail) -> h + sum tail

let ($) f g = fun x -> f (g x)


let printOption l =
  match l with 
    None -> ()
  | Some value -> print_int value

let test (y: string option list option) =
  match w with
  None -> None
  |Some l -> match l with
              [] -> []
              | Some h::t-> match h with


                                