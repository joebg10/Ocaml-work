(**curring means that every function only takes in one parameter *)
(*
# List.map;;
- : ('a -> 'b) -> 'a list -> 'b list = <fun>
# let addOne x = x + 1;;                   
val addOne : int -> int = <fun>
# let listofInts = [1;2;3;4;5]
  ;;  
val listofInts : int list = [1; 2; 3; 4; 5]
# List.map addOne listOfInts
  ;;
Error: Unbound value listOfInts
Hint: Did you mean listofInts?
# List.map addOne listofInts;;
- : int list = [2; 3; 4; 5; 6]*)

let rec map f list =
  match list with 
    []-> []
  |h::t -> f h::(map f t)

let rec applyFunctions fs x =
  match fs with
    []-> []
  | h::t -> h x::(applyFunctions t x)

let rec sum1 list =
  match list with
    [] -> 0
  | h :: t -> h + sum1 t   (**returns ints*))

let rec sum list =
  match list with
   []-> None
  |[x]-> Some x
  |h::t -> match sum t with
            None -> None
            |
            Some total -> Some (total + h)  (**returns an int option*))

let rec filter f l=
  match l with 
    []-> []
  |h::t -> if f h then h::filter f t else filter f t

let rec print_list l =
  match l with
    [] -> ();
  |h::t -> print_int h ; print_string " " ; print_list t

let main = 

  print_list (applyFunctions [(fun x -> x + 1 ); (fun y -> y +2)] 7)