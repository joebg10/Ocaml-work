let sat_p f = 
  (*let* x = item _p in 
    if p x then return_p x else 
    empty *)
  item_p >>= (fun x-> if f x then return_p x else empty_p)

let f x = [([x], x)]

let rec foo (g, n) = 
  match n with
  | 0 -> [(g 1, g 0)]
  | n -> (g n, g (n - 1)) :: foo (g, n - 1)

let res = foo(f, 2)

type fruit = Apple | Pear | Grape | Banana
type veggie = Carrot | Cucumber | Cabbage
type salad = fruit * veggie list

let rec foo a c = 
  match a with
  | [] -> c
  | s::ss -> let c1, c2 = c in
    (match s with
     | Apple, _ -> foo ss (c1+1,c2)
     | _ , Cabbage -> foo ss (c1, c2+1) 
     | _ -> foo ss c)



type symbol = A | B | C 
type op = Un of (symbol -> symbol) | Bi of (symbol -> symbol -> symbol) | S of symbol | Out of (symbol list -> symbol -> int )

let apply1 (operation: op) (ls: symbol list) = 
  match operation with
  | Un a -> List.map a ls
  | Bi b ->
    ( match ls with
      | h1::h2::tl -> b h1 h2 :: tl | _ -> [A])

let rec apply2 (two_op : op * op ) (ls: symbol list) = 
  match two_op with
  | S s , Out o -> o ls s
  | Out o, S s -> apply2 (S s, Out o) ls
  | S s , Un a -> let _ = a s in 1