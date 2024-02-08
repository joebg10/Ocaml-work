(************ PROBLEM safe dot-product ************)

(***
   Please, write a function that compute the dot product of two lists in a safe way.
   This function should produce a output no matter what input lists it is provided with.

   Note: dot product only exists for two vector of the same length:
   https://en.wikipedia.org/wiki/Dot_product#Algebraic_definition

   Example
   >>>> safe_dot_product [1;2;3] [2;2;0] = Some 6
   >>>> safe_dot_product [1;2] [2;3] = Some 8
*)

let rec check l1 l2 =
  match (l1, l2) with 
    ([], [])-> true
  |
    (_::t1, _::t2) -> check t1 t2
  |
    _-> false 
let rec safe_dot_product (vect1: int list) (vect2: int list) : int option =
  if check vect1 vect2 then 
    match vect1, vect2 with
      [], [] -> Some 0
    | [x], [y] -> Some (x * y)
    | h1::t1, h2::t2 -> (match safe_dot_product t1 t2 with
        None -> None
      |Some total -> Some (total + h1 * h2))
  else 
    None
(***
   Please, write another version of safe dot product which is tail recursive.
*)

let rec safe_dot_product_tail_rec (vect1: int list) (vect2: int list): int option = 
  failwith "unimplemented"





(************ PROBLEM Bunny's Fibonacci ************)


(***
   Please, write a function that given a positive integer i returns the  number
   in position i in the bunny's Fibonacci Sequence defined as follows.

   The Bunny's Fibonacci Sequence looks like this [0;1;2;3;2;4;4;7;6;11;10...]
   where
   - the start of the sequence is 0;1;2;3
   - a number on an even index i is the sum of the numbers on the previous even
     indices, i.e. the numbers in positions (i-2) and (i-4).
     For example `2` on index 4 is a sum of `0` on index 0 and `2` on index 2
   - a number on an odd index j is the sum of the numbers on the previous odd
     indices   i.e. the numbers in positions (j-2) and (j-4).
     For example, `7` on index 7 is a sum of `3` on index 3 and `4` on index 5

   Example:
   (*The first element (index 0) of bunny's Fibonacci Sequence*)
   >>>> bunny_fib_idx 0 = 0
   (*The 5th element of (index 4) of bunny's Fibonacci Sequence*)
   >>>> bunny_fib_idx 4 = 2
   (*The 9th element of (index 8) of bunny's Fibonacci Sequence*)
   >>>> bunny_fib_idx 8 = 6
*)
let rec bunny_fib_idx (idx: int) (*Assume idx is positive*): int = 
  match idx with 
    0->0
  | 1-> 1
  | 2-> 2
  | 3-> 3
  |h -> bunny_fib_idx(h-2) + bunny_fib_idx(h-4)


(***
   Implement a tail recursive version of bunny_fib_idx
   Your function should run in linear time.
*)
let bunny_fib_idx_tail_rec (idx: int) (*Assume idx is positive*): int = 
  failwith "unimplemented"




    (************ PROBLEM Binary Addition ************)

    (**
       Please, write a function to convert a list of integers 0 and 1 into boolean
       bits (true for 1, and false for 0).

       Your code needs to check whether there is any illegal number (non-0 and 1) in the list
    *)
let to_list opt =
  match opt with
    None -> []
  | Some v -> v


let rec int_list_to_bits (int_lst: int list): bool list option=
  match int_lst with
    [] -> Some []
  | 0::t -> (match int_list_to_bits t with 
              None-> None
            | Some k -> Some (false::k))
  | 1::t -> (match int_list_to_bits t with 
              None-> None
            | Some k -> Some (true::k))
  |x::t -> None



(***
   Please, write a function that takes in input two bits (booleans) and returns a pair where 
   - The first element is a boolean asserting wether there is a bit carrying over.
   - The second element is the resulting bit.
 ***)
let sum_bit (bit1: bool) (bit2: bool): (bool * bool) =
  match bit1, bit2 with 
    false, false -> (false, false)
  | false, true -> (false, true)
  | true, false -> (false, true)
  | true, true -> (true, false)


(***
   Please, write a function that returns the sum of two list of bits (bool). 

   PRECONDITION:
   When adding bits1 and bits2 you can assume that the least significant bit is at the 
   head of the list and the most signifcant bit is at the end of the list. 

   --So for example the number 18 in binary starting from the most signifcant bit to the 
   least significant bit is 10010. 

   However the binary representation of 18 when passed into the sum_bits method  is 
   represented as 0 1 0 0 1 (i.e., starting from the least significant bit to the 
   most significant bit) which is equivalent to [false;true;false;false;true]

   --the number 8 in binary starting from the most signifcant bit to the 
   least significant bit is 1000. 

   However the binary representation of 8 when passed into the sum_bits method is 
   represented as 0 0 0 1 (i.e., starting from the least significant bit to the 
   most significant bit) which is equivalent to [false;false;false;true]

   POSTCONDITION:
   Now the result (18+8=26) of adding these two lists of bools is [false;true;false;true;true]
   i.e., in the output you also again put the least significant bit at the head of the list 
   and the most significant bit at the end of the list. 

   Here are some more examples:
   Example:
   (*1 + 1 = 10*)
   >>>> sum_bits [true] [true] = [false; true]
   (*11 + 1110 = 10001*)
   >>>> sum_bits [true; true] [false; true; true; true] = [true; false; false; false; true]
   (*0 + 110 = 110*)
   >>>> sum_bits [false] [false; true; true] = [false; true; true]

*)
let rec sum_bits (bits1: bool list) (bits2: bool list): bool list = 
  match bits1, bits2 with
    [], [] -> []
  | [h1], [] -> [h1]
  | [], [h1] -> [h1]
  | h1::t1, [] -> h1::sum_bits t1 []
  | [], h1::t1 -> h1::sum_bits [] t1
  | h1::t1, h2::t2 -> match sum_bit h1 h2 with
                        (a1, b1) -> if a1 == false
                                    then a1::(sum_bits t1 t2)
                                    else b1::(sum_bits (h1::t1) t2)




(***
   Please, write a function that converts a list of bits back to a list of 0s and 1s
   (true for 1, and false for 0).
*)
let rec bits_to_int_list (bits: bool list): int list = 
  match bits with
    [] -> []
  | h::t -> match h with 
          false->  0::(bits_to_int_list t)
        | true -> 1::(bits_to_int_list t)


(***
   Please write a function that sums two binary number, 
   where each number is represented by a list of 0s and 1s.
   For example the binary number 10010 will be represented by the list [1;0;0;1;0]

   Your function needs to deal carefully with invalid inputs like the following:
   - the input list is empty
   - the input list contains elements that are not 0 or 1
   - the input list is a non-singleton list starting with 0 (like [0,1,1,1])

   Hint: use the functions that you defined previously, in this exercise, as helper functions. 

   Example:
   >>>> sum_bin [1] [1] = Some [1;0]
   >>>> sum_bin [1;1] [1;1;1;0] = Some [1;0;0;0;1]
   >>>> sum_bin [0] [1;1;0] = Some [1;1;0]
 ***)
let rec sum_bin (b_num1: int list) (b_num2: int list): int list option =
  failwith "unimplemented"


