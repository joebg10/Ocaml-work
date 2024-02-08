(****
   Please read `Assignment3.pdf` before starting the implementation.
   The pdf provides more detailed description, motivation, and hints about this assignment.
 ****)

(*************** warm up ***************)

(****  
   Take a filename and a list of strings, and write the strings to the file
   indicated by the filename.

   Remember to close any possible channel before ending.

   @param f_name: the name of the file to write to
   @param content: a list of string to write to file
*)
let rec writetoFile (oc: out_channel) (contents: string list) =
  match contents with
    [] -> ()
  | h::t -> Printf.fprintf oc "%s\n" h ; writetoFile oc t

let write_list_str (f_name: string) (content: string list): unit =
  let oc = open_out f_name
  in let wf = writetoFile oc content
  in let cl = close_out oc
  in ()
(****
   Take a file and reads it line by line and returns a list of
   all the strings of each line in order.

   Remember to close any input channel before ending.

   @param f_name: file name to read from
   @return: each line as one element of the list in reverse order

   Example:
   That is if the file looks like this:
   ```
   Hello
   World
   CS320
   ```
   the returned list should be `["Hello"; "World"; "CS320"]` .
*)
let rec readFile (ic: in_channel) (l: string list) =
  match input_line ic with
  exception End_of_file -> List.rev(l)
  |h -> readFile ic (h::l)

let read_list_str (f_name: string): (string list) =
  let ic = open_in f_name
  in let rd = readFile ic []
  in let cl = close_in ic
  in rd

(*************** Functions over ciltrees ***************)

(* Definition of ciltree *)
type ciltree = L of int list | I of int | T of ciltree * char * ciltree

(* Some examples of biltrees *)
let ex1 = T (T (I 1, 'a' , T (I (-34), 'b', L [-21; 53; 12])), 'c', T (I (-18), 'd' , I 1))
let ex2 = T (T (T (T (I 31, 'h', L [9; 34; -45]), 'e', L [70; 58; -36; 28]), 'l', I 3), 
             'l', T (I 2, 'o', I 49))
let ex3 = T (T (T (L [9; 4; -1; 0; -5], 'c', L [40]), 's', I 1), '3', T (L [420; 69], 
                                                                         '2', I (-3)))



(****
   Count the number of int in the input tree

   "Int" is all the node with constructor `I`

   @param tree: the tree to count in
   @return: the number of int in the input `tree`

   Example:
   count_ints ex1 = 4
   count_ints ex2 = 4
   count_ints ex3 = 2
*)
let rec count_ints (tree: ciltree): int = 
  match tree with
    I x -> 1
  |L _ -> 0
  |T (lst, node, rst) -> count_ints(lst) + count_ints(rst)

(****
   Map the `func` onto all the elements in the list of `tree`

   "list" is all the node that with constructor `L`
   We will apply func on to all the **elements** of the list in constructor `L`

   @param func: the function to apply to the element
   @param tree: input tree
   @return: the new tree after the map appends

   Example:
   map_on_all_lists_elem (fun n -> n * 2) 
      (T (L [1;2;3], 't', L [3;2;1])) = T (L [2;4;6], 't', L [6;4;2])

   map_on_all_lists_elem (fun n -> 0) 
    (T (T (L [1], 'a', I 10), 't', L [3; 2; 1])) = 
    T (T (L [0], 'a', I 10), 't', L [0; 0; 0])

   map_on_all_lists_elem ((+) 1) 
    (T (T (L [1], 'a', I 10), 't', L [3; 2; 1])) = 
    T (T (L [2], 'a', I 10), 't', L [4; 3; 2])
*)
let rec apply func l =
  match l with
    [] -> []
  | h::t -> func h :: apply func t

let rec map_on_all_lists_elem (func: int -> int) (tree: ciltree): ciltree =
  match tree with
    I a -> tree
  | L x -> L (apply func x)
  | T (lst, node, rst) -> T (map_on_all_lists_elem func lst, node, map_on_all_lists_elem func rst)


(****
   shift the entire tree to the right

   Here is the shift

       r                       p
     /   \                   /   \
    p     u   shift_right   x     r
   / \   / \    ======>    / \   / \
   x  T3 T4 T5             T1 T2 T3  u
   / \                               / \
   T1  T2                            T4 T5
   If the tree do not have the structure in the given picture, 
   return the original input tree.

   @param tree: the tree before the shift
   @return: the tree after the shift

   Example:
   shift_right (L [1; 2; 3]) = (L [1; 2; 3])
   shift_right (T (L [1], 't', T (I 1, 'r', T (I 69, 'e', L[14])))) =
    (T (L [1], 't', T (I 1, 'r', T (I 69, 'e', L[14]))))

   (*the minimum tree to shift*)
   shift_right (T( T (I 12, 'b', L [1]), 'c', T (I 12, 'd', L [34; 96]))) =
   T( I 12, 'b', T (L [1], 'c', T (I 12, 'd', L [34; 96])) )
   (*a general tree*)
   shift_right (
    T( 
      T (
        T (I 1, 'r', T (I 69, 'e', L[14])), 
        'b', 
        T (L [34], 'r', T (L [420;69], 'e', I 44))
      ), 
      'c', 
      T (T (I 44, 'm', L []), 'd', L [])
    )
   ) =
   T( 
    T (I 1, 'r', T (I 69, 'e', L[14])), 
    'b', 
    T (
      T (L [34], 'r', T (L [420;69], 'e', I 44)), 
      'c', 
      T (T (I 44, 'm', L []), 'd', L [])
    ) 
   )
*)
let rec shift_right (tree: ciltree): ciltree = 
  match tree with
    I _ -> tree
  | L _ -> tree
  | T (lst, node, rst) -> match lst with
      I a -> tree
    | L x -> tree
    | T (lst1, node1, rst1) -> T ( lst1, node1, T (rst1 ,node, rst))


(****
   Whether `tree` contains the `subtree_to_find`

   Return whether the given `tree` contains the subtree (a node with all of its descendent) 
   exactly equals the input `subtree_to_find`

   @param tree: the tree to find in
   @param subtree_to_find: the subtree that we need to find in `tree`
   @return: whether `tree` contains the `subtree_to_find`

   Example:
   (*a tree always contains itself*)
   tree_contains (L [1, 2, 3]) (L [1,2,3]) = true
   (*the right most subtree*)
   tree_contains (T (L [1], 't', T (I 1, 'r', T (I 69, 'e', L[14]))))
    (T (I 69, 'e', L[14])) = true

   tree_contains (T (L [1], 't', T (I 1, 'r', T (I 69, 'e', L[14]))))
    (I 1) = true

   (*there is no subtree that exactly matches the input*)
   tree_contains (T (L [1], 't', T (I 1, 'r', T (I 69, 'e', L[14]))))
    (T (L [1], 't', I 1)) = false
*)
let rec tree_contains (tree: ciltree) (subtree_to_find: ciltree): bool =
  match tree with
    I _ -> tree = subtree_to_find
  | L _ -> tree = subtree_to_find
  | T (lst, node, rst) -> if tree = subtree_to_find then true else tree_contains lst subtree_to_find || tree_contains rst subtree_to_find

(*************** cil and cilbtree ***************)

(****
   cil is a type that can contain a list of int, a int, or a char
*)
type cil = L of int list | I of int | C of char


(****
   a general tree where each node is a cil
*)
type cil_gtree = Empty | Node of cil * cil_gtree list


(****
   construct a empty cil_gtree
*)
let mk_empty_cil_gtree: cil_gtree = 
  Empty


(****
   Construct a cil_gtree given the value of the root and its children
*)
let mk_cil_gtree (root_val: cil) (children: cil_gtree list) : cil_gtree = 
  Node (root_val, children)


(****
   get the root value of a cil_gtree
*)
let cil_gtree_root (tree: cil_gtree): cil option = 
  match tree with
    Empty -> None
  | Node (rv, chil) -> Some rv


(****
   get the children of a cil_gtree
*)
let cil_gtree_children (tree: cil_gtree): cil_gtree list option = 
  match tree with
    Empty -> None
  | Node (rv, chil) -> Some chil

(****
   converts the input `tree` into a cil_gtree
*)
let rec cil_tree_to_cil_gtree (tree: ciltree): cil_gtree = 
  match tree with 
    I a -> Node (I a, [])
  | L b -> Node (L b, [])
  | T (lst, root, rst) -> Node (C root, [cil_tree_to_cil_gtree lst] @ [cil_tree_to_cil_gtree rst])
(****
   converts the input `tree` into a ciltree
*)
let rec cil_gtree_to_cil_tree (tree: cil_gtree): ciltree option = failwith "unimplemented"





