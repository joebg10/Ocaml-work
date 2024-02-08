type 'a binaryTree = Empty | Node of ('a binaryTree * 'a * 'a binaryTree)

let rec insert x t = 
  match t with
    Empty -> Node(Empty, x, Empty)
  | Node(left, root, right) -> if x < root 
    then Node(insert x left, root, right)
    else Node(left, root, insert x right)

let rec print_binary_Tree t = 
  match t with
    Empty -> ()
  | Node(left, root, right) -> print_int root; print_binary_Tree left; print_binary_Tree right

let createBinaryTreeFromList l  = List.fold_right insert l Empty

let main = 
  print_binary_Tree(createBinaryTreeFromList [3;2;1]);

  print_newline();
  print_newline();
  print_newline()