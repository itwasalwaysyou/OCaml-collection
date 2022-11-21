#lab3
type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst ;;

(* BST INSERT. Return a new BST that's like TREE, but with KEY. We need this to
   make the test cases below. *)

let bstInsert tree key =
  let rec inserting subtree =
    match subtree
    with 
    | BstEmpty -> BstNode(key, BstEmpty, BstEmpty) 
    | BstNode(otherKey, leftSubtree, rightSubtree) ->
        if key < otherKey
        then BstNode(otherKey, inserting leftSubtree, rightSubtree)
        else if key > otherKey
        then BstNode(otherKey, leftSubtree, inserting rightSubtree)
        else subtree
  in inserting tree ;;

(* BST IS IN. Test if KEY is in TREE. It may help with debugging. You need not
   call it in the code you will write. *)


    
let bstIsIn key tree =
  let rec isInning subtree =
    match subtree
    with BstEmpty -> false |
      BstNode(otherKey, leftSubtree, rightSubtree) ->
        if key < otherKey
        then isInning leftSubtree
        else if key > otherKey
        then isInning rightSubtree
        else true
  in isInning tree ;;

exception BadEmptyBst 

let bstDelete tree key = 
  let rec bstMaxKey sub_tree= 
    match sub_tree with 
    | BstEmpty->  raise BadEmptyBst 
    | BstNode (otherKey, leftSubtree, rightSubtree) -> 
        match BstNode (otherKey, leftSubtree, rightSubtree) with 
        | BstEmpty->  raise BadEmptyBst 
        | BstNode (otherKey, BstEmpty, BstEmpty) ->otherKey
        | BstNode (otherKey, leftSubtree, BstEmpty) ->otherKey
        | BstNode (otherKey, leftSubtree, rightSubtree) ->
            bstMaxKey (rightSubtree) 
  in 
  
  let rec deleting subtree key1 =
    match subtree
    with 
    | BstEmpty-> subtree
    | BstNode (otherKey, leftSubtree, rightSubtree) ->
        if key1 < otherKey
        then BstNode(otherKey, deleting leftSubtree key1, rightSubtree)
        else if key1 > otherKey
        then BstNode(otherKey, leftSubtree, deleting rightSubtree key1)
        else 
          match BstNode (otherKey, leftSubtree, rightSubtree) with 
          | BstEmpty-> subtree
          | BstNode (otherKey, BstEmpty, BstEmpty)-> BstEmpty 
          | BstNode (otherKey, BstEmpty, _)-> rightSubtree
          | BstNode (otherKey, _, BstEmpty)->  leftSubtree
          | BstNode (otherKey, _, _) -> 
              let key1= bstMaxKey leftSubtree in
              BstNode(key1, deleting leftSubtree key1, rightSubtree)
            
  in if bstIsIn key tree
  then deleting tree key
  else tree ;;

  




(* Let T be a BST. We'll make it by adding nodes one at a time, so OCaml will
   print many intermediate BST's that we don't care about. Ignore those. *)

let t = BstEmpty        ;;
let t = bstInsert t 100 ;;
let t = bstInsert t 70  ;;


(* 5 points if you get BstEmpty in the end. *)

(*
type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst
val bstInsert : 'a bst -> 'a -> 'a bst = <fun>
val bstIsIn : 'a -> 'a bst -> bool = <fun>
exception BadEmptyBst
val bstDelete : 'a bst -> 'a -> 'a bst = <fun>
val t : 'a bst = BstEmpty
val t : int bst = BstNode (100, BstEmpty, BstEmpty)

