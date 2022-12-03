(* 
  CSci 2041 Lab Assignment 10 TESTS.

    James Moen
    13 Nov 22

  40 points.

  Note that arguments are instances of THING, but Boolean valued functions
  return OCaml Booleans. This was simpler than having them return (SYMBOL "t")
  and NIL.
*)

(* THING. Abbreviated type of a Lisp object. *)

type thing = 
    Cons of thing * thing |
    Nil | 
    Number of int | 
    Symbol of string ;;

(*

  YOUR CODE GOES HERE !!!

*)

(* 1. Tests for EVERY. ----------------------------------------------------- *)

(* IS CONS. This is equivalent to Lisp's function of the same name. *)

let isCons thing =
  match thing
  with Cons (_, _) ->
    true |
    _ ->
      false ;;

(* IS ZERO. This is equivalent to Lisp's (λ (n) (= n 0)). *)

let isZero thing =
  match thing
  with Number what ->
    what = 0 |
    _ ->
      false ;;


exception BadMatch ;;

let rec every expr data = 
  
  
  match data with 
  | Nil -> true
  | Cons(data, next) ->
      let curr= expr data in
      if curr=false then
        false
      else 
        every expr next 
  |_ -> raise BadMatch

;;
  

let substitute elements old update=
  
  let rec substituting list= 
    match list with 
    |Nil -> Nil
    |Cons(data, next)-> 
        begin 
          match data with
          |Symbol(string) -> 
              if data= old  then 
                Cons(update, substituting next)
              else 
                Cons(data,substituting next)
          |_ -> Cons (data, substituting next)
        end
    |_ -> raise BadMatch

  
  in substituting elements
  
;;
  

let rec questyEqual left right = 
  
  let movingPointer curr_left next_left right =
    match right with 
    | Cons(curr_right, next_right)-> 
        
        begin 
          if curr_left = Symbol "?" then 
            questyEqual next_left next_right 
            
          else if curr_right <> curr_left then
            false 
          else 
            questyEqual next_left next_right

              
        end 
    |_ -> false
      
  in
  
  let rec basicMatching left right= 
    match left with 
    | Number(int)-> 
        left = right
    | Symbol(string) ->
        if string = "?" then 
          true 
        else left= right
    | Cons(curr_left, next_left) -> 
        if next_left= Nil then 
          match right with 
          |Cons(curr_right,Nil)-> 
              basicMatching curr_left curr_right
          |_ -> false
        else 
          movingPointer curr_left next_left right 
    |_ -> raise BadMatch
            
  in basicMatching left right
    
    
;;

(* (every (λ (n) (= n 0)) nil) returns true. 2 pts. *)

every isZero Nil ;;

(* (every (λ (n) (= n 0)) (quote (0)) returns true. 2 pts. *)

every
  isZero
  (Cons (Number 0, Nil)) ;;

(* (every (λ (n) (= n 0)) (quote (0 0)) returns true. 2 pts. *)

every
  isZero
  (Cons (Number 0, (Cons (Number 0, Nil)))) ;;

(* (every (λ (n) (= n 0)) (quote (0 1))) returns false. 2 pts. *)

every
  isZero
  (Cons (Number 0, (Cons (Number 1, Nil)))) ;;

(* (every is-cons nil) returns true. 2 pts. *)

every isCons Nil ;;

(* (every is-cons (quote ((A) (B)))) returns true. 2 pts. *)

every
  isCons
  (Cons
     ((Cons
         (Symbol "A",
          Nil)),
      (Cons
         ((Cons
             (Symbol "B",
              Nil)),
          Nil)))) ;;

(* (every is-cons (quote (A (B)))) returns false. 2 pts. *)




every
  isCons
  (Cons
     (Symbol "A",
      (Cons
         ((Cons
             (Symbol "B",
              Nil)),
          Nil)))) ;;

(* 2. Tests for SUBSTITUTE. ------------------------------------------------ *)

(* (subsitute nil (quote A) (quote B)) returns Nil. 2 pts. *)

substitute
  Nil
  (Symbol "A")
  (Symbol "B") ;;

(* (substitute (quote (A B A)) (quote A) (quote B))
   returns
   Cons (Symbol "B", Cons (Symbol "B", Cons (Symbol "B", Nil))). 2 pts. *)

substitute
  (Cons (Symbol "A", (Cons (Symbol "B", (Cons (Symbol "A", Nil))))))
  (Symbol "A")
  (Symbol "B") ;;

(* (substitute (quote (X Y Z)) (quote A) (quote B))
   returns
   Cons (Symbol "X", Cons (Symbol "Y", Cons (Symbol "Z", Nil))). 2 pts. *)

substitute
  (Cons (Symbol "X", (Cons (Symbol "Y", (Cons (Symbol "Z", Nil))))))
  (Symbol "A")
  (Symbol "B") ;;

(* (substitute (quote (A (B))) (quote B) (quote A))
   returns
   Cons (Symbol "A", Cons (Cons (Symbol "B", Nil), Nil)). 2 pts. *)

substitute
  (Cons (Symbol "A", (Cons ((Cons (Symbol "B", Nil)), Nil))))
  (Symbol "B")
  (Symbol "A") ;;

(* 3. Tests for QUESTY-EQUAL. ---------------------------------------------- *)

(* (questy-equal (quote 1) (quote 1)) returns true. 2 pts. *)

questyEqual (Number 1) (Number 1) ;;

(* (questy-equal (quote A) (quote 1)) returns false. 2 pts. *)

questyEqual (Symbol "A") (Number 1) ;;

(* (questy-equal (quote A) (quote A)) returns true. 2 pts. *)

questyEqual (Symbol "A") (Symbol "A") ;;

(* (questy-equal (quote ?) (quote X)) returns true. 2 pts. *)

questyEqual (Symbol "?") (Symbol "X") ;;

(* (questy-equal (quote (A B C)) (quote (A X Y))) returns false. 2 pts. *)

questyEqual
  (Cons (Symbol "A", (Cons (Symbol "B", (Cons (Symbol "C", Nil))))))
  (Cons (Symbol "A", (Cons (Symbol "X", (Cons (Symbol "Y", Nil)))))) ;;

(* (questy-equal (quote (A ? ?)) (quote (A X Y))) returns true. 2 pts. *)

questyEqual
  (Cons (Symbol "A", (Cons (Symbol "?", (Cons (Symbol "?", Nil)))))) 
  (Cons (Symbol "A", (Cons (Symbol "X", (Cons (Symbol "Y", Nil)))))) ;;

(* (questy-equal (quote (A (B 1) (C 2 3))) (quote (A (B 1) (C 2 3))))
   returns true. 2 pts. *)

questyEqual
  (Cons
     (Symbol "A",
      (Cons
         ((Cons
             (Symbol "B",
              (Cons
                 (Number 1,
                  Nil)))),
          (Cons
             ((Cons
                 (Symbol "C",
                  (Cons
                     (Number 2,
                      (Cons
                         (Number 3,
                          Nil)))))),
              Nil))))))
  (Cons
     (Symbol "A",
      (Cons
         ((Cons
             (Symbol "B",
              (Cons
                 (Number 1,
                  Nil)))),
          (Cons
             ((Cons
                 (Symbol "C",
                  (Cons
                     (Number 2,
                      (Cons
                         (Number 3,
                          Nil)))))),
              Nil)))))) ;;

(* (questy-equal (quote (A ? (C 2 ?))) (quote (A (B 1) (C 2 3))))
   returns true. 4 pts. *)

questyEqual
  (Cons
     (Symbol "A",
      (Cons
         (Symbol "?",
          (Cons
             ((Cons
                 (Symbol "C",
                  (Cons
                     (Number 2,
                      (Cons
                         (Symbol "?",
                          Nil)))))),
              Nil))))))
  (Cons
     (Symbol "A",
      (Cons
         ((Cons
             (Symbol "B",
              (Cons
                 (Number 1,
                  Nil)))),
          (Cons
             ((Cons
                 (Symbol "C",
                  (Cons
                     (Number 2,
                      (Cons
                         (Number 3,
                          Nil)))))),
              Nil)))))) ;;

(*
type thing = Cons of thing * thing | Nil | Number of int | Symbol of string
val isCons : thing -> bool = <fun>
val isZero : thing -> bool = <fun>
exception BadMatch
val every : (thing -> bool) -> thing -> bool = <fun>
val substitute : thing -> thing -> thing -> thing = <fun>
val questyEqual : thing -> thing -> bool = <fun>
- : bool = true
- : bool = true
- : bool = true
- : bool = false
- : bool = true
- : bool = true
- : bool = false
- : thing = Nil
- : thing = Cons (Symbol "B", Cons (Symbol "B", Cons (Symbol "B", Nil)))
- : thing = Cons (Symbol "X", Cons (Symbol "Y", Cons (Symbol "Z", Nil)))
- : thing = Cons (Symbol "A", Cons (Cons (Symbol "B", Nil), Nil))
- : bool = true
- : bool = false
- : bool = true
- : bool = true
- : bool = false
- : bool = true
- : bool = true
- : bool = true
*)