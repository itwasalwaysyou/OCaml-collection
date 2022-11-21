open Printf ;;


type thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;


exception ExtraError

let printThing thing =
  
  
  
  
  let rec printingThing in_One_Line =
  

          
    let rec printingThings things= 
      match things with
      | Cons (data, next) ->
          printingThing data; 
          if next !=Nil then
            printf " "; 
          printingThings next
      | Nil -> ()
      | _ -> raise ExtraError
    in
    
    match in_One_Line with
    | Closure(a,b,c) -> printf "[Closure]"
    | Number(num)-> printf "%i" num    
    | Nil -> printf "[nil]"
    | Primitive (thing) -> printf "[Primitive]"
    | Symbol(string)-> printf "%s" string
    | Cons (data, next)->
        printf "(";
        printingThings in_One_Line 
        ;printf ")"
          
                
  
  in printingThing thing; printf "\n"
;;





printThing Nil ;;                                             (* 2 pts. nil *)

printThing (Number 7734) ;;                                  (* 2 pts. 7734 *)

printThing (Symbol "lobyms") ;;                            (* 2 pts. lobyms *)

printThing (Closure (Nil, Nil, [])) ;;                  (* 2 pts. [Closure] *)

printThing (Primitive (fun _ _ -> Nil)) ;;            (* 2 pts. [Primitive] *)

(* More complex tests involving lists. *)

printThing                                                    (* 2 pts. (a) *)
  (Cons (Symbol "a", Nil)) ;;

printThing                                                  (* 2 pts. (a b) *)
  (Cons (Symbol "a", Cons (Symbol "b", Nil))) ;;

printThing                                                (* 2 pts. (a b c) *)
  (Cons (Symbol "a",
         Cons (Symbol "b",
               Cons (Symbol "c", Nil)))) ;;

printThing                                              (* 2 pts. ((a) b c) *)
  (Cons (
      Cons (Symbol "a", Nil),
      Cons (Symbol "b",
            Cons (Symbol "c", Nil)))) ;;

printThing                                              (* 2 pts. ((a b) c) *)
  (Cons (
      Cons (Symbol "a",
            Cons (Symbol "b", Nil)),
      Cons (Symbol "c", Nil))) ;;

printThing                                              (* 2 pts. (a (b c)) *)
  (Cons (Symbol "a",
         Cons (
           Cons(Symbol "b", Cons (Symbol "c", Nil)),
           Nil))) ;;

printThing                                              (* 2 pts. ((a b c)) *)
  (Cons (
      Cons (Symbol "a",
            Cons (Symbol "b",
                  Cons (Symbol "c", Nil))),
      Nil)) ;;

(*
type thing =
    Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list
exception ExtraError
val printThing : thing -> unit = <fun>
[nil]
- : unit = ()
7734
- : unit = ()
lobyms
- : unit = ()
[Closure]
- : unit = ()
[Primitive]
- : unit = ()
(a)
- : unit = ()
(a b)
- : unit = ()
(a b c)
- : unit = ()
((a) b c)
- : unit = ()
((a b) c)
- : unit = ()
(a (b c))
- : unit = ()
((a b c))
- : unit = ()   
*)
