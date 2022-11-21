open List;;
open Printf ;;


let rec howMany e l =
  if l = [] then
    0
  else 
   
    let freq = howMany e (List.tl l)  in 
    if List.hd l = e then 
      1+ freq 
    else 
      freq 
        
        
;;



let rec delete e l =
  if l= [] then 
    []
  else
    
  if List.hd l =e then
    delete e (List.tl l)
  else 
    (List.hd l) :: (delete e (List.tl l))
;;




let mean l= 
  if l=[] then 
    0.0
  else 
  
    let rec sum lst =
      if lst= [] then 
        0.0
      else
        let total = (List.hd lst) +. sum(List.tl lst) in
        total
    in
    let res= (sum l)/. float_of_int (List.length l) in
    res
;;


(* Tests for HOW MANY. *)

printf "%i\n" (howMany 1 []) ;;                      (* 2 pt: 0 *)
printf "%i\n" (howMany 1 [1]) ;;                     (* 2 pt: 1 *)
printf "%i\n" (howMany 2 [1; 2; 3]) ;;               (* 2 pt: 1 *)
printf "%i\n" (howMany 5 [2; 4; 6]) ;;               (* 2 pt: 0 *)
printf "%i\n" (howMany "c" ["c"; "b"; "c"; "d"]) ;;  (* 2 pt: 2 *)
printf "%i\n" (howMany "x" ["a"; "b"; "c"; "d"]) ;;  (* 2 pt: 0 *)

(* Tests for DELETE. *)

printThings "%i" (delete 1 []) ;;                     (* 2 pt: [] *)
printThings "%i" (delete 1 [1]) ;;                    (* 2 pt: [] *)
printThings "%i" (delete 1 [1; 2; 3]) ;;              (* 2 pt: [2 ; 3] *)
printThings "%i" (delete 4 [1; 2; 3]) ;;              (* 2 pt: [1 ; 2 ; 3] *)
printThings "%i" (delete 1 [1; 2; 1; 3; 1; 4]) ;;     (* 2 pt: [2 ; 3 ; 4] *)
printThings "%s" (delete "a" ["x"; "a"; "y"; "a"]) ;; (* 2 pt: [x ; y] *)

(* Tests for MEAN. *)

printf "%f\n" (mean [1.0]) ;;                         (* 2 pts: 1.000000 *)
printf "%f\n" (mean [1.0; 2.0]) ;;                    (* 2 pts: 1.500000 *)
printf "%f\n" (mean [1.0; 0.0; -1.0; 1.0]) ;;         (* 2 pts: 0.250000 *)
(*
test result: 

val howMany : 'a -> 'a list -> int = <fun>
val delete : 'a -> 'a list -> 'a list = <fun>
val mean : float list -> float = <fun>
val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit =
  <fun>
  
  
String concatenate :

^ : s^t


Printf module; 

open Prinf;; 
printf "string: %s integer %d float %f done\n"
  "hi"         5      1.23;;
(* output: 
   string: hi integer 5 float 1.230000 done *)  
  
int_of_float 3.14;; -> int= 3
  
let x= ref 520;;
val x: int ref = {contents =520}

array_list = [|10;20;30;40|];;  
list=[10;20;30;40];;  

[1;2;3]@[4;5;6]= [1;2;...;6]

2-tuple -> fst, snd- must be a pair!  (10,10.,"am") -> int*float*string
