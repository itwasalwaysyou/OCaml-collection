
let num=fst;;
let den= snd;;

let rat n d = 
  let rec gcd i j = 
    if i <> 0 
    then if j > i 
      then gcd i (j - i) 
      else gcd (i - j) j 
    else j 
  in 
  let res = (n/(gcd n d),d/(gcd n d) )in 
  res
;; 


let ratAdd a b= 
  let res= rat (num a * den b + num b*den a) (den a * den b) in 
  res 
;;

let ratMul a b=
  let res= rat (num a * den b) (den a * num b) in
  res 
;;


let ratDiv a b=
  let res = rat (num a *den b) (den a * num b) in
  res 
;;

let ratGt a b =
  if (num a * den b) > (den a * num b) 
  then true
  else
    false
;;




let rec euler()=
  let c = 1 in
  let s = rat 0 1 in
  let t = rat 1 1 in
  
  let rec eulerHelper s t c=
    
    if (ratGt t (rat 1 100000)) then 
      let s = ratAdd s t in
      let t = ratDiv t (rat c 1) in
      let c = c+ 1 in 
      eulerHelper s t c 
    else  
      s
  in
  eulerHelper s t c 
;;





let ratPrint (n, d) =
  Printf.printf "%i / %i\n" n d ;;

(* BOOL PRINT. Print a BOOL B. You don't have to know how this works either. *)

let boolPrint b =
  Printf.printf "%b\n" b ;;

(* Test the rational arithmetic functions. *)

ratPrint (rat 1 2) ;;                                       (* 2 pts: 1 / 2 *)

ratPrint (rat 10 20) ;;                                     (* 2 pts: 1 / 2 *)

ratPrint (ratAdd (rat 1 2) (rat 1 2)) ;;                    (* 2 pts: 1 / 1 *)

ratPrint (ratAdd (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: 5 / 6 *) 
  
ratPrint (ratDiv (rat 1 2) (rat 10 2)) ;;                   (* 2 pts: 1 / 10 *)

ratPrint (ratDiv (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: 3 / 2 *)
                                                            
boolPrint (ratGt (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: true *)

boolPrint (ratGt (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: false *)

(* The big finish. Compute E. *)

ratPrint (euler ()) ;;                             (* 20 pts: 109601 / 40320 *)
                                   
(*  
val num : 'a * 'b -> 'a = <fun>
val den : 'a * 'b -> 'b = <fun>
val rat : int -> int -> int * int = <fun>
val ratAdd : int * int -> int * int -> int * int = <fun>
val ratMul : int * int -> int * int -> int * int = <fun>
val ratDiv : int * int -> int * int -> int * int = <fun>
val ratGt : int * int -> int * int -> bool = <fun>
val euler : unit -> int * int = <fun>
val ratPrint : int * int -> unit = <fun>
val boolPrint : bool -> unit = <fun>
                     
                                                            
                                                 
