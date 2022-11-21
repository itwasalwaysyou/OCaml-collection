(*
   PRINT POLY. Pretty-print polynomials from CSci 2041 Project 1.

James Moen
  25 Sep 22
*)

(* PRINTF. Define various predefined printing functions. *)

open Printf ;;

(* POLY. A univariate polynomial, with nonzero integer coefficients and integer
   exponents greater than or equal to zero. *)

type poly =
    PolyEmpty |
    PolyTerm of int * int * poly ;;

(* PRINT POLY. Print POLY so it's allegedly easy to read. You need not know how
   this works. Maybe it will help with debugging. *)

let printPoly poly =

(* PRINTING POLY. Do all the work for PRINT POLY. *)

  let rec printingPoly poly =
    match poly
    with PolyEmpty ->
      () |

      PolyTerm (coef, expo, other) ->
        printf " %c %i x^%i"
          (if coef < 0 then '-' else '+')
          (abs coef) expo ;
        printingPoly other

(* Lost? This is PRINT POLY's body. *)

  in match poly
  with PolyEmpty ->
    printf "0\n" |

    PolyTerm(coef, expo, other) ->
      printf "%i x^%i" coef expo ;
      printingPoly other ;
      printf "\n" ;;



let isPolyOk p=
  let rec searching curr curr_ex=
    match curr with 
    |PolyTerm(coefficient, exponent, nextPolyterm) -> 
        if coefficient =0 then 
          false
        else if exponent <0 then 
          false 
        else if exponent >= curr_ex then 
          false 
        else  
          let curr_ex=exponent in 
          searching nextPolyterm curr_ex 
            
    |PolyEmpty -> true 
      
  in searching p max_int
;;

exception MakePolyError 

let makePoly i= 
  let rec making list=
    match list with 
    |[] ->  PolyEmpty 
    |h::[] -> raise MakePolyError
    |h::t -> PolyTerm (h, List.hd t, making(List.tl t))
  in if isPolyOk(making i)
  then making i
  else raise MakePolyError
;;


(*test part *)







let polyAdd l r=

  let rec adding left right = 
  
    match left with (*first match*)
    |PolyEmpty -> 
        if right= PolyEmpty then 
          PolyEmpty
        else right
    |PolyTerm(coefficientL, exponentL, nextPolytermL)-> 
  
        match right with (*second match*)
       
        |PolyTerm(coefficientR, exponentR, nextPolytermR)-> 
            if exponentL > exponentR then 
              let left= nextPolytermL
              in 
              PolyTerm(coefficientL, exponentL, adding left right)
  
            else if exponentL< exponentR then 
              let right= nextPolytermR
              in 
              PolyTerm(coefficientR, exponentR, adding left right)
            else let left= nextPolytermL in
              let right= nextPolytermR in 
              
              if (coefficientL+coefficientR=0) then 
                adding left right
              else 
                PolyTerm(coefficientL+coefficientR, exponentL, adding left right) 
        |PolyEmpty -> 
            if left= PolyEmpty then 
              PolyEmpty
            else left 
              
  in adding l r 
  
;;


let polyMinus r = 
  let rec minusing curr=
    match curr with (*first match*)
    |PolyEmpty -> 
        PolyEmpty 
    |PolyTerm(coefficient, exponent, next)-> 
        PolyTerm(-coefficient, exponent, minusing next) 
  in minusing r;;




printPoly(polyAdd (makePoly[7;4;1;2;-4;1;-3;0])(makePoly [1;2]) );
printPoly(polyAdd (makePoly[7;4;1;2;-4;1;-3;0])(makePoly []) );

printPoly(polyAdd (makePoly [9;6;3;5;2;4;2;3;-1;2;5;0]) (makePoly[7;4;1;2;-4;1;-3;0]));
printPoly(polyAdd (makePoly[7;4;1;2;-4;1;-3;0])(makePoly [9;6;3;5;2;4;2;3;-1;2;5;0]) );


printPoly(makePoly [3;5;2;4;2;3;-1;2;5;0]);
printPoly(polyMinus (makePoly [3;5;2;4;2;3;-1;2;5;0]));



(*test if isPolyOk works in makePoly exception*)

printPoly(makePoly[3;5;9;4;2;3;-4;1;2;0]);
printPoly(makePoly [0;2]);
printPoly(makePoly [1;-2]);
printPoly(makePoly[1;2;1;3]);
printPoly(makePoly[1;2;1;2]);

(*type poly = PolyEmpty | PolyTerm of int * int * poly
val printPoly : poly -> unit = <fun>
val isPolyOk : poly -> bool = <fun>
exception MakePolyError
val makePoly : int list -> poly = <fun>
val polyAdd : poly -> poly -> poly = <fun>
val polyMinus : poly -> poly = <fun>
7 x^4 + 2 x^2 - 4 x^1 - 3 x^0
7 x^4 + 1 x^2 - 4 x^1 - 3 x^0
9 x^6 + 3 x^5 + 9 x^4 + 2 x^3 - 4 x^1 + 2 x^0
9 x^6 + 3 x^5 + 9 x^4 + 2 x^3 - 4 x^1 + 2 x^0
3 x^5 + 2 x^4 + 2 x^3 - 1 x^2 + 5 x^0
-3 x^5 - 2 x^4 - 2 x^3 + 1 x^2 - 5 x^0
3 x^5 + 9 x^4 + 2 x^3 - 4 x^1 + 2 x^0
Exception: MakePolyError.*)


