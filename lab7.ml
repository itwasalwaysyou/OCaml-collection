(*
   TESTS7. Tests for CSci 2041 Lab 7.

   James Moen
   23 Oct 22

*)

(* TIME. Print TITLE and the time needed to compute FUNC (). Return the value
   of FUNC (). FUNC will typically be an anonymous function. Students don't
   have to write this. *)

   let rec c n k=
   match k with 
   |0 -> 1
   |_ -> 
       if n= 0 then 
         0
       else
         c (n-1) k + c (n-1) (k-1) 
   
   
 ;;
 
 let memyC n k = 
   let t = Hashtbl.create  ~random: false 1000 in
   let rec memorying n k = 
     match k with 
     |0 -> 1
     |_ -> 
         if n= 0 then 
           0
         else 
           let r= Hashtbl.find_opt t (n, k) in
           match r with 
           | Some value -> value
           | None ->
               let r = memorying (n - 1)(k) + memorying (n - 1)(k-1) in
               Hashtbl.add t (n,k) r ;
               r 
   in memorying n k 
 ;; 
 
 
 
 let time title func =
   let t0 = Sys.time ()
   in let result = func ()
   in let t1 = Sys.time ()
   in Printf.printf "%s %f seconds\n" title (t1 -. t0) ;
   result ;;
 
 (*
 
   YOUR CODE GOES HERE!!!
 
 *)
 
 (* Tests. The first few tests make sure C works for base cases and one or two
    simple cases. All these tests should have VERY small execution times. Times
    for your code MAY BE DIFFERENT. *)
 
 time "c test1" (fun () -> c 0 0) ;;
 
 (* c test1 0.000001 seconds
    - : int = 1
    1 point. *)
 
 time "c test2" (fun () -> c 0 1) ;;
 
 (* c test2 0.000001 seconds
    - : int = 0
    1 point. *)
 
 time "c test3" (fun () -> c 1 0) ;;
 
 (* c test3 0.000001 seconds
    - : int = 1
    1 point. *)
 
 time "c test4" (fun () -> c 1 1) ;;
 
 (* c test4 0.000001 seconds
    - : int = 1
    1 point. *)
 
 time "c test5" (fun () -> c 8 4) ;;
 
 (* c test5 0.000008 seconds
    - : int = 70
    4 points. *)
 
 time "c test6" (fun () -> c 5 2) ;;
 
 (* c test6 0.000002 seconds
    - : int = 10
    4 points. *)
 
 (* This test may have a run time measured in tens of seconds. Be patient! You
    get extra points for waiting. *)
 
 time "c test7" (fun () -> c 40 10) ;;
 
 (* c test7 40.424319 seconds
    - : int = 847660528
    8 points. *)
 
 (* These tests make sure MEMY C works for base cases and simple cases too. They
    should also have very small execution times, but maybe a little larger than
    those of C. It's because MEMY C does work to maintain its hash table. Times
    for your code MAY BE DIFFERENT. *)
 
 time "memyC test1" (fun () -> memyC 0 0) ;;
 
 (* memyC test1 0.000003 seconds
    - : int = 1
    2 points. *)
 
 time "memyC test2" (fun () -> memyC 0 1) ;;
 
 (* memyC test2 0.000002 seconds
    - : int = 0
    2 points. *)
 
 time "memyC test3" (fun () -> memyC 1 0) ;;
 
 (* memyC test3 0.000002 seconds
    - : int = 1
    2 points. *)
 
 time "memyC test4" (fun () -> memyC 1 1) ;;
 
 (* memyC test4 0.000003 seconds
    - : int = 1
    2 points. *)
 
 time "memyC test5" (fun () -> memyC 8 4) ;;
 
 (* memyC test5 0.000012 seconds
    - : int = 70
    6 points. *)
 
 time "memyC test6" (fun () -> memyC 5 2) ;;
 
 (* memyC test6 0.000002 seconds
    - : int = 10
    6 points. *)
 
 (* This test should have a very short run time: MUCH less than one second! *)
 
 time "memyC test7" (fun () -> memyC 40 10) ;;
 
 (* memyC test7 0.000095 seconds
    - : int = 847660528
    10 points. *)
 

(*

val c : int -> int -> int = <fun>
val memyC : int -> int -> int = <fun>
val time : string -> (unit -> 'a) -> 'a = <fun>
c test1 0.000006 seconds
- : int = 1
c test2 0.000005 seconds
- : int = 0
c test3 0.000004 seconds
- : int = 1
c test4 0.000008 seconds
- : int = 1
c test5 0.000032 seconds
- : int = 70
c test6 0.000008 seconds
- : int = 10
c test7 37.209552 seconds
- : int = 847660528
memyC test1 0.000007 seconds
- : int = 1
memyC test2 0.000016 seconds
- : int = 0
memyC test3 0.000004 seconds
- : int = 1
memyC test4 0.000005 seconds
- : int = 1
memyC test5 0.000050 seconds
- : int = 70
memyC test6 0.000058 seconds
- : int = 10
memyC test7 0.000465 seconds
- : int = 847660528
*)