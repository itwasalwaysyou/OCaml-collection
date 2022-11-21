#lab6
type 'base mutyQueue =
   MutyQueueNode of
     'base *
     'base mutyQueue ref *
     'base mutyQueue ref ;; 


let mutyQueueMake s= 
 let rec h = MutyQueueNode (s, ref h, ref h) 
 in h
;;



let mutyQueueEmpty q = 
 match q with
 | MutyQueueNode(_, left, right)-> 
     match !left with
     | MutyQueueNode(_, left_left, right_right)-> 
         left==left_left
     
let mutyQueueEnqueue q e  = 
 match q with 
 | MutyQueueNode(_, left, right)-> 
     match !left with 
     | MutyQueueNode(_, left_left, left_right)-> 
         let curr_end= !left in
         let new_node=MutyQueueNode(e, ref curr_end, ref q)in 
         left_right := new_node;
         left:= new_node

let mutyQueueDequeue q =
 match q with 
 | MutyQueueNode(_, _, first)-> 
     match !first with 
     | MutyQueueNode(data, head, second)-> 
         match !second with
         | MutyQueueNode(_, to_first, to_third)-> 

             first:= !second;
             to_first:= !head;
             data 


(* Make a QUEUE whose sentinel is the empty string "" and test it. The comments
say what each test should return, and how many points you get (if any) for
successful tests. *)

let queue = mutyQueueMake "" ;;

(* 2 pt. MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>}) *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)

mutyQueueEnqueue queue "A" ;;     (* 1 pt. () *)

mutyQueueEmpty queue ;;           (* 2 pt. false *)

mutyQueueEnqueue queue "B" ;;     (* 1 pt. () *)

mutyQueueEnqueue queue "C" ;;     (* 1 pt. () *)

mutyQueueDequeue queue ;;         (* 5 pt. "A" *)

mutyQueueDequeue queue ;;         (* 5 pt. "B" *)

mutyQueueDequeue queue ;;         (* 5 pt. "C" *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 5 pt. "" *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)


(*
type 'base mutyQueue =
    MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref
val mutyQueueMake : 'a -> 'a mutyQueue = <fun>
val mutyQueueEmpty : 'a mutyQueue -> bool = <fun>
val mutyQueueEnqueue : 'a mutyQueue -> 'a -> unit = <fun>
val mutyQueueDequeue : 'a mutyQueue -> 'a = <fun>
val queue : string mutyQueue =
  MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>})

