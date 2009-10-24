
(* implements partial orders over arrays for any type which has the polymorphic
 * compare function. *)

open Fam_batteries

(* the int below is the int one gets from the compare function. *)
type order = Comparable of int | Incomparable

(* counts the number of times f gives true when applied to a 
 # array_count (fun x -> x > 0) [|1;2;3;-3;-2|];;        
- : int = 3
 * *)
let array_count f a =  
  Array.fold_left 
    (fun subtot x -> if f x then subtot+1 else subtot)
    0
    a

(*
 * compare two arrays.
#   compare_array [| 2; 3|] [| 2;-3|];;
- : order = Comparable 1
#   compare_array [|-2; 3|] [| 2;-3|];;
- : order = Incomparable
#   compare_array [|-2;-3|] [|-2; 3|];;
- : order = Comparable (-1)
#   compare_array [|-2;-3|] [|-2;-3|];;
- : order = Comparable 0
*)
let compare_array a1 a2 = 
  let comparison = ArrayFuns.map2 compare a1 a2 in
  let count_cmp f = array_count f comparison in
  (* count the number of greaters and less thans *)
  match (count_cmp (fun x -> x > 0), count_cmp (fun x -> x < 0)) with
  | 0,0 -> Comparable 0
  | 0,_ -> Comparable (-1)
  | _,0 -> Comparable 1
  | _,_ -> Incomparable


