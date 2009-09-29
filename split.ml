open Fam_batteries
open MapsSets

type split = 
  { size : int;
  left_side : IntSet.t}

let size s = s.size
let left_side s = s.left_side
let right_side s = 
  List.fold_left 
    (fun cmpl i ->
      if IntSet.mem i s.left_side then cmpl else IntSet.add i cmpl)
    IntSet.empty
    (ListFuns.range s.size)

let smallest_side s = 
  let left_cardinal = IntSet.cardinal s.left_side in
  if left_cardinal <= s.size - left_cardinal then left_side s
  else right_side s
  
let size_of_smallest_side s = IntSet.cardinal (smallest_side s)

let check_sizes_equal s1 s2 = 
  if s1.size <> s2.size then
    invalid_arg "split sizes not equal"

let equal s1 s2 = 
  s1.size = s2.size && IntSet.equal s1.left_side s2.left_side

(* make compare such that sorting first sorts in terms of decreasing
 * size of smallest side then increasing in terms of smallest side content.
 * the logic is that in orderings we want the most important splits first.
  *)
let compare s1 s2 = 
  check_sizes_equal s1 s2; (* don't allow cmp of diff sized splits *)
  (* first decreasing in terms of split size *)
  let sm1 = smallest_side s1
  and sm2 = smallest_side s2
  in
  let size_d = (IntSet.cardinal sm2) - (IntSet.cardinal sm1) in
  if size_d <> 0 then size_d
  (* else increasing in terms of min elt *)
  else IntSet.compare sm1 sm2

let complement s = 
  { s with left_side = right_side s }

  (* these have a different format than the usual pprs *)
let ppr_int_list = Ppr.ppr_gen_list_inners "," Format.pp_print_int
let ppr_int_set ff s = ppr_int_list ff (IntSet.elements s)

let ppr ff s = 
  Format.fprintf ff "%a|@,%a" 
  ppr_int_set s.left_side 
  ppr_int_set (complement s).left_side

let print s = ppr Format.std_formatter s

let set_contains_zero set = IntSet.mem 0 set 

let of_data size left_side = 
  if left_side <> IntSet.empty &&
    size < 1 + IntSet.max_elt left_side then
    invalid_arg "of_data: size too small";
  let our_split = {size = size; left_side = left_side} in
  if set_contains_zero our_split.left_side then our_split
  else complement our_split

let of_size_and_list size l = of_data size (IntSetFuns.of_list l)

let trivial size = of_size_and_list size (ListFuns.range size)

let is_trivial s = 
  0 = size_of_smallest_side s

(* isn't a pendant or trivial split *)
let is_internal s = 
  let soss = size_of_smallest_side s in
  soss <> 0 && soss <> 1

(*
 * split compatibility.
 * let our two splits be A|B and C|D.
 * let U = union, ^ = intersection, and < = subset.
 * first note that because left_sides contain zero by def, 
 * we have A ^ B <> 0.
 * note
 * A = (A ^ C) U (A ^ D)  ->  A ^ D = 0 <=> C contains A
 * C = (C ^ A) U (C ^ B)  ->  B ^ C = 0 <=> A contains C
 * B = (B ^ C) U (B ^ D)  ->  B ^ D = 0 <=> C contains B
 * and that we can't have C containing B.
 * therefore compatibility is the same as A < C, C < A, or B < C.
 *)
let compatible s1 s2 = 
  check_sizes_equal s1 s2;
  (IntSet.subset s1.left_side s2.left_side) || (* A < C *)
  (IntSet.subset s2.left_side s1.left_side) || (* C < A *)
  (IntSet.subset (complement s2).left_side s1.left_side) (* B < C *)

(* restrict the split to a smaller set *)
let restrict new_size s = 
  if new_size >= size s then s
  else
    { size = new_size;
    left_side = IntSet.filter (fun x -> x < new_size) s.left_side}

let restricted_compatible s1 s2 = 
  let size = min (size s1) (size s2) in
  compatible (restrict size s1) (restrict size s2)

let pairwiseify f sl1 sl2 = 
  try
    List.iter
      (fun s1 -> 
        List.iter 
          (fun s2 -> if not (f s1 s2) then raise Exit)
          sl2)
      sl1;
    true
  with
  | Exit -> false

let pairwise_compatible = pairwiseify compatible
let pairwise_restricted_compatible = pairwiseify restricted_compatible

(*
let a = of_size_and_list 5 [1;2]
let b = of_size_and_list 5 [3;4]
let c = of_size_and_list 5 [2;3]
let d = of_size_and_list 5 []
 *
# (a,b,c,d);;
- : split * split * split * split =
(0,3,4|1,2, 0,1,2|3,4, 0,1,4|2,3, 0,1,2,3,4|)
# compatible a b;;
- : bool = true
# compatible a c;;
- : bool = false
# compatible a d;;
- : bool = true
# compatible b c;;
- : bool = false
# compatible b d;;
- : bool = true
*)
