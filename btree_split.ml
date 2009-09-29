(* btree_splits.ml
 * this is the collection of functions which use both btrees and splits. as an
 * extension of btree, i've decided to open it. *)

open Btree
open MapsSets

let splits_of_tree tree = 
  (* aux gets the left_sides of the splits *)
  let rec aux = function
    | Node(left,right) -> 
        let l_splits = aux left
        and r_splits = aux right in
        (IntSet.union (List.hd l_splits) (List.hd r_splits)) ::
          (l_splits @ r_splits)
    | Leaf i -> [ IntSet.singleton i ]
  in
  let n_leaves = Btree.n_leaves tree in
  let get_splits t = 
    List.map (Split.of_data n_leaves) (aux t) in
  match tree with 
  | Node(left,right) -> 
    List.stable_sort 
      Split.compare
    (* tl below so that we don't get root split 2x *)
      ((get_splits left) @ (List.tl (get_splits right)))
  | Leaf _ -> assert(false)

let internal_splits_of_tree tree = 
  List.filter Split.is_internal (splits_of_tree tree)
