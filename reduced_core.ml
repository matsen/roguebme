(* we construct the points of the "reduced" polytope, i.e. where we take the
 * difference between the upper and the lower weight vector
  *)
open Fam_batteries
open MapsSets


let ppr_intset_list = Ppr.ppr_list IntSetFuns.ppr

let make_trees n_tips = 
    Array.of_list (Btree.make_all_unrooted n_tips)

(* calculate the reduced points for adding one more taxon starting from the
 * given lower tree. return a list of (point, tree_list) where tree_list is a
 * list of trees which have that given point. *)
let points_of_lower_tree lower_tree_name lower_tree = 
  let n_lower_taxa = Btree.n_leaves lower_tree in
  let n_upper_taxa = n_lower_taxa + 1 in
  let splits = Btree_split.splits_of_tree lower_tree in
  let n_splits = List.length splits in
  let ambient_dim = n_splits + n_lower_taxa in

  let make_trees n_tips = 
    Array.of_list (Btree.make_all_unrooted n_tips) in
  let upper_trees = make_trees n_upper_taxa in
  let normed_upper_weights_arr = 
    Bme.normed_bme_uptris_of_trees upper_trees in

  let info_ch = open_out (lower_tree_name^".info") in
  let info_ff = Format.formatter_of_out_channel info_ch in

  Format.fprintf info_ff "lower tree: %a@." Btree.ppr_btree lower_tree;

  (* let split_compl = Array.map (List.map lower_complement) splits *)

  let tree_points = 
    ArrayFuns.map2 
      (fun upper_tree weights ->
        let point = Array.make ambient_dim 0 in
        Format.fprintf info_ff "@.upper tree: %a@." Btree.ppr_btree upper_tree;
        (* for every split in the lower tree, put in the sum of the weights (in
         * the upper tree) for all lower taxon pairs which cross that split in the
         * lower tree *)
        ListFuns.iteri
          (fun split_num split ->
            let left_side = Split.left_side split 
            and right_side = Split.right_side split in
            (* total up the weights across the given split *)
            let total_weight = 
              IntSet.fold
                (fun i ->
                  ( + )
                    (IntSet.fold 
                      (fun j ->
                        ( + ) (Uptri.get_loose weights i j))
                      right_side
                      0))
                left_side
                0 
            in
            Format.fprintf info_ff "split: %a\t@,weight %d@." 
              Split.ppr split total_weight;
            point.(split_num) <- total_weight;)
          splits;
        (* fill in the rest of the coordinates with the weights from the
         * original taxa to the "new" taxon *)
        for i=0 to n_lower_taxa-1 do
          point.(i+n_splits) <- 
            Uptri.get_loose weights i n_lower_taxa
        done;
        Format.fprintf info_ff "%a\n" Ppr.ppr_int_array point;
        (upper_tree, point))
      upper_trees
      normed_upper_weights_arr
    in
    close_out info_ch;
    let grouped_by_points = 
      Base.group_by_f snd (Array.to_list tree_points) in
    Array.of_list 
    (List.map 
      (function
      | (_,point)::_ as l -> 
          (* check to make sure grouping worked right *)
          List.iter 
            (fun (_,other_pt) -> assert(point = other_pt)) 
            l;
          (point, List.map fst l)
      | [] -> assert(false))
      grouped_by_points)


