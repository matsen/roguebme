(* we construct the points of the "reduced" polytope, i.e. where we take the
 * difference between the upper and the lower weight vector
  *)
open Fam_batteries
open MapsSets


let ppr_intset_list = Ppr.ppr_list IntSetFuns.ppr

let make_trees n_tips = 
    Array.of_list (Btree.make_all_unrooted n_tips)

let to_n_set n = IntSetFuns.of_list (ListFuns.init n (fun i -> i))

let () = if not !Sys.interactive then begin

  List.iter 
  (fun (lower_tree, file_prefix) ->
    let fully_grouped = 
      Reduced_core.points_of_lower_tree file_prefix lower_tree in
    let poly_ch = open_out (file_prefix^".poly") in
    Printf.fprintf poly_ch "# lower tree: %s\n\n" (Btree.to_string lower_tree);
    Printf.fprintf poly_ch "POINTS\n";
    Array.iteri
      (fun cone_num (point, tree_list) ->
        Printf.fprintf 
          poly_ch
          "# cone %d: %s\n%s\n"
          cone_num
          (String.concat " " (List.map Btree.to_string tree_list))
          (String.concat 
            "\t" 
            (List.map string_of_int (1::(Array.to_list point)))))
      fully_grouped;
    close_out poly_ch;

    let tree_ch = open_out (file_prefix^".trees") in
    Printf.fprintf tree_ch "# lower tree: %s\n" (Btree.to_string lower_tree);
    Array.iter
      (fun (_, tree_list) ->
        Printf.fprintf 
          tree_ch
          "%s\n"
          (String.concat " " (List.map Btree.to_string tree_list)))
      fully_grouped;
    close_out tree_ch;)
  [Trees.lower_tree4,  "p_reduced4";
   Trees.lower_tree5,  "p_reduced5";
   Trees.lower_tree6a, "p_reduced6a";
   Trees.lower_tree6b, "p_reduced6b"]
  end
