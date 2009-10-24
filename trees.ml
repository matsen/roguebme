
let all_trees = 
  List.map 
    (fun (tree_str, tree_name) ->
      (Btree_io.of_newick_str tree_str, tree_name))
  [
  "((0,1),(2,3))",          "p_reduced4";
  "((0,1),((2,3),4))",      "p_reduced5";
  "(((0,1),4),((2,3),5))",  "p_reduced6a";
  "(((0,1),(2,3)),(4,5))",  "p_reduced6b";
  ]
