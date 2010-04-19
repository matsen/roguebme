open Quartet
open Quartet_btree

(* 
These are the examples of two trees which are associated to a given cone of the
reduced fan which actually have different distances to the lower tree.

this file could get run through max_quartet_dist.top
*)
let lower = Btree_io.of_newick_str "(((0,1),4),((2,3),5))"
and t = Btree_io.of_newick_str "(0,((((1,2),(3,6)),5),4))"
and t' = Btree_io.of_newick_str "(0,((((1,5),(3,6)),2),4))"
;;
let rt = Btree.del_taxon 6 t
and rt' = Btree.del_taxon 6 t'
;;
let qrt = qset_of_t rt
and qrt' = qset_of_t rt'
and qlower = qset_of_t lower
;;
let dist = qdist lower rt
and dist' = qdist lower rt'
;;
let qd = QSet.diff qlower qrt
and qd' = QSet.diff qlower qrt'
;;

