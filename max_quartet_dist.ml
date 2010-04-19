(* to check if caterpillars really do have the maximum distance to an arbitrary
 * tree*)


(*
# iter_over_pairs (Printf.printf "(%d,%d) ") [1;2;3;4];;
(1,2) (1,3) (1,4) (2,3) (2,4) (3,4)
*)
let rec iter_over_pairs f = function
  | x::l -> 
      List.iter (fun y -> f x y) l;
      iter_over_pairs f l
  | [] -> ()


let min_n_tips = 4
and max_n_tips = 7

let () = 
  if not (!Sys.interactive) then
  begin
  print_endline "n\tcomb\tall";
  for n_tips = min_n_tips to max_n_tips do
    let all = Btree.make_all_unrooted n_tips
    and comb = Btree.comb n_tips
    and max_comb = ref 0
    and max_all = ref 0
    in
    List.iter 
      (fun t ->
        let d = Quartet_btree.qdist t comb in
        if d > !max_comb then max_comb := d)
      all;
    iter_over_pairs
      (fun t t' ->
        let d = Quartet_btree.qdist t t' in
        if d > !max_all then max_all := d)
      all;
    Printf.printf "%d\t%d\t%d\n" n_tips (!max_comb) (!max_all);
  done
end

let x = Btree_io.of_newick_str
