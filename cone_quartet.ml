(*
 * we run this after simulate_cones.ml -- see TOC.txt
 * cone realizations = the collection of trees which have a given reduced cone
 *)

open MapsSets
open Fam_batteries

exception Not_same

let parse_args () =
  let files  = ref [] in
  let usage = "cone_quartet prefix\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [ ] in
  Arg.parse args anon_arg usage;
  List.rev !files

let assert_same_and_hd = function
  | [] -> assert(false)
  | x::_ as l -> 
      if not (ListFuns.all_same l) then begin
        Ppr.print_int_list l;
        raise Not_same;
      end;
      x

let get_trees props = 
  (Btree_io.of_newick_str 
      (List.hd (StringMap.find "LOWER_TREE" props)),
   Btree_io.parse_tree_list_list
    (StringMap.find "VERT_TREES" props ))

let process_simulation fname = 
  let props = Polymake.get_properties fname in
  let (lower_tree, tree_list_list) = get_trees props in
  let lower_quartets = Quartet_btree.qset_of_t lower_tree in
  let rogue_taxon_id = 1 + Btree.max_ind lower_tree in
  assert(Btree.n_leaves lower_tree = rogue_taxon_id);
  (* for each of the cone realizations, a collection of their quartets when
   * restricted to something other than the rogue taxon (we assume that's the
   * highest numbered taxon) *)
  let restr_qset_list_arr = 
    Array.map 
      (List.map 
        (fun t -> Quartet_btree.qset_of_t 
                    (Btree.del_taxon rogue_taxon_id t)))
      (Array.of_list tree_list_list)
  in
  let raw_strs = StringMap.find "RAW_SIM_VOL" props in
  let sim_results = 
    Array.map float_of_string (Array.of_list raw_strs) in
  (* make sure that the number of upper trees is equal to the number of cones *)
  assert(Array.length restr_qset_list_arr = Array.length sim_results);
  (* for each cone, this is the quartet distance of the lower tree to the trees
   * associated with that cone (it checks that they are the same for all trees
   * associated with the cone (called the realizations as above) *)
  let qdist_arr =
    Array.mapi
      (fun i cone_realizations ->
        try
          assert_same_and_hd
            (List.map (* map across different cone realizations *)
              (fun qs ->
                Quartet.qset_half_symdiff lower_quartets qs)
              cone_realizations)
        with
        | Not_same ->
            Btree.print lower_tree;
            List.iter Btree.print (List.nth tree_list_list i);
            assert(false)
        )
      restr_qset_list_arr
  in
  let collected_results = 
    Array.make 
      (1 + Array.fold_left max 0 qdist_arr)
      0.
  in
  (* total up the fraction of hits for cones which have each number of
   * compatible splits *)
  for i=0 to (Array.length qdist_arr)-1 do
    let qdist = qdist_arr.(i) in
    collected_results.(qdist) <-
      collected_results.(qdist) +. sim_results.(i)
  done;
  (* write results *)
  let ch_out = 
    open_out ((Filename.chop_extension fname)^".sim_qtet") in
  List.iter
    (fun s -> Printf.fprintf ch_out "# %s\n" s)
    (StringMap.find "COMMENTS" props);
  Array.iteri 
    (fun n_compat frac -> 
      Printf.fprintf ch_out "%d\t%g\n" n_compat frac)
    collected_results;
  close_out ch_out


let () =
  if not !Sys.interactive then begin
    let files = parse_args () in
    List.iter process_simulation files;
  end

