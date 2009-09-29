let rec twopow i =
  if i=0 then 1
  else if i > 0 then 2*(twopow (i-1))
  else invalid_arg "twopow: negative input"

let int_uptri_diff = Uptri.apply_pairwise ( - )

let dist_uptri_of_btree tree = 
  (* we assume that the leaves are indexed 0,...,size-1 *)
  let n_leaves = Btree.n_leaves tree in
  let m = Uptri.create n_leaves 0 in
  List.iter (
    fun ((i,j), dist) ->
      Uptri.set_loose m i j dist)
    (Btree.make_internode_distance_list tree);
  m

let bme_uptri_of_dist u = Uptri.map (fun d -> (2. ** (float_of_int (-d)))) u
let bme_uptri_of_tree t = bme_uptri_of_dist (dist_uptri_of_btree t)

(* normed_bme_uptris_of_dist_uptris :
 * find something to multiply them with to make all entries greater than or equal
 * to one *)
let normed_bme_uptris_of_dist_uptris uptris = 
  let max_dist = 
    Array.fold_right (fun uptri -> max (Uptri.fold_left max 0 uptri)) uptris 0
  in
  (* to normalize, multiply all bme vectors by 2^max_dist *)
  Array.map (Uptri.map (fun d -> twopow (max_dist - d))) uptris

let normed_bme_uptris_of_trees trees = 
  normed_bme_uptris_of_dist_uptris (Array.map dist_uptri_of_btree trees)

(* facets :
  * make a list of the facets for the vertex numbered "which"
 *)
let facets bme_vertex_arr which = 
  let facets = ref [] in
  for other = (Array.length bme_vertex_arr)-1 downto 0 do
      if other <> which then
        facets := 
          (int_uptri_diff bme_vertex_arr.(which) bme_vertex_arr.(other))
          ::!facets
  done;
  !facets
 
let make_all_bme_pairs n_taxa = 
  List.map (fun t -> (t, bme_uptri_of_tree t)) 
    (Btree.make_all_unrooted n_taxa)

let float_uptri_dot u1 u2 = 
  assert(Uptri.size u1 = Uptri.size u2);
  Array.fold_left ( +. ) 0.
    (Uptri.to_array (Uptri.apply_pairwise ( *. ) u1 u2))

let run_bme d = 
  List.sort (fun (_,v1) (_,v2) -> compare v1 v2) (
    List.map (fun (t, w) -> t, float_uptri_dot d w) (
      make_all_bme_pairs (Uptri.size d)))

let ppr_bme_pair ff (t,v) =
  Format.fprintf ff "%a: %a" 
    Btree.ppr_btree t 
    Format.pp_print_float v

