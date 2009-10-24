(*
 * give simulate_cones a prefix and it will read the poly.out and .trees files,
 * then simulates using the exponential distribution of branch lengths on those
 * cones. it makes a sim_raw file.
 *)
open MapsSets
open Fam_batteries

let n_draws_exp = ref 3 
let seed = ref 1

let parse_args () =
  let files  = ref [] in
  let n_draws_exp_opt = "-e", Arg.Set_int n_draws_exp,
  "The base ten exponent for the number of draws used."
  and seed_opt = "-s", Arg.Set_int seed,
  "Specify a seed for MT19937 random number generation."
  and usage = "simulate_cones prefix\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [ n_draws_exp_opt; seed_opt ] in
  Arg.parse args anon_arg usage;
  List.rev !files

let rng = Gsl_rng.make Gsl_rng.MT19937
let exponential_dist_f mean = 
  Gsl_randist.exponential rng ~mu:mean

(* if we are given 3n-3, what are 2n-3 and n? *)
let n_edges_and_n_taxa_of_dim dim = 
  assert(dim mod 3 = 0);
  let n = 1 + (dim / 3) in
  (2*n-3, n)

let rec pow10 e = 
  if e <= 0 then 1
  else 10 * (pow10 (e-1))

let simulate_in_cones rand_gen_f prefix = 
  let q_cones = Cones_core.q_cones_of_fname (prefix^".poly.out")
  and (lower_tree, tree_list_list) =
    Btree_io.parse_trees_file (prefix^".trees") 
  and n_draws = pow10 !n_draws_exp
  in
  let n_vars = Cones_core.n_vars_of_cone q_cones.(0) in
  (* add one below because internode is the number of internal nodes between the
   * two taxa *)
  let avg_pairwise_dist = 
    1. +. (Btree.average_internode_distance lower_tree)
  in
  let (n_edges, n_taxa) = n_edges_and_n_taxa_of_dim n_vars in
  (* we simulate with the edges having parameter one, and the extra distances
   * having parameter avg_pairwise_dist. *)
  let simulation_params = 
    Array.concat
    [
      [| 0. |]; (* for the polymake first coordinate *)
      (Array.make n_edges 1.);
      (Array.make n_taxa avg_pairwise_dist)
    ]
  in
  assert(n_vars + 1 = Array.length simulation_params);
  let r_cones = Array.map Cones_core.r_cone_of_q_cone q_cones in
  let counts = Array.map (fun _ -> 0) r_cones in
  for i=1 to n_draws do
    let containing_cone = 
      Cones_core.r_point_in_which_cone 
        r_cones
        (Array.map rand_gen_f simulation_params)
    in
    counts.(containing_cone) <- counts.(containing_cone) + 1;
  done;
  let raw_results = 
    Array.map (fun count -> Base.int_div count n_draws) counts
  in
  assert(Array.length raw_results = List.length tree_list_list);
  let ch_out = open_out (prefix^".sim_raw") in
  Printf.fprintf 
    ch_out
    "# simulation with exponential distribution and 10^%d draws on %s\n"
    !n_draws_exp
    prefix;
  Array.iter (fun x -> Printf.fprintf ch_out "%g\n" x) raw_results;
  close_out ch_out


    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let files = parse_args () in
    Gsl_rng.set rng (Nativeint.of_int !seed);
    List.iter 
      (simulate_in_cones exponential_dist_f)
      files;
  end

