(*
 * cone realizations = the collection of trees which have a given reduced cone
 *)

open MapsSets
open Fam_batteries

let parse_args () =
  let files  = ref [] in
  (*
  let n_draws_exp_opt = "-e", Arg.Set_int n_draws_exp,
  "The base ten exponent for the number of draws used."
  and seed_opt = "-s", Arg.Set_int seed,
  "Specify a seed for MT19937 random number generation."
  *)
  let usage = "cone_compatibility prefix\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [ ] in
  Arg.parse args anon_arg usage;
  List.rev !files


(* we count the number of others which are compatible with all splits in a given
 * list *)
let how_many_compatible_with_given_list others given_l = 
  List.fold_left
    (fun total other -> 
      total + 
      (if Split.pairwise_restricted_compatible given_l [other] then 1 
      else 0))
    0
    others

let assert_same_and_hd = function
  | [] -> assert(false)
  | x::_ as l -> assert(ListFuns.all_same l); x


let process_simulation prefix = 
  let (lower_tree, tree_list_list) =
    Btree_io.parse_trees_file (prefix^".trees") 
  in
  let lower_splits = Btree_split.internal_splits_of_tree lower_tree in
  let split_list_arr = 
    Array.map 
      (List.map Btree_split.internal_splits_of_tree)
      (Array.of_list tree_list_list)
  in
  let raw_strs = 
          (File_parsing.string_list_of_file (prefix^".sim_raw")) in
  (* note that we expect a single header line at the beginning of the sim_raw
   * file *)
  let raw_results_header = List.hd raw_strs in
  let sim_results = 
    Array.map float_of_string (Array.of_list (List.tl raw_strs)) in
  assert(Array.length split_list_arr = Array.length sim_results);
  (* for each cone, this is the number of splits of the lower tree which are
   * compatible with the trees associated with that cone (it checks that they
   * are the same for all trees associated with the cone (called the
   * realizations as above)) *)
  let number_of_lower_splits_compatible =
    Array.map 
      (fun cone_realizations ->
        assert_same_and_hd
          (List.map (* map across different cone realizations *)
            (how_many_compatible_with_given_list lower_splits)
            cone_realizations))
      split_list_arr
  in
  let collected_results = 
    Array.make 
      (1 + Array.fold_left max 0 number_of_lower_splits_compatible)
      0.
  in
  (* total up the fraction of hits for cones which have each number of
   * compatible splits *)
  for i=0 to (Array.length split_list_arr)-1 do
    let n_compatible = number_of_lower_splits_compatible.(i) in
    collected_results.(n_compatible) <-
      collected_results.(n_compatible) +. sim_results.(i)
  done;
  (* write results *)
  let ch_out = open_out (prefix^".sim_out") in
  Printf.fprintf ch_out "%s\n" raw_results_header;
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

