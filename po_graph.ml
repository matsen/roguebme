(*
 * make a graph of the partial order of the vertices
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
  let usage = "po_graph prefix\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [ ] in
  Arg.parse args anon_arg usage;
  List.rev !files


let write_graph prefix = 
  (* get the upper trees, with multiplicity *)
  let tree_strs = 
    List.tl (File_parsing.string_list_of_file (prefix^".trees"))
  in
  let props = Polymake.get_properties (prefix^".poly.out") in
  let points = Polymake.parse_q_mat (StringMap.find "VERTICES" props) in
  assert(List.length tree_strs = Array.length points);
  let comparisons = 
    Uptri.init 
      (Array.length points)
      (fun i j -> Array_po.compare_array points.(i) points.(j))
  in
  (* write to file *)
  let ch = open_out (prefix^".po.dot") in
  Printf.fprintf ch "digraph g {\nrankdir=LR\n";
  let interesting = Array.make (Array.length points) false in
  let make_interesting i = interesting.(i) <- true in
  (* write the graph *)
  Uptri.iterij 
    (fun i j -> function
      | Array_po.Incomparable -> ()
      (* we point to the bigger one *)
      | Array_po.Comparable c ->
          make_interesting i;
          make_interesting j;
          if c > 0 then
            Printf.fprintf ch "\tp%d -> p%d\n" i j
          else if c < 0 then
          Printf.fprintf ch "\tp%d -> p%d\n" j i
          else
            (* they are equal. this shouldn't happen *)
            assert(false))
    comparisons;
  (* write the labels *)
  ListFuns.iteri
    (fun i tree_label ->
      if interesting.(i) then
        Printf.fprintf ch "\tp%d [label=\"%s\"]\n" i tree_label)
    tree_strs;
  Printf.fprintf ch "}\n";
  close_out ch;
  ()

let () =
  if not !Sys.interactive then begin
    let files = parse_args () in
    List.iter write_graph files;
  end

