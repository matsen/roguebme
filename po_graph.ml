(*
 * make a graph of the partial order of the vertices
 * as argument, give it a prefix where a prefix.poly.out and a prefix.trees file
 * live.
 *)


open MapsSets
open Fam_batteries

let parse_args () =
  let files  = ref [] in
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
  let ch = open_out (prefix^".po.dot")
  and dom_ch = open_out (prefix^".domination") in
  Printf.fprintf ch "digraph g {\nrankdir=LR\n";
  let interesting = Array.make (Array.length points) false in
  let make_interesting i = interesting.(i) <- true in
  let write_domination i j = 
    Printf.fprintf dom_ch "[%s] > [%s]\n"
      (Polymake.polymake_vect_of_q_vect points.(i))
      (Polymake.polymake_vect_of_q_vect points.(j));
  in
  (* write the graph *)
  Uptri.iterij 
    (fun i j -> function
      | Array_po.Incomparable -> ()
      (* we point to the bigger one *)
      | Array_po.Comparable c ->
          make_interesting i;
          make_interesting j;
          if c > 0 then begin
            Printf.fprintf ch "\tp%d -> p%d\n" i j;
            write_domination i j
          end
          else if c < 0 then begin
            Printf.fprintf ch "\tp%d -> p%d\n" j i;
            write_domination j i
          end
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
  close_out dom_ch;
  ()

let () =
  if not !Sys.interactive then begin
    let files = parse_args () in
    List.iter write_graph files;
  end

