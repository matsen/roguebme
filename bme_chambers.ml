(* to make the chambers of the BME fan *)

let facet_list_to_file fname fl = 
  let ch = open_out fname in
  Printf.fprintf ch "%s\n" "FACETS";
  Polymake.write_q_vector_list ch fl;
  close_out ch

let min_n_tips = 4
and max_n_tips = 7

let make_vertices n_tips = 
  let trees = Array.of_list (Btree.make_all_unrooted n_tips) in
  let vertices = Bme.normed_bme_uptris_of_trees trees in
  let ch = open_out (Printf.sprintf "bme_points%d.txt" n_tips) in
  let ff = Format.formatter_of_out_channel ch in
  for ind=0 to (Array.length trees)-1 do
      Printf.fprintf ch "%d\n" ind;
      Btree.ppr_btree ff trees.(ind);
      Format.pp_print_newline ff ();
      Uptri.ppr_int_uptri ff vertices.(ind);
      Format.pp_print_newline ff ();
  done;
  close_out ch;
  vertices


let () = begin
  for n_tips = min_n_tips to max_n_tips do
    let pos_orthant = 
      Polymake.positive_orthant_facets (Uptri.n_entries_of_dim n_tips)
    and vertices = make_vertices n_tips 
    in
    Array.iteri
      (fun i _ ->
        facet_list_to_file 
          (Printf.sprintf "bme_t%d_vert%03d.poly" n_tips i)
          ((List.map 
            (fun u -> Array.map Num.num_of_int (Uptri.to_array u))
            (Bme.facets vertices i))
            @ pos_orthant))
      vertices;
  done
end
