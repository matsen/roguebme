(* not every point need be a vertex. therefore we have to be a little careful
 * when doing simulation using POINTS and the list of trees. this utility takes
 * a prefix and reads the prefix.poly.out and the prefix.trees files and makes a
 * prefix.vert_tree file which has the previous entries of the .poly.out file
 * plus a VERT_TREES entry for the trees which correspond to vertices. *)

open MapsSets

let make_vert_tree prefix = 
  let (lower_tree, tree_list_list) =
    Btree_io.parse_trees_file (prefix^".trees") 
  in
  let poly_name = prefix^".poly.out" in
  let props = Polymake.get_properties poly_name in
  let points = 
    Polymake.parse_q_mat (StringMap.find "POINTS" props)
  and verts = 
    Polymake.parse_q_mat (StringMap.find "VERTICES" props) in
  let h = Hashtbl.create (Array.length points) in
  List.iter2
    (fun point tree_list -> 
      Hashtbl.add 
        h 
        point 
        (String.concat " " (List.map Btree.to_string tree_list)))
    (Array.to_list points)
    tree_list_list;
  let out_ch = open_out (prefix^".vert_tree") in
  Printf.fprintf out_ch "# lower tree: %s\n" (Btree.to_string lower_tree);
  let poly_ch = open_in poly_name in
  let rec write_poly_lines () = 
    try 
      Printf.fprintf out_ch "%s\n" (input_line poly_ch);
      write_poly_lines ();
    with End_of_file -> close_in poly_ch
  in
  write_poly_lines ();
  Printf.fprintf out_ch "\nVERT_TREES\n";
  Array.iter
    (fun v -> Printf.fprintf out_ch "%s\n" (Hashtbl.find h v))
    verts;
  close_out out_ch;
  ()

let () =
  if not !Sys.interactive then begin
    Arg.parse [] make_vert_tree "";
  end

