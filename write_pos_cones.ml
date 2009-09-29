open MapsSets
open Fam_batteries

let write_cones_from_fname fname = 
  let cones = Cones_core.q_cones_of_fname fname
  and base_fname = Filename.chop_extension fname in
  let n_vars = Array.length cones.(0).(0) - 1 in
  let max_width = Base.int_width ((Array.length cones)-1) in
  let zero_pad i = Base.zero_pad_int_to_width max_width i in
  Array.iteri
    (fun i ineqs ->
      let ch = open_out (base_fname^"."^(zero_pad i)^".poly") in
      Printf.fprintf ch "INEQUALITIES\n";
      Array.iter
        (fun v ->
          Printf.fprintf ch "%s\n" 
            (Polymake.polymake_vect_of_q_vect v))
      ineqs;
      Polymake.write_q_vector_list ch 
        (Polymake.positive_orthant_facets n_vars);
      close_out ch)
    cones


let () = if not !Sys.interactive then 
  (Array.iter 
    write_cones_from_fname 
    (Array.sub Sys.argv 1 ((Array.length Sys.argv)-1)))


