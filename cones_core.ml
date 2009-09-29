open MapsSets
open Fam_batteries

(* utils *)
let r_dot v1 v2 = 
  ArrayFuns.fold_left2 (fun stot x y -> stot +. x *. y) 0. v1 v2

let q_vect_diff v1 v2 = 
  ArrayFuns.map2 Num.sub_num v1 v2


(* we stop short of making a polymorphic cone type, but lay some groundwork here
 * in case we do want to later. 
 * *)
let fold_left = Array.fold_left
let iter = Array.iter

let r_cone_of_q_cone cone = 
  Array.map (Array.map Num.float_of_num) cone

let n_vars_of_cone c = 
  let n_vars_arr = Array.map Array.length c in
  if Base.array_all_same n_vars_arr then (n_vars_arr.(0)-1)
  else failwith "n_vars_of_cone: not all ineqs same length"


(* read a polymake file with a GRAPH entry and take differences of vertices so
 * that we get inequalities for the cones.
 *)

let q_cones_of_fname fname = 
  let props = Polymake.get_properties fname 
  in
  let points = 
    Polymake.parse_q_mat (StringMap.find "POINTS" props)
  and verts = 
    Polymake.parse_q_mat (StringMap.find "VERTICES" props)
  and graph = 
    Polymake.parse_index_mat (StringMap.find "GRAPH" props) in
  let n_verts = Array.length verts in
  if Array.length points <> n_verts then
    failwith ("the number of points is not the same as the number of vertices in "^fname);
  assert(points <> [||]);
  assert(Array.length graph = n_verts);
  Array.mapi
    (fun i adj ->
      Array.map
        (fun j -> q_vect_diff verts.(j) verts.(i))
        adj)
    graph

let r_point_in_cone point cone = 
  try
    iter (fun ineq -> if r_dot ineq point < 0. then raise Exit) cone;
    true
  with
  | Exit -> false
 
let r_point_in_which_cones cones point = 
  let in_which = ref [] in
  for i=(Array.length cones)-1 downto 0 do 
    if r_point_in_cone point cones.(i) then
      in_which := i::(!in_which)
  done;
  !in_which
 
let r_point_in_which_cone cones point = 
  match r_point_in_which_cones cones point with
  | [] -> failwith "point not in cones"
  | [i] -> i
  | _ -> failwith "point in multiple cones"
