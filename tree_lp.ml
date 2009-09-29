open Fam_batteries

let sparse_int_uptri_of_list dims l =
  let u = Uptri.create dims 0 in
  List.iter (
    fun (x,y,i) -> Uptri.set_loose u x y i
  ) l;
  u

let sparse_float_uptri_of_list dims l =
  let u = Uptri.create dims 0. in
  List.iter (
    fun (x,y,i) -> Uptri.set_loose u x y i
  ) l;
  u

(*
# choose [1;2;3;4;5] 2;;
- : (int list * int list) list =
  [([1; 2], [3; 4; 5]); ([1; 3], [2; 4; 5]); ([2; 3], [1; 4; 5]);... etc...
*)
let choose from_list k = 
  let rec aux from k sofar =
    assert(k>=0);
    if k=0 then 
      List.map (fun (xin, xout) -> (xin, List.rev_append from xout)) sofar
    else begin
      match from with
      | x::l ->
          (* add it to the in set *)
          (aux l (k-1) (List.map (fun (xin, xout) -> (x::xin, xout)) sofar)) @
          (* don't add it in *)
          (aux l k (List.map (fun (xin, xout) -> (xin, x::xout)) sofar))
      | [] -> 
          (* k > 0 *)
          []
    end
  in
  List.rev (aux (List.rev from_list) k [[],[]])

(* single_triangle_ineq :
 * expresses that d(x,z) + d(z,y) >= d(x,y)
 *)
let single_triangle_ineq dims x y z =
  Constraint.constrain_positive (
    Constraint.float_arr_of_int_arr (
      Uptri.to_array (
        sparse_int_uptri_of_list dims [x,z,1; z,y,1; x,y,-1])))

(* triangle_ineqs :
 * make all of the tri ineqs for a given dimension
 * *)
let triangle_ineqs dims = 
  List.map ( function 
    | ([x;y],[z]) -> single_triangle_ineq dims x y z
    | _ -> assert(false)
  ) (
    List.flatten (
      List.map (
        fun (threeList,_) -> choose threeList 2
      ) (choose (ListFuns.range dims) 3))
  )

(* the difference between the long pairs of paths *)
let quartet_equality dims ((a,b),(c,d)) = 
  sparse_float_uptri_of_list dims 
    [a, c, 1.; b, d, 1.; a, d, -.1.; b, c, -.1.; ] 

(* half the difference between the long pairs and the short pair *)
let quartet_internal dims ((a,b),(c,d)) = 
  sparse_float_uptri_of_list dims 
    [a, c, 0.5; b, d, 0.5; a, b, -.0.5; c, d, -.0.5; ]

(* enforces constraints so that the DM will have the quartet ((a,b),(c,d))
 * with the given bounds on the internal edge *)
let bounded_internal_quartet dims min_internal_edge max_internal_edge ((a,b),(c,d)) = 
  assert(a<>b&&a<>c&&a<>d&&b<>c&&b<>d&&c<>d);
  let equality = quartet_equality dims ((a,b),(c,d))
  and internal = quartet_internal dims ((a,b),(c,d))
  in
  [Uptri.to_array equality, (0.,0.);
  Uptri.to_array internal, 
    (min_internal_edge, max_internal_edge)] 

(* enforces constraints so that the DM will have the given quartet exactly *)
let exact_quartet dims pa pb pc pd internal ((a,b),(c,d)) = 
  (* convert makes a constraint for the given path to be exactly c *)
  let convert l c = 
    Uptri.to_array (sparse_float_uptri_of_list dims l), (c,c) in
  [
    convert [ a, b, 1. ] (pa +. pb);
    convert [ c, d, 1. ] (pc +. pd);
    convert [ a, c, 1. ] (pa +. internal +. pc);
    convert [ a, d, 1. ] (pa +. internal +. pd);
    convert [ b, c, 1. ] (pb +. internal +. pc);
    convert [ b, d, 1. ] (pb +. internal +. pd);
  ]

