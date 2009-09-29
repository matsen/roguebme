open Fam_batteries

let float_arr_of_int_arr = Array.map float_of_int
let constrain_zero a = (a, (0., 0.))
let constrain_positive a = (a, (0., infinity))
let constrain_negative a = (a, (-. infinity, 0.))
let constrain_float a x = (a, (x, x))

let ei dims i = 
  assert(0 <= i && i < dims);
  Array.init dims (fun j -> if i=j then 1. else 0.)

let extend_zero dims a = Array.append a (Array.make dims 0.)
let extend_constrained_zero dims (a, c) = 
  (extend_zero dims a, c)

let positive_orthant dims = 
  List.map 
    (fun i -> 
      constrain_positive (ei dims i))
    (ListFuns.range dims)

let ppr_constraint ff (a, (low,high)) = 
  Format.fprintf ff "@[%a @[(%a, %a)@]@]" 
    (* Uptri.ppr_float_uptri (Uptri.of_array a) *)
    Ppr.ppr_float_array a
    Format.pp_print_float low
    Format.pp_print_float high

let ppr_constraint_list = Ppr.ppr_list ppr_constraint 
let print_constraint_list = Ppr.print_of_ppr ppr_constraint_list

let ppr_constraint_results ff (v,(a,(low,high))) = 
  Format.fprintf ff "@[%a @[(%a <= %a <= %a)@]@]" 
    Uptri.ppr_float_uptri (Uptri.of_array a)
    Ppr.ppr_gfloat low
    Ppr.ppr_gfloat (Bme.float_uptri_dot v (Uptri.of_array a))
    Ppr.ppr_gfloat high

let ppr_constraint_results_list = Ppr.ppr_list ppr_constraint_results

let print_constraint_results v = 
  Ppr.print_of_ppr 
    (fun ff c_list -> 
      ppr_constraint_results_list ff 
        (List.map (fun c -> (v,c)) c_list))

