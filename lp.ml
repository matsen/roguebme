
type direction = Maximize | Minimize 

let trans_dir = function
  | Maximize -> Glpk.Maximize
  | Minimize -> Glpk.Minimize

(* solve :
  * lin_constraints is a list of (constraint_v, (lower,upper))
  * *)
let solve objective objective_v lin_constraints var_constraints = 
  let n_vars = Array.length objective_v 
  and constraint_vs = Array.of_list (List.map fst lin_constraints) 
  and constraint_bnds = Array.of_list (List.map snd lin_constraints) 
  in
  Array.iter (fun v -> assert(n_vars = Array.length v)) constraint_vs;
  assert(n_vars = Array.length var_constraints);
  let lp = Glpk.make_problem 
             (trans_dir objective)
	     objective_v
             constraint_vs
             constraint_bnds
             var_constraints
  in
  Glpk.set_message_level lp 0;
  try
    Glpk.scale_problem lp;
    Glpk.use_presolver lp true;
    Glpk.simplex lp;
    Some (Glpk.get_obj_val lp, Glpk.get_col_primals lp)
  with
  | Glpk.No_primal_feasible_solution -> None
  | Glpk.No_dual_feasible_solution -> None

  (*
  let (x,v) = Lp.solve  
            Lp.Maximize
            [|10.; 6.; 4.|]
            [
              [|1.; 1.; 1.|], (-.infinity, 100.);
              [|10.; 4.; 5.|],(-.infinity, 600.);
              [|2.; 2.; 6.|], (-.infinity, 300.);
	     ]
	     [| 0., infinity; 0., infinity; 0., infinity|] in
  Printf.printf "Z: %g    x0: %g    x1: %g    x2: %g\n%!" x v.(0) v.(1) v.(2)
*)
