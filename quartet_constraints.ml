let make_quartet_constraints n_tips qinfo = 
  let n_extra_dims = match qinfo with
  | (quartet, extracols)::rest -> Array.length extracols
  | [] -> assert(false)
  in
  List.map (* constrain these to be zero *)
    Constraint.constrain_zero (
      (List.map (
        fun (quartet, extracols) ->
        (* equality constraints for tree metric (zero extended) *)
          Constraint.extend_zero 
            n_extra_dims
            (Uptri.to_array (Tree_lp.quartet_equality n_tips quartet)))
      qinfo) @
(* equality constraints for internal edge *)
      (List.map (
        fun (quartet, extracols) ->
          Array.append
            (Uptri.to_array (Tree_lp.quartet_internal n_tips quartet))
            (Array.map (fun x -> -. x) extracols))
      qinfo))

