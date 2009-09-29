module OrderedIntUptri = struct
  type t = int Uptri.uptri
  let compare = Uptri.compare
end

module StringableIntUptri = struct
  type t = int Uptri.uptri
  let to_string u = 
    "[|"^
    (String.concat "; " (List.map string_of_int (Array.to_list u.Uptri.data)))
    ^"|]"
end

module IntUptriSet = Set.Make(OrderedIntUptri)
module IntUptriSetFuns = 
  MapsSets.SetFuns (OrderedIntUptri) (StringableIntUptri)

(* the induced action from coordinate permutation on upper triangular matrices *)
let induced_uptri_action u s = 
  Uptri.init (Uptri.size u) (
    fun i j ->
      Uptri.get_loose u (Sn.act s i) (Sn.act s j)
  ) 

let induced_uptri_coord_transformation s coord = 
  let dim = Sn.length s in
  let (i, j) = Uptri.int_to_pair dim coord in
  Uptri.pair_to_int_loose dim (Sn.act s i) (Sn.act s j)

(* the induced action on the flattened coordinates *)
let induced_uptri_coord_action s = 
  Sn.init  
    (Uptri.n_entries_of_dim (Sn.length s)) 
    (induced_uptri_coord_transformation s)

(* add a new column to the right hand side of the uptri *)
let add_col u a = 
  let orig_size = Uptri.size u in
  if orig_size <> Array.length a then failwith "add_col : dims don't match";
  Uptri.init (orig_size + 1) (
    fun i j ->
      if j = orig_size then a.(i) 
      else Uptri.get u i j
  )

let rec cartesian_power l n =
  assert(n >= 0);
  if n = 0 then [[]]
  else (
    let prev = cartesian_power l (n-1) in
    List.flatten (
      List.map (fun x -> List.map (fun sofar -> x::sofar) prev) l)
    )
        
(* lift u to one size bigger, while substituting the elements of subs_options in
* for each new element *)
let lift u subs_options = 
  List.map (add_col u) (
   List.map Array.of_list (cartesian_power subs_options (Uptri.size u)))

