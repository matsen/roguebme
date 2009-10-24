open MapsSets
open Fam_batteries

(* manipulation *)

let prepend_q_to_vector x v = Array.append [|x|] v

(* input *)

let whitespace = Str.regexp "[ \t]+"

(* returns a string map from property name to string list with data 
 * there needs to be a comment line up top. *)
let get_properties fname = 
  let unfiltered = 
    List.map
      (function | hd::tl -> tl | _ -> assert(false))
      (File_parsing.partition_list
        (fun s -> 
          Str.string_match File_parsing.empty_line_rex s 0)
        (File_parsing.string_list_of_file fname)) in
  List.fold_right
    (function 
      | hd::tl -> 
          (* print_endline ("found property "^hd);  *)
          StringMap.add hd tl
      | _ -> assert(false))
    (List.filter (( <> ) []) unfiltered)
    StringMap.empty

let parse_q_vect s = 
  Array.map 
    Num.num_of_string 
    (Array.of_list (Str.split whitespace s))

let parse_q_mat sl = 
  Array.of_list (List.map parse_q_vect sl)

let parse_index_list s = 
  Array.map 
    int_of_string
    (Array.of_list (Str.split whitespace s))

let parse_index_mat sl = 
  Array.of_list (List.map parse_index_list sl)



(* output *)

let polymake_vect_of_q_vect v = 
  String.concat 
    " "
    (List.map Num.string_of_num (Array.to_list v))

let write_q_vector ch v = 
  Printf.fprintf ch "%s\n" (polymake_vect_of_q_vect v)

let write_q_vector_list ch fl =
  List.iter (write_q_vector ch) fl


let e_i dim i = 
  let one = Num.num_of_int 1
  and zero = Num.num_of_int 0 in
  assert(i < dim);
  Array.init dim (fun j -> if i=j then one else zero)

(*
# positive_orthant_facets 3;;
- : Num.num array list =
[[|Num.Int 0; Num.Int 1; Num.Int 0; Num.Int 0|];
 [|Num.Int 0; Num.Int 0; Num.Int 1; Num.Int 0|];
 [|Num.Int 0; Num.Int 0; Num.Int 0; Num.Int 1|]]
*)
let positive_orthant_facets dim = 
  List.map (fun i -> e_i (dim+1) (i+1)) (ListFuns.range dim)

