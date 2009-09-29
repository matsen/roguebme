open Fam_batteries

let int_div x y = (float_of_int x) /. (float_of_int y) 

(* pull out the elements of a list which are unique after the application of f.
# list_f_uniques fst [1,2; 1,3; 4,5; 2,3];;
- : (int * int) list = [(1, 2); (4, 5); (2, 3)]
 *)
let list_f_uniques f linit = 
  List.rev 
    (fst 
      (List.fold_left 
        (fun (l, fl) x ->
          let fx = f x in
          if List.mem fx fl then (l,fl)
          else (x::l, fx::fl))
      ([], [])
      linit))

(* group by uniqueness after the application of f
 # group_by_f fst [1,2; 1,3; 4,5; 2,3];;
- : (int * int) list list = [[(1, 2); (1, 3)]; [(4, 5)]; [(2, 3)]]
 *)
let group_by_f f linit = 
  let h = Hashtbl.create ((List.length linit) / 2) in
  let applied = List.map f linit in
  List.iter2 (fun k x -> Hashtbl.add h k x) applied linit;
  List.map 
    (fun k -> List.rev (Hashtbl.find_all h k))
    (list_f_uniques (fun x -> x) applied)

let rec int_width i = 
  if i < 0 then 1+(int_width (-i))
  else if i < 10 then 1
  else 1 + (int_width (i/10))

let zero_pad_int_to_width width i = 
  let s = string_of_int i in
  StringFuns.left_pad (width - (String.length s)) '0' s

let array_all_same a = ListFuns.all_same (Array.to_list a)

