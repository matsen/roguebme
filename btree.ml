
open MapsSets

type btree = Node of btree * btree | Leaf of int

let rec recur f start = function
  | Node(a,b) -> f (recur f start a) (recur f start b)
  | Leaf i -> start i

let n_leaves = recur ( + ) (fun _ -> 1)
let max_ind = recur max (fun i -> i)
let taxon_list = recur ( @ ) (fun i -> [i])
let to_string = recur (fun x y -> "("^x^","^y^")") string_of_int

let rec del_taxon tax = function
  | Node(Leaf i,b) when i = tax -> b
  | Node(a, Leaf i) when i = tax -> a
  | Node(a, b) -> Node(del_taxon tax a, del_taxon tax b)
  | Leaf i as t -> t

let rec ppr_btree ff t =
  (* install_printer apparently doesn't like recursive functions *)
  let rec aux = function
    | Node(a, b) -> Format.fprintf ff "@[(%a,@,%a)@]" ppr_btree a ppr_btree b
    | Leaf i -> Format.fprintf ff "%d" i
  in
  aux t

let print t = 
  ppr_btree Format.std_formatter t;
  Format.pp_print_newline Format.std_formatter ()

let rec comb n = 
  assert(n > 0);
  if n = 1 then Leaf 0
  else Node (comb (n-1), Leaf (n-1))

(* a comb can either be rooted at one of the ends, which is an end_comb, or it
 * can be rooted in the middle, or it can be rooted at a pendant edge. 
 * if it is rooted in the middle, then each side of the root is an end_comb. if
 * it is rooted at a pendant edge, then it looks like  *)
let is_comb t = 
  let rec is_end_comb = function
    | Node(Leaf _, b) -> is_end_comb b
    | Node(a, Leaf _) -> is_end_comb a
    | Leaf _ -> true
    | _ -> false
  in
  match t with
  | Node(Leaf _, Node(a,b)) (* rooted at pendant edge *)
  | Node(Node(a,b), Leaf _) (* rooted at pendant edge *)
  | Node(a,b) (* rooted at an internal edge *)
    -> is_end_comb a && is_end_comb b
  | Leaf _ -> true

let pair_cartesian_product la lb = 
  List.flatten (List.map (fun a -> List.map (fun b -> (a,b)) lb) la)

(* this makes all of the rooted tree shapes *)
let make_all_rooted_shapes tree_size = 
  (* aux returns list_of_trees *)
  let rec aux start_index make_of_size =
    assert(make_of_size > 0);
    if make_of_size = 1 then [Leaf start_index]
    else
      let max_lhs = make_of_size / 2 in
      (* lhs = left hand size *)
      let rec aux2 lhs trees = 
        if lhs > max_lhs then trees
        else aux2 (lhs+1) (
          trees @ 
          (List.map (fun (a,b) -> Node(a,b))
          (pair_cartesian_product 
            (aux start_index lhs) 
            (aux (start_index+lhs) (make_of_size-lhs)))))
      in
      aux2 1 []
  in
  aux 0 tree_size

(* this makes all of the unrooted trees 
  # List.map List.length (List.map make_all_unrooted [4;5;6;7;8;9]);;
- : int list = [3; 15; 105; 945; 10395; 135135]
*)
let rec make_all_unrooted size = 
  if size <= 1 then [Leaf 0]
  else begin
    let addleaf t = Node(t, Leaf (size-1)) in
    let rec add_everywhere t = 
      (addleaf t)::
        (match t with
        | Node(a,b) -> 
            ((List.map (fun below -> Node(below,b)) (add_everywhere a)) @
            (List.map (fun below -> Node(a,below)) (add_everywhere b)))
        | Leaf i -> [])
    in
    let unrooted_add_everywhere = function 
      | Node(a,b) -> 
          List.map (fun below -> Node(a,below)) (add_everywhere b)
    | Leaf i as leaf ->
        [addleaf leaf]
    in
    List.flatten (List.map unrooted_add_everywhere (make_all_unrooted (size-1)))
  end

      
(* make a list of
 * ((tax_a, tax_b), <number of internal nodes betw tax_a and tax_b>) *)
let make_internode_distance_list tree = 
  (* return *)
  let increment_root_dist (index, dist) = (index,dist+1) in
  let rec combine add_factor a b =
    let (a_root_dists, a_internode_dists) = aux a
    and (b_root_dists, b_internode_dists) = aux b in
    (List.map increment_root_dist (a_root_dists @ b_root_dists),
    a_internode_dists @
    b_internode_dists @ (
      List.map 
      (fun ((a_ind,a_dist),(b_ind,b_dist)) ->
        ((a_ind,b_ind),add_factor+a_dist+b_dist))
      (pair_cartesian_product a_root_dists b_root_dists)))
  and aux = function
    | Node(a,b) -> combine 1 a b
    | Leaf i -> ([(i, 0)],[])
  in
  (* special treatment for root, unfortunately *)
  match tree with
  | Node(a,b) -> snd (combine 0 a b)
  | Leaf i -> assert(false)

let average_internode_distance tree = 
  let dists = List.map snd (make_internode_distance_list tree) in
  Base.int_div (List.fold_left ( + ) 0 dists) (List.length dists)

(*
 * quartetable_rerootings :
# Btree.t;;
- : Btree.btree = (((0,1),(2,3)),(4,5))
# Btree.quartetable_rerootings Btree.t;;
- : ((Btree.btree * Btree.btree) * (Btree.btree * Btree.btree)) list =
  [(((4,5), (2,3)), (0, 1)); (((4,5), (0,1)), (2, 3));
   (((0,1), (2,3)), (4, 5))]
  *)
let quartetable_rerootings tree = 
  let rec aux above = function
    | Node(a,b) -> begin
      (match a with
      | Node(r,s) -> ((above, b), (r, s))::(aux (Node(above, b)) a)
      | Leaf i -> []) @
      (match b with
      | Node(r,s) -> ((above, a), (r, s))::(aux (Node(above, a)) b)
      | Leaf i -> [])
    end
    | Leaf _ -> []
  in
  match tree with
  | Node(left,right) -> 
      (aux left right) @ 
      (aux right left) @ (
        (* we need to take special care to get this edge too, if needed *)
        match (left,right) with
        | (Node(a,b),Node(c,d)) -> [(a,b),(c,d)]
        | _ -> [])
  | Leaf _ -> []

let rec cartesian_product = function
  | a_list :: list_list ->
      let prev = cartesian_product list_list in
      List.flatten ( 
        List.map (fun x -> List.map (fun l -> x::l) prev) a_list)
  | [] -> [ [] ]

let quartets tree = 
  (* List.flatten ( *)
    List.map (
      fun ((a,b),(c,d)) ->
        cartesian_product (List.map taxon_list [a;b;c;d])
    ) (quartetable_rerootings tree)

let t = Node(Node(Node(Leaf 0,Leaf 1), Node(Leaf 2, Leaf 3)),Node(Node(Leaf 4,
Leaf 5), Node(Leaf 6, Leaf 7)))
let t = Node(Node(Node(Leaf 0,Leaf 1), Node(Leaf 2, Leaf 3)),Node(Leaf 4, Leaf 5))
(* let t = Node(Node(Leaf 0,Leaf 1), Node(Leaf 2, Leaf 3)) *)
