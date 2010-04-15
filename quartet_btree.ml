
let combine_single x1 x2 = (x1,x2)
let combine_sp x p = (x,p)
let combine_pair (x1,x2) (y1,y2) = Quartet.make x1 x2 y1 y2
let combine_s_tri x1 (x2,(y1,y2)) = Quartet.make x1 x2 y1 y2

(* apply f to pairs in cartesian product *)
let cartesian_f f la lb = 
  List.flatten 
    (List.map (fun a -> List.map (fun b -> f a b) lb) la)

let collect_quartets t = 
  let rec aux = function
    | Btree.Node(a,b) -> 
        let (a_s, a_p, a_t, a_q) = aux a
        and (b_s, b_p, b_t, b_q) = aux b
        in
        (a_s @ b_s,
        a_p @ b_p @ (cartesian_f combine_single a_s b_s),
        a_t @ b_t @ (cartesian_f combine_sp a_s b_p)
                  @ (cartesian_f combine_sp b_s a_p),
        a_q @ b_q @ (cartesian_f combine_pair a_p b_p)
                  @ (cartesian_f combine_s_tri a_s b_t)
                  @ (cartesian_f combine_s_tri b_s a_t))
    | Btree.Leaf i -> ([i],[],[],[])
  in
  let (_,_,_,q) = aux t in
  q

let qset_of_t t = Quartet.QSetFuns.of_list (collect_quartets t)

let qdist t s = Quartet.qset_half_symdiff (qset_of_t t) (qset_of_t s)

let x = Btree_io.of_newick_str
