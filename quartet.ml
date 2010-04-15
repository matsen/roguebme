
exception Not_distinct

(* note that by convention, we have min a1 a2 < min b1 b2 and the a's and b's
 * are sorted. use make below! *)
type quartet = { a1: int; a2: int; b1: int; b2: int}

let all_distinct q = 
  q.a1 <> q.a2 && q.a1 <> q.b1 && q.a1 <> q.b2 &&
    q.a2 <> q.b1 && q.a2 <> q.b2 && q.b1 <> q.b2

let make x1 x2 y1 y2 = 
  let xmin = min x1 x2
  and xmax = max x1 x2
  and ymin = min y1 y2
  and ymax = max y1 y2
  in
  let q = 
    if xmin < ymin then
      {a1=xmin; a2=xmax; b1=ymin; b2=ymax}
    else
      {a1=ymin; a2=ymax; b1=xmin; b2=xmax}
  in
  if not (all_distinct q) then raise Not_distinct;
  q

let to_string q = 
  Printf.sprintf "((%d,%d),(%d,%d))" q.a1 q.a2 q.b1 q.b2

let ppr ff q = Format.fprintf ff "%s" (to_string q)

let of_four_list l = 
  match l with
  | x1::x2::y1::y2::[] -> make x1 x2 y1 y2
  | _ -> invalid_arg "of_four_list"


(* quartet sets *)
module OrderedQuartet = struct
  type t = quartet
  let compare = Pervasives.compare
end

module StringableQuartet = struct
  type t = quartet
  let to_string = to_string
end

module QSet = Set.Make(OrderedQuartet)
module QSetFuns = 
  MapsSets.SetFuns (OrderedQuartet) (StringableQuartet)

(* assuming that s1 and s2 are the same size, what is the half the size of the
 * symmetric difference? *)
let qset_half_symdiff s1 s2 = 
  let n1 = QSet.cardinal s1
  and n2 = QSet.cardinal s2
  in
  if n1 <> n2 then invalid_arg "qset_half_symdiff: sets not same size";
  n1 - (QSet.cardinal (QSet.inter s1 s2))
  

