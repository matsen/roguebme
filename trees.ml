
(* ((0,1),(2,3)) *)
let lower_tree4 = 
  Btree.Node(
    Btree.Node(Btree.Leaf 0, Btree.Leaf 1),
    Btree.Node(Btree.Leaf 2, Btree.Leaf 3))

(* ((0,1),((2,3),4)) *)
let lower_tree5 = 
  Btree.Node(
    Btree.Node(Btree.Leaf 0, Btree.Leaf 1),
    Btree.Node(
      Btree.Node(Btree.Leaf 2, Btree.Leaf 3),
      Btree.Leaf 4))

(* (((0,1),4),((2,3),5)) *)
let lower_tree6a = 
  Btree.Node(
    Btree.Node(Btree.Node(Btree.Leaf 0, Btree.Leaf 1), Btree.Leaf 4),
    Btree.Node(Btree.Node(Btree.Leaf 2, Btree.Leaf 3), Btree.Leaf 5))

(* (((0,1),(2,3)),(4,5)) *)
let lower_tree6b = 
  Btree.Node(
    Btree.Node(
      Btree.Node(Btree.Leaf 0, Btree.Leaf 1),
      Btree.Node(Btree.Leaf 2, Btree.Leaf 3)),
    Btree.Node(Btree.Leaf 4, Btree.Leaf 5))

  (*
let strs = 
  List.map 
    Btree.to_string
    [lower_tree4; lower_tree5; lower_tree6a; lower_tree6b]

val strs : string list =
  ["((0,1),(2,3))"; "(((0,1),(2,3)),4)"; "(((0,1),4),((2,3),5))";
  "(((0,1),(2,3)),(4,5))"]
  *)
