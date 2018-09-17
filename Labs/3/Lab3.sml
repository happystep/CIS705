(* Definition of a Binary Tree *)
datatype 'a BinTree = Leaf of 'a
    | Node of 'a BinTree * 'a BinTree

(* Example of a Binary Tree *)
val Tree1 = Node (
    Node (Node(Leaf(4),Leaf(7)),
        Leaf(8)),
    Node (Leaf(5),
        Node(Leaf(3),Leaf(11))))
    
    
fun foldt operation function (Leaf(x)) = function(x)
| foldt operation function (Node(x,y)) = operation( (foldt operation function x), (foldt operation function y))