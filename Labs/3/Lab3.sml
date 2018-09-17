(* Definition of a Binary Tree *)
datatype 'a BinTree = Leaf of 'a
    | Node of 'a BinTree * 'a BinTree

(* Example of a Binary Tree *)
val Tree1 = Node (
    Node (Node(Leaf(4),Leaf(7)),
        Leaf(8)),
    Node (Leaf(5),
        Node(Leaf(3),Leaf(11))))

fun print_tree f (Leaf (x)) = f(x)
    | print_tree f (Node (x, y)) = (print_tree(f) (x)) ^ " and " ^  (print_tree(f) (y)) 


fun height(Leaf(x)) = 0 
| height(Node(x,y)) =
if (height(x) < height(y))
then 1 + height(y) 
else 1 + height(x) 


fun foldt operation function (Leaf(x)) = function(x)
| foldt operation function (Node(x,y)) = operation( (foldt operation function x), (foldt operation function y))
 
fun print_tree' f x = (foldt (op^) (f) (x) ) ^ " and "