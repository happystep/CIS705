(* CIS 705 | Luis Bobadilla | Lab 3 *)

(* Definition of a Binary Tree *)
datatype 'a BinTree = Leaf of 'a
    | Node of 'a BinTree * 'a BinTree

(* Example of a Binary Tree for testing *)
val Tree1 = Node (
    Node (Node(Leaf(4),Leaf(7)),
        Leaf(8)),
    Node (Leaf(5),
        Node(Leaf(3),Leaf(11))))

(* Prints the leaves of the tree with " and " in between *) 
fun print_tree f (Leaf (x)) = f(x)
    | print_tree f (Node (x, y)) = (print_tree(f) (x)) ^ " and " ^  (print_tree(f) (y)) 

(* This is a helper function for deepest, it returns the height of a Tree *)
fun height(Leaf(x)) = 0 
    | height(Node(x,y)) =
    if (height(x) < height(y))
        then 1 + height(y) 
    else 1 + height(x) 

(* deepest that returns the height (the maximal distance
from the root to a leaf) of a binary tree, together with a
list of the “deepest” nodes *)
fun deepest (Leaf(x)) = (0, [x])
    | deepest (Node(x,y)) = 
    if (height(x) < height(y))
        then
            let val (l,k) = deepest(y)
                in 
                (l+1, k)  
                end
    else if (height(x) = height(y)) 
        then
            let val (lx,kx) = deepest(x)
                val (ly,ky) = deepest(y)
                in 
                    (ly+1, kx@ky)
            end
    else 
        let val (i,j) = deepest(x) 
            in 
            (i+1,j)
        end

(* foldt takes an operation, a function and a tree and returns the operation applied on the values of the tree *)
fun foldt operation function (Leaf(x)) = function(x)
    | foldt operation function (Node(x,y)) = operation( (foldt operation function x), (foldt operation function y))
 
 (* same functionality as print_tree *)
fun print_tree' f x = foldt (fn (x,y) => x ^ " and " ^ y) f x 