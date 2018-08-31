(* Luis Bobadilla - Lab 2 - CIS 705 *)

fun add_squares (nil) = 0
    | add_squares (n :: ns) = n*n + add_squares(ns)
 
fun mult_nonzero (nil) = 0 
    | mult_nonzero ( n :: ns ) =
        if n <> 0 
            then  n * mult_nonzero (ns)
        else
            mult_nonzero(ns)

fun half_evens (nil) = []
    | half_evens (n :: ns) = 
        if (n mod 2) = 0
        then 
        half_evens((n div 2) :: ns)
        else
        half_evens(ns)

(*   USE MAP, FILTER AND FOLDR
fun add_squares' (nil) = 0
    | add_squares' (x) = map(fn y => op + (map(fn x => x*x )))

fun mult_nonzero' (nil) = 0
    | mult_nonzero' (n::ns) = List.filter(fn n => n >= 0) * mult_nonzero' (ns)
(
fun half_evens' (nil) = []
    | half_evens' (n::ns) = 
        if (n mod 2) 
        then 
        List.foldr (op div) 

        *)