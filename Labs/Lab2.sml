(* Luis Bobadilla - Lab 2 - CIS 705 *)

fun add_squares (nil) = 0
    | add_squares (n :: ns) = n*n + add_squares(ns)
 
fun mult_nonzero (nil) = 1 
    | mult_nonzero ( n :: ns ) =
        if n <> 0 
            then  n * mult_nonzero (ns)
        else
            mult_nonzero(ns)
(* 
I have to return 1 for NIL, or it won't work because if you
 multiply by 0, you will always end up with 0.
*)

fun half_evens (nil) = []
    | half_evens (n :: ns) = 
        if (n mod 2) = 0
        then 
        (n div 2) :: half_evens(ns)
        else
        half_evens(ns)

  
fun add_squares' x = foldr op+ 0 (map (fn x => x*x) x)


fun mult_nonzero' x = foldr op* 1 (List.filter(fn x => x > 0) x) 
(* 
I have to initialize op* to 1, or it won't work because if you
 multiply by 0, you will always end up with 0.
*)


fun half_evens' x = map ( fn x => x div 2 ) (List.filter (fn x => x mod 2 = 0) x)
