(* Luis Bobadilla - Lab 1 - CIS 705 *)

fun power n x = 
    if n=0 
    then 1 
    else x * power (n-1) x

fun square n = 
    power 2 n 

fun cube n = 
    power 3 n

fun twice f v =
    f(f(v))

fun to9th x = 
    twice cube x

(*
The error given when doing 
to9th = power9 is the following
stdIn:1.2-2.7 Error: operator and operand don't agree [equality type required]
  operator domain: ''Z * ''Z
  operand:         (int -> int) * (int -> int)
  in expression:
    to9th = power 9

This error occurs because it is trying to do an equality type check 
to a type that isn't a equality type (''Z being the more restrictive
polymorphism type for equallity types). There is no reasonable way to 
compare functions. Equality types in SML include int, string, char and bool. 
*)

