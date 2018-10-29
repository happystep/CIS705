#lang racket

(require "LuisBobadillaProj3.rkt")

(display "Tests")
(display "Expected (France,Paris)")
(run "tuple France Paris")
(display "\n")

(display "Expected (France,Paris) and the pair (Spain, Madrid)")
(run "+ tuple France Paris 
  tuple Spain Madrid")
(display "\n")


(display "Expected (Spain,Madrid)") 
(run "let two + tuple France Paris 
          tuple Spain Madrid
let one tuple France Paris
- two one")
(display "\n")

(display "Expected (France,Paris) and (Finland,Helsinki).") 
(run "let table + + tuple France Paris tuple Spain Madrid tuple Finland Helsinki
select1 F table") 
(display "\n")

(display "Expected (a2,c3), (a2,c4), (a3,c3), (a3,c4), (a2,c1).") 
(run "let table1 + + + tuple a1 b1 tuple a2 b2 tuple a3 b2 tuple a2 b3
let table2 + + + tuple b3 c1 tuple b2 c3 tuple b2 c4 tuple b4 c2
* table1 table2")
(display "\n")

(display "Expected (Asia,Tokyo), (Asia,Beijing), (America,DC), (Africa,Nairobi).")
(run "let countries 
      +++ tuple Asia Japan 
          tuple Asia China
        + tuple Europe UK 
          tuple America Canada
       ++ tuple America US
          tuple Europe Italy
        + tuple Africa Kenya
          tuple Europe France
let capitals
      +++ tuple Japan Tokyo 
          tuple France Paris
        + tuple Italy Rome 
          tuple UK London
       ++ tuple Canada Ottawa 
          tuple US DC
        + tuple Kenya Nairobi
          tuple China Beijing
let join * countries capitals
 - select1 A join 
   tuple America Ottawa")
