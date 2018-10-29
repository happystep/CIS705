; Hunter Goddard
; CIS 705
; Fall 2018

#lang racket
(require "project3.rkt")

(newline)
(display "Testing function application, should give 16 (changes 1-4):\n")
(run "app fn x * x x 4")
(newline)

(display "Testing let, should give 16 (changes 1 and 5):\n")
(run "let x 4 * x x")
(newline)

(display "Testing let with a function binding, should give 16 (changes 1-5):\n")
(run "let x fn a * a a app x 4")
(newline)

(display "Adding 2 + 2 (change 6a):\n")
(run "+ 2 2")
(newline)

(display "Testing if statements (changes 7 and 8):\n")
(display "True; should show 2 (false gives 3)\n")
(run "if 1 2 3")
(display "False; should show 2 (true gives 1)\n")
(run "if 0 1 2")
(display "Statement with an operation for the conditional (should give 1)\n")
(run "if * 3 2 1 0")
(newline)

(display "Creating tuple (Nintendo, SNES) (change 9)\n")
(run "tuple Nintendo SNES")
(newline)

(display "Adding tables (Nintendo, SNES) and (Sega, Genesis) and (Nintendo, SNES) (changes 6b and 9)\n")
(display "Should (optionally) avoid duplicates\n")
(run "++ tuple Nintendo SNES tuple Sega Genesis tuple Nintendo SNES")
(newline)

(display "Joining tables (Sega, Genesis) and (Genesis, Sonic) (changes 9 and 10)\n")
(display "Should give (Sega, Sonic)\n")
(run "* tuple Sega Genesis tuple Genesis Sonic")
(newline)

(display "Joining tables (Sega, Genesis) and (Xbox, Halo) (changes 9 and 10)\n")
(display "Should give an empty table (no matches)\n")
(run "* tuple Sega Genesis tuple Xbox Halo")
(newline)

(display "Joining tables (Xbox, Halo) and (Microsoft, Xbox) (changes 9 and 10)\n")
(display "Should give an empty table (joins are on the \"middle\" values)\n")
(run "* tuple Xbox Halo tuple Microsoft Xbox")
(newline)

(display "Testing join with add: (changes 6b, 9, and 10)\n")
(display "Joining ((Nintendo, SNES) (Sega, Genesis)) with ((SNES, Mario) (Genesis, Sonic))\n")
(display "Should return ((Nintendo, Mario) (Sega, Sonic))\n")
(run "* + tuple Nintendo SNES tuple Sega Genesis + tuple SNES Mario tuple Genesis Sonic")
(newline)

(display "\n*** Amtoft's tests from assignment description ***\n\n")
(display "Should return 13:\n")
(run
"let plus fn x fn y + x y
let plusfive app plus 5
app plusfive 8")
(newline)

(display "Should return 98:\n")
(run
"let twice fn f fn x app f app f x
let multseven fn x * 7 x 
app app twice multseven 2")
(newline)

(display "Should return 720:\n")
(run
"let Z fn f app fn x app f fn v app app x x v fn x app f fn v app app x x v
let Fac fn f fn n 
   if n * n app f - n 1 
   1
app app Z Fac 6")
(newline)

(display "Should return a list '((\"France\" . \"Paris\")) ")
(display "containing only the pair (France, Paris).\n")
(run "tuple France Paris")
(newline)

(display "Should return a list containing the pair (France, Paris) ")
(display "and the pair (Spain, Madrid)\n")
(run
"+ tuple France Paris
   tuple Spain Madrid")
(newline)

(display "Should return a list containing only the pair (Spain, Madrid)\n")
(run
"let two + tuple France Paris
           tuple Spain Madrid
let one tuple France Paris
- two one")
(newline)

(display "Should return the pairs (France, Paris) and (Finland, Helsinki)\n")
(run
"let table + + tuple France Paris tuple Spain Madrid tuple Finland Helsinki
select1 F table")
(newline)

(display "Should return the tuples (a2,c3), (a2,c4), (a3,c3), (a3,c4), (a2,c1)\n")
(display "(the order does not matter)\n")
(run
"let table1 + + + tuple a1 b1 tuple a2 b2 tuple a3 b2 tuple a2 b3
let table2 + + + tuple b3 c1 tuple b2 c3 tuple b2 c4 tuple b4 c2
* table1 table2")
(newline)

(display "Should return the pairs (Asia, Tokyo), (Asia, Beijing), (America, DC), (Africa, Nairobi)\n")
(run
"let countries 
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
(newline)