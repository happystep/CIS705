(* SKELETON INTERPRETER FOR A SMALL INTERACTIVE CALCULATOR 
     CIS505/705, K-State, Fall 2018 *)

     (* Skeleton Written by: Dr. Amtoft
          Modified by: Luis Bobadilla     *)

(* General Functions *)

fun is_digit c = #"0" <= c andalso c <= #"9"

fun digit2int d = (* assumes d is a digit *)   
      ord(d) - ord(#"0")

(* digits2int converts say [#"4",#"5",#"2"] to 254 *)
fun digits2int [] = 0
|   digits2int (d :: ds) = 10 * (digits2int ds) + (digit2int d)

(* power x n returns x^n for n >= 0 *)
fun power x n =  
   if n = 0
   then 1
   else x * power x (n-1)

fun getOp #"+" = SOME (fn v1 => fn v2 => v1 + v2)
|   getOp #"-" = SOME (fn v1 => fn v2 => v1 - v2)
|   getOp #"*" = SOME (fn v1 => fn v2 => v1 * v2)
|   getOp #"^" = SOME (fn v1 => fn v2 => power v1 v2)
|   getOp _ = NONE

fun interpret registers stack current display = let
  fun process ch  = 
     if is_digit(ch) 
     then interpret   
            registers 
            stack 
            (ch::current)
            (Int.toString(digits2int(ch::current)))
     else if ch = #"E"
     then interpret   
            registers 
            stack 
            (tl(current))
            (Int.toString(digits2int(tl(current))))
     else if ch = #"C"
     then interpret    
            registers 
            [] 
            current
            "The stack is cleared!"
     else if ch = #"Z"  
     then interpret 
            (fn i => 0) 
            []
            current
            "The stack and the registers are cleared!"
     else if ch = #"X"
     then (print "Thanks for using the CIS505/705 calculator! Bye\n"; ())
     else let  (* we know that the number currently being typed, if any,
                     has to be put on the stack *)
        val data = digits2int current
        val stack' = if null current 
                     then stack 
                     else data :: stack
       in if ch = #"P"
     then interpret
            registers
            stack'
            []
            (if null current
             then "Error: nothing to push"
             else "The number "^(Int.toString data)^ " is now on the stack")
     else if ch = #"S"
     then interpret     (* MODIFY! *)
            registers
            (* (fn j =>  if j = (hd(stack')) then (hd(tl(stack')))  else j )    *)
            stack'
            current
            "*** store the element just below the top of the stack in the register whose number is at the top!"
     else if ch = #"R"  (* MODIFY! *)
     then interpret
            registers
            stack'
            current
            "*** retrieve the value of the register whose number is at the top of the stack!"
     else case getOp ch of
            SOME oper => 
              interpret   (* MODIFY! *)
                registers
                (oper (hd(    tl(    stack'))) (hd(stack')) :: (tl(tl(stack'))))
                []
                (Int.toString(  oper (hd(    tl(    stack'))) (hd(stack'))   ))
           | NONE => interpret 
                       registers stack current
                       "Error: the input character is not a valid symbol"
     end
   in
  (print (display^"\n? ");
   case (TextIO.inputLine TextIO.stdIn) of
    (SOME line) => (case explode line of
        [ch,#"\n"] => process ch    (* Call the main function *)
      | _ =>  (print "Error: input must be only one character\n";
                interpret 
                  registers stack current display))
   | NONE => (print "Error: input a character\n"; 
                interpret 
                  registers stack current display))
  end

fun run () = interpret 
              (fn i => 0)    (* all registers are zero *)
              []             (* the stack is empty *)
              []             (* nothing has been read yet *)
              "The CIS505/705 Calculator is ready!"
