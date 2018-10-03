(*

Language grammar

P : Program
C : Command
E : Expression
D : Declaration
N : Numeral
I : Identifier

P ::= D C

C ::=  skip |  C1 ; C2  |  if E { C1 } { C2 }  |  while E { C }  
   |  I = E   |  I [ E ] = E  | output E  |  input I

E ::=  N  |  I  |  I [ E ]  |  (E1 + E2)  |  (E1 * E2)  |  (E1 - E2)

D ::= int I; D2
   |  array I N ; D2 
   |  <nothing>

N ::=  strings of digits

I ::=  strings of letters or digits, starting with a letter, 
           not including keywords

*)

(* --- type declarations, for forming operator trees (abstract syntax) --- *)

type Num = int

type Id = string

datatype Exp =
  NumE of Num
| IdE of Id
| ArrE of Id * Exp
| AddE of Exp * Exp
| SubE of Exp * Exp
| MulE of Exp * Exp

datatype Comm =
  SkipC
| SeqC of Comm * Comm
| IfC of Exp * Comm * Comm
| WhileC of Exp * Comm
| AssignC of Id * Exp
| AAssignC of Id * Exp * Exp
| OutputC of Exp
| InputC of Id

datatype Decl = 
  IntD of Id
| ArrayD of Id * Num
| SeqD of Decl * Decl
| NoDecl

datatype Prog = 
  ProgP of Decl * Comm
| ErrorP of string   (* to report errors *)

(* ============= scanner ================= *)
(* the scanner converts the input into a list of "tokens" *)

datatype Token = 
   SkipT
 | IfT
 | WhileT
 | OutputT
 | InputT
 | IntT
 | ArrayT
 | SemicT
 | EqualT
 | PlusT
 | MinusT
 | TimesT
 | LparenT
 | RparenT
 | LsquareT
 | RsquareT
 | LcurlyT
 | RcurlyT
 | IdT of string
 | NumT of int

(* --- auxiliary functions --- *)

fun print_token token = case token of
   SkipT => "skip"
 | IfT => "if"
 | WhileT => "while"
 | OutputT => "output"
 | InputT => "input"
 | IntT => "int"
 | ArrayT => "array"
 | SemicT => ";"
 | EqualT => "="
 | PlusT => "+"
 | MinusT => "-"
 | TimesT => "*"
 | LparenT => "("
 | RparenT => ")"
 | LsquareT => "["
 | RsquareT => "]"
 | LcurlyT => "{"
 | RcurlyT => "}"
 | (IdT s) => ("identifier "^s)
 | (NumT n) => "number"

fun is_digit(ch) = ord(ch) >= ord(#"0") andalso ord(ch) <= ord(#"9")

fun char2digit(ch) = ord(ch) - ord(#"0")

fun is_letter(ch) = 
      (ord(ch) >= ord(#"a") andalso ord(ch) <= ord(#"z"))
         orelse
      (ord(ch) >= ord(#"A") andalso ord(ch) <= ord(#"Z"))

(* scanNum: char list -> (int * char list) *)
fun scanNum(inp) = 
      let fun scan([],acc) = (acc,[])
          |   scan(c::cs,acc) =
                if is_digit(c)
                then scan(cs,10 * acc + char2digit(c))
                else (acc,c::cs)
       in scan(inp,0)
      end

(* scanId: char list -> (string * char list) *)
fun scanId(inp) =
      let fun scan([],acc) = (acc,[]) 
          |   scan(cs as c::cs', acc) = 
                if is_letter(c) orelse is_digit(c)
                then scan(cs', acc^(implode [c]))
                else (acc,cs)
       in scan(inp,"") 
      end

(* scan: char list -> Token list *)
fun scan [] = []
|   scan (cs as (c::cs1)) =
      if is_digit(c)
      then let val (n,cs2) = scanNum(cs) 
            in (NumT n) :: (scan cs2) 
           end  
      else if c = #";"
           then SemicT :: (scan cs1)
      else if c = #"="
           then EqualT :: (scan cs1)
      else if c = #"+"
           then PlusT :: (scan cs1)
      else if c = #"-"
           then MinusT :: (scan cs1)
      else if c = #"*"
           then TimesT :: (scan cs1)
      else if c = #"("
           then LparenT :: (scan cs1)
      else if c = #")"
           then RparenT :: (scan cs1)
      else if c = #"["
           then LsquareT :: (scan cs1)
      else if c = #"]"
           then RsquareT :: (scan cs1)
      else if c = #"{"
           then LcurlyT :: (scan cs1)
      else if c = #"}"
           then RcurlyT :: (scan cs1)
      else if is_letter(c)
      then let val (s,cs2) = scanId(cs)
            in if s = "skip"
               then SkipT :: (scan cs2)
               else if s = "if"
               then IfT :: (scan cs2)
               else if s = "while"
               then WhileT :: (scan cs2)
               else if s = "output"
               then OutputT :: (scan cs2)
               else if s = "input"
               then InputT :: (scan cs2)
               else if s = "int"
               then IntT :: (scan cs2)
               else if s = "array"
               then ArrayT :: (scan cs2)
               else (IdT s) :: (scan cs2)
           end
      else scan cs1

(* ============= parser =========== *)

exception SyntaxError of string

(* expectToken(token,token::tokens) = tokens   *)
fun expectToken(token,[]) = 
      raise (SyntaxError ((print_token token)^" expected"))
|   expectToken(token,token1::tokens) =
      if token = token1
      then tokens 
      else raise (SyntaxError  
        ((print_token token)^" expected but "^(print_token token1)^" seen"))

fun getIdT((IdT s) :: tokens) = (s,tokens)
|   getIdT([]) = 
      raise (SyntaxError "identifier expected")
|   getIdT(token :: tokens) = raise (SyntaxError 
       ("identifier expected but "^(print_token token)^" seen"))

fun getNumT((NumT n) :: tokens) = (n,tokens)
|   getNumT([]) = 
      raise (SyntaxError "number expected")
|   getNumT(token :: tokens) = raise (SyntaxError 
       ("number expected but "^(print_token token)^" seen"))

(* parseExp: tokens -> Exp * tokens   *)
fun parseExp([]) = raise (SyntaxError "expression expected")
|   parseExp((NumT n) :: tokens) = (NumE n,tokens)
|   parseExp((IdT s) :: tokens) = 
      (case tokens of 
        (LsquareT :: tokens1) => 
          (let val (exp1,tokens2) = parseExp(tokens1)
            in (ArrE(s,exp1),expectToken(RsquareT,tokens2))
           end)
      | _ => (IdE s,tokens))
|   parseExp(LparenT :: tokens) =
      let val (exp1,tokens1) = parseExp(tokens)
       in case tokens1 of
            (PlusT :: tokens0) =>
               (let val (exp2,tokens2) = parseExp(tokens0)
                 in (AddE(exp1,exp2),expectToken(RparenT,tokens2))
                end)
          | (MinusT :: tokens0) =>
               (let val (exp2,tokens2) = parseExp(tokens0)
                 in (SubE(exp1,exp2),expectToken(RparenT,tokens2))
                end)
          | (TimesT :: tokens0) =>
               (let val (exp2,tokens2) = parseExp(tokens0)
                 in (MulE(exp1,exp2),expectToken(RparenT,tokens2))
                end)
          | otherwise => raise (SyntaxError "operator expected")
      end
|   parseExp(token :: tokens) = 
      raise (SyntaxError ((print_token token)^" cannot start an expression"))

(* parse1Comm: tokens -> Comm * tokens       *)
(*  reads a command that is "atomic, i.e.,   *)
(*   not formed by sequential composition    *)
fun parse1Comm([]) = raise (SyntaxError "command expected")
|   parse1Comm(SkipT :: tokens) =
           (SkipC, tokens)
|   parse1Comm(IfT :: tokens) =
      let val (exp,tokens1) = parseExp(tokens)
          val (comm1,tokens2) = parseComm(expectToken(LcurlyT,tokens1))
          val (comm2,tokens3) = parseComm(expectToken(LcurlyT,
                                    expectToken(RcurlyT,tokens2)))
       in (IfC(exp,comm1,comm2),expectToken(RcurlyT,tokens3))
      end
|   parse1Comm(WhileT :: tokens) =
      let val (exp,tokens1) = parseExp(tokens)
          val (comm,tokens2) = parseComm(expectToken(LcurlyT,tokens1))
       in (WhileC(exp,comm),expectToken(RcurlyT,tokens2))
      end
|   parse1Comm(OutputT :: tokens) =
      let val (exp,tokens1) = parseExp(tokens)
       in (OutputC(exp),tokens1)
      end
|   parse1Comm(InputT :: tokens) =
      let val (s,tokens1) = getIdT(tokens)
       in (InputC(s),tokens1)
      end
|   parse1Comm((IdT s) :: tokens) = (case tokens of
       (LsquareT :: tokens1) => (let
           val (exp1,tokens2) = parseExp(tokens1)
           val (exp2,tokens3) = parseExp(expectToken(EqualT,
                                         expectToken(RsquareT,tokens2)))
          in (AAssignC(s,exp1,exp2),tokens3)
         end)
      | _ => (let val (exp,tokens1) = parseExp(expectToken(EqualT,tokens))
                in (AssignC(s,exp),tokens1)
               end))
|   parse1Comm(token :: tokens) = 
      raise (SyntaxError ((print_token token)^" cannot start command"))
(* parseComm: tokens -> Comm * tokens                *)
(*  reads a sequence of one or more atomic  commands *)
and parseComm(tokens) = 
      let val (comm1,tokens1) = parse1Comm(tokens)
       in case tokens1 of
            (SemicT :: tokens2) => 
                  (let val (comm2,tokens3) = parseComm(tokens2)
                    in (SeqC(comm1,comm2),tokens3)
                   end)
          | otherwise => (comm1,tokens1)
      end

(* parseProg: tokens -> Prog * tokens   *)
fun parseProg tokens = case tokens of
      [] => raise (SyntaxError "a program must contain a command")
    | (token :: tokens1) => 
          (if token = IntT
           then let 
               val (s,tokens2) = getIdT(tokens1)
               val (ProgP(decls,comm),tokens3) = 
                      parseProg (expectToken(SemicT,tokens2))
              in (ProgP(SeqD(IntD s,decls),comm),tokens3)
             end
           else if token = ArrayT
           then let 
               val (s,tokens2) = getIdT(tokens1)
               val (n,tokens3) = getNumT(tokens2)
               val (ProgP(decls,comm),tokens4) = 
                      parseProg (expectToken(SemicT,tokens3))
              in (ProgP(SeqD(ArrayD(s,n),decls),comm),tokens4)
             end
           else let val (comm,tokens2) = parseComm tokens
                   in (ProgP(NoDecl,comm),tokens2)
                  end)

(* parse: string -> Prog *)
fun parse inp =
      (let val tokens = scan (explode inp)
           val (prog,tokens1) = parseProg tokens
        in if null tokens1
           then prog
           else raise (SyntaxError "input contains symbols after the program")
      end)
      handle (SyntaxError s) => ErrorP s