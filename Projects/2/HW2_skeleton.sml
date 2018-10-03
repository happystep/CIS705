(*
   The Interpret function assumes a "parse" function, 
      written in another file.
*)

(* ABSTRACT SYNTAX

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

*)

(* EXCEPTIONS *)

exception IntNotDeclared of string
exception ArrayNotDeclared of string
(* *** ADD A FEW MORE *)

(* ENVIRONMENTS and STORES *)

type Loc = int    (* locations in stores *)

(* Global Environment For Integer Identifiers *)

type IntEnv = (Id * Loc) list  (* associates identifiers with locations *)

(* IEnvInsert: Id -> Loc -> IntEnv *)
fun IEnvInsert id loc ienv = (id,loc) :: ienv

(* IEnvLookup: IntEnv -> Id -> Loc *)
fun IEnvLookup [] x = raise (IntNotDeclared x)
|   IEnvLookup ((y,loc)::ienv) x =
      if x = y then loc
      else IEnvLookup ienv x

(* Global Environment For Array Identifiers *)

type ArrayEnv = Id * (int -> Loc) list  
    (* each array identifier is associated with a 
         mapping from indices to locations *)

(* AEnvInsert: Id -> (int -> Loc) -> ArrayEnv *)
fun AEnvInsert id locs aenv = (id,locs) :: aenv

(* AEnvLookup: ArrayEnv -> Id -> int -> Loc *)
fun AEnvLookup [] x _ = raise (ArrayNotDeclared x)
|   AEnvLookup ((y,locs)::aenv) x k =
      if x = y then locs k
      else AEnvLookup aenv x k

(* Stores *)

type Store = (Id * Loc) list  

(* StoLookup: Store -> Loc -> Num *)
fun StoLookup [] _ = 0   (* all locations are initially zero *)
|   StoLookup ((loc1,v1)::sto1) loc = 
         if loc = loc1 then v1 else StoLookup sto1 loc

(* StoUpdate: Loc -> Num -> Store *)
fun StoUpdate loc v sto = (loc,v) :: sto

(* EVALUATION OF EXPRESSIONS
     ExpEval: Exp -> Envs -> Store -> Num 
*)

fun ExpEval (NumE n) _ _ = 27 (* *** MODIFY *)
|   ExpEval (IdE id) (ienv,_) sto = 28 (* *** MODIFY *)
|   ExpEval (ArrE(id,exp)) (envs as (_,aenv)) sto = 29 (* *** MODIFY *)
|   ExpEval (AddE(exp1,exp2)) envs sto =
      let val v1 = ExpEval exp1 envs sto
          val v2 = ExpEval exp2 envs sto
       in v1 + v2
      end
|   ExpEval (SubE(exp1,exp2)) envs sto =
      let val v1 = ExpEval exp1 envs sto
          val v2 = ExpEval exp2 envs sto
       in v1 - v2
      end
|   ExpEval (MulE(exp1,exp2)) envs sto =
      let val v1 = ExpEval exp1 envs sto
          val v2 = ExpEval exp2 envs sto
       in v1 * v2
      end

(* PROCESSING OF DECLARATIONS 
     DeclExec: Decl -> (IntEnv * ArrayEnv * int) -> (IntEnv * ArrayEnv * int)
*)

fun DeclExec (IntD id) (ienv, aenv, next) =
      (IEnvInsert id next ienv, aenv, next+1)
|   DeclExec (ArrayD(id,n)) (ienv, aenv, next) =
      (ienv,
       aenv, (* *** MODIFY *)
       next + n)
|   DeclExec (SeqD(decl1,decl2)) envs =
        DeclExec decl2 (DeclExec decl1 envs)
|   DeclExec NoDecl envs = envs

(* EXECUTION OF COMMANDS *)

type InputStream = Num list
type OutputStream = Num list
type RunTimeState = Store * InputStream * OutputStream

(*
CommExec: Comm -> (IntEnv * ArrayEnv) -> RunTimeState -> RunTimeState
*)

fun CommExec SkipC envs state = state
|   CommExec (SeqC(cmd1,cmd2)) envs state = (* *** MODIFY *)
          CommExec cmd1 envs state
|   CommExec (IfC(exp,cmd1,cmd2)) envs (state as (sto,_,_)) = (* *** MODIFY *)
          CommExec cmd2 envs state
|   CommExec (WhileC(exp,cmd)) envs state = (* *** MODIFY *)
          CommExec cmd envs state
|   CommExec (AssignC(id,exp)) (envs as (ienv,_)) (sto, inp, outp) =
      let val v = 47 (* *** MODIFY *)
          val loc = 48 (* *** MODIFY *)
       in (StoUpdate loc v sto, inp, outp)
      end
|   CommExec (AAssignC(id,exp1,exp2)) (envs as (_,aenv)) (sto, inp, outp) =
      let val v = 49 (* *** MODIFY *)
          val k = ExpEval exp1 envs sto
          val loc = 50 (* *** MODIFY *)
       in (StoUpdate loc v sto, inp, outp)
      end
|   CommExec (OutputC exp) envs (sto,inp,outp) =
          (sto, inp, (51 :: outp))  (* *** MODIFY *)
                    (* we eventually reverse the order *)
|   CommExec (InputC id) (ienv,_) (sto,inp,outp) = (* *** MODIFY *)
          (StoUpdate 7 (hd inp) sto, (tl inp), outp)

(* RUNNING THE PROGRAM *)

fun ProgRun (ProgP(decl,comm)) inp =
       let val (ienv,aenv,_) = DeclExec decl ([],[],0)
           val (_,_,outp) = CommExec comm (ienv,aenv) ([], inp, [])
         in rev outp
        end
|   ProgRun(ErrorP s) _ = (print ("*** syntax error: "^s^"\n"); [0])

fun Interpret prog inp = ProgRun (parse prog) inp
      handle 
        (IntNotDeclared x) =>
            (print ("*** error: "^x^" used as integer but is not declared\n"); [0])
      | (ArrayNotDeclared x) =>
            (print ("*** error: "^x^" used as array but is not declared\n"); [0])
  (* *** ADD A FEW MORE *)
