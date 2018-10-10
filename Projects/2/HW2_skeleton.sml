(* 
  * CIS 705 - Programming Languages
  * Homework Assignment 2
  * ORIGINAL SKELETON BY: Dr. Torben Amtoft
  * MODIFIED BY: Luis Bobadilla
  *)

(*
   The Interpret function assumes a "parse" function, 
      written in another file.
*)
use "HW2_parser.sml";

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

exception InputExhausted

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

fun ExpEval (NumE n) _ _ = n (* *** MODIFY just return the number as stated above*)
|   ExpEval (IdE id) (ienv,_) sto = 
      let val loc = IEnvLookup ienv id
      in StoLookup sto loc 
      end (* *** MODIFY think about looking up the environment to find the store to find value*)
|   ExpEval (ArrE(id,exp)) (envs as (_,aenv)) sto = (* *** MODIFY this one will be the tricky one *)
      let val loc = AEnvLookup aenv id (ExpEval exp envs sto)
      in StoLookup sto loc
      end
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
       AEnvInsert id (fn x => x + next) aenv, (* *** MODIFY ---I'm not sure what n is, does it map (int -> Loc)???? That is the second parameter of the fn()  *)
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
|   CommExec (SeqC(cmd1,cmd2)) envs state = (* *** MODIFY ied *)
      let val state1 = CommExec cmd1 envs state
      in CommExec cmd2 envs state1 
      end
|   CommExec (IfC(exp,cmd1,cmd2)) envs (state as (sto,_,_)) = (* *** MODIFY ied *)
          let val v = ExpEval exp envs sto 
          in if v > 0 then CommExec cmd1 envs state
          else CommExec cmd2 envs state
          end
|   CommExec (WhileC(exp,cmd)) envs state = (* *** MODIFY ied *)
      let val v = ExpEval exp envs (#1(state))
      in if v > 0 then CommExec(SeqC(cmd, WhileC(exp,cmd))) envs state
      else state
      end
|   CommExec (AssignC(id,exp)) (envs as (ienv,_)) (sto, inp, outp) =
      let val loc = IEnvLookup ienv id (* *** MODIFY ied*)
          val v = ExpEval exp envs sto (* *** MODIFY ied*)
       in ((StoUpdate loc v sto), inp, outp)
      end
|   CommExec (AAssignC(id,exp1,exp2)) (envs as (_,aenv)) (sto, inp, outp) =
      let val v = ExpEval exp2 envs sto (* *** MODIFY this is my guess, because isn't exp2 what we want to put in that location?? after we eval *)
          val k = ExpEval exp1 envs sto
          val loc = AEnvLookup aenv id k (* *** MODIFY AEnvLookup: ArrayEnv -> Id -> int -> Loc *)
       in (StoUpdate loc v sto, inp, outp)
      end
|   CommExec (OutputC exp) envs (sto,inp,outp) =
      let val v = ExpEval exp envs sto
      in (sto, inp, (v :: outp))  (* *** MODIF ied *)
      end
                  (* we eventually reverse the order *)
|   CommExec (InputC id) (ienv,_) (sto,inp,outp) = (* *** MODIFY ied *)
      let val env = IEnvLookup ienv id 
      in
          ((StoUpdate env (hd inp) sto), (tl inp), outp)
      end
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
