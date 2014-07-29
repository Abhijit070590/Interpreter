(* Type checking *)
type exprType =
    Int
  | Bool
  | Fun of exprType * exprType

type env =
    EmptyEnv
  | NonEmptyEnv of (string * exprType) * env

(* Expression - begin *)
type expr =
  | Id          of string
  | IntConst    of int
  | Add         of expr   * expr
  | Mul         of expr   * expr
  | Sub         of expr   * expr
  | If          of expr   * expr * expr
  | Let         of string * exprType * expr * expr
  | BoolConst   of bool
  | Not         of expr
  | Or          of expr   * expr
  | And         of expr   * expr
  | Equals      of expr   * expr
  | FunDef      of string * exprType * exprType * expr
  | FunApp      of expr   * expr
  | RecFunDef   of string * exprType * exprType * expr
  | Closure     of string * expr * env
  | RecClosure  of string * expr * env
(* Expression - end *)

let rec string_of_type t =
  match t with
    Int         -> "Int"
  | Bool        -> "Bool"
  | Fun(t1, t2) -> (string_of_type t1) ^ " -> " ^ (string_of_type t2)

exception TypeError of string

(* Environment interface - begin *)
let emptyEnv () = EmptyEnv

let addBinding x t env =
  NonEmptyEnv((x, t), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, t), env') ->
    if x = vname then t
    else              (apply x env')
(* Environment interface - end *)


(* Interpreter - begin *)
let rec teval e tenv =
  match e with
  | Id(vname)           -> (apply vname tenv)
  | IntConst(_)         -> Int
  | BoolConst(_)        -> Bool
  | Add(e1, e2)
  | Sub(e1, e2)
  | Mul(e1, e2)         ->
      let t1 = (teval e1 tenv) and t2 = (teval e2 tenv)
      in
      (
        match (t1, t2) with
          (Int, Int) -> Int
        | _          -> raise (
              TypeError
                ("Binary arithmetic expects (Int, Int), but was provide with ("
                  ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ")")
            )
      )
  | If(b, e1, e2)       ->
    if (teval b tenv) = Bool then
      let t1 = (teval e1 tenv) and t2 = (teval e2 tenv)
      in
        if t1 = t2 then t1
        else raise (TypeError "If requires the types of both its branches to be the same.")
    else
      raise (TypeError "If requires the condition expression to be of type Bool")
  | Let(vname, t, e1, e2)  ->
      let t1 = (teval e1 tenv)
      in
      if t = t1 then
        let tenv' = (addBinding vname t tenv)
        in
        (teval e2 tenv')
      else raise (TypeError "Let: The declared type of the variable doesn't match that of the expression it is being bound to.")
  | Not(e)              ->
      if (teval e tenv) = Bool then Bool
      else raise (TypeError "Not expects an expression of type Bool.")
  | Or(e1, e2)
  | And(e1, e2)         ->
       let t1 = (teval e1 tenv) and t2 = (teval e2 tenv)
      in
      (
        match (t1, t2) with
          (Bool, Bool) -> Bool
        | _          -> raise (
              TypeError
                ("Binary boolean operator expects (Bool, Bool), but was provide with ("
              ^ (string_of_type t1) ^ ", " ^ (string_of_type t2))
            )
      )
  | Equals(e1, e2)      ->
      let t1 = (teval e1 tenv) and t2 = (teval e2 tenv)
      in
      (
        match(t1, t2) with
          (Bool, Bool)
        | (Int, Int)   -> Bool
        | _                            ->
            raise (TypeError "Equals operator: both e1 and e2 should evaluate to expressions of the same type")
      )
  | FunDef(par, tpar, tret, body) ->
      let tenv' = (addBinding par tpar tenv)
      in
        let tbody = (teval body tenv')
        in
        if tbody = tret then
          Fun(tpar, tbody)
        else
          raise (TypeError "The declared return type of the function must match the evaluated type of its body.")
  | RecFunDef(par, t, tret, body) ->
      raise (TypeError "RecFunDef not supported as of now.")
  | FunApp(f, arg)      ->
      let tf = (teval f tenv) and targ = (teval arg tenv)
      in
      (
        match tf with
          Fun(tpar, tbody) ->
            if(tpar <> targ) then
              raise (TypeError "FunApp requires the parameter type to be the same as the argument type.")
            else
              targ
        | _ -> raise (TypeError "FunApp needs the operator to evaluate to a function.")
      )
  | Closure(_, _, _) 
  | RecClosure(_, _, _) -> raise (TypeError "Closures do not occur in the syntax of this programming language.")
(* Interpreter - end *)

(* test cases *)
(*
  fun x : int -> 2 * x
*)
let f1 =
  FunDef(
    "x",
    Int,
    Int,
    Mul(
      IntConst(2),
      Id("x")
    )    
  )

let tf1 = (string_of_type (teval f1 (emptyEnv())))

(*
  fun x : int -> if x = 0 then true else false
*)
let f2 =
  FunDef(
    "x",
    Int,
    Bool,
    If(
      Equals(
        Id("x"),
        IntConst(0)
      ),
      BoolConst(true),
      BoolConst(false)
    ) 
  )

let tf2 = (string_of_type (teval f2 (emptyEnv())))


(*
(*
let rec fact n =
  if n = 0 then 1
  else n * (fact (n - 1))
in
fact 4
*)

let e1 =
  Let(
    "fact",
    RecFunDef(
      "n",
      If(
        Equals(
          Id("n"),
          IntConst(1)
        ),
        IntConst(1),
        Mul(
          Id("n"),
          FunApp(
            Id("fact"),
            Sub(
              Id("n"),
              IntConst(1)
            )
          )
        )
      )
    ),
    FunApp(
      Id("fact"),
      IntConst(6)
    )
  )

let ans1 = (string_of_type (teval e1 (emptyEnv())))
(*
let rec fib n =
  if n = 0 || n = 1 then 1
  else (fib (n - 1)) + (fib (n - 2))
in
fib 6
*)

let e2 =
  Let(
    "fib",
    RecFunDef(
      "n",
      If(
        Or(
          Equals(
            Id("n"),
            IntConst(1)
          ),
          
          Equals(
            Id("n"),
            IntConst(0)
          )
        ),
        IntConst(1),
        Add(
          FunApp(
            Id("fib"),
            Sub(
              Id("n"),
              IntConst(1)
            )
          ),
          FunApp(
            Id("fib"),
            Sub(
              Id("n"),
              IntConst(2)
            )
          )
        )
      )
    ),
    FunApp(
      Id("fib"),
      IntConst(10)
    )
  )

let ans2 = (string_of_type (teval e2 (emptyEnv())))
*)
