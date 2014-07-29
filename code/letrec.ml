(* Recursive functions *)

(*
  Our earlier implementation of the interpreter is not suitable for execution of recursive functions.
  When there is a call to itself from a recursive function f, the environment where it will look for the
  closure for f doesn't have a binding for f. Hence, a Not_found exception will be thrown.
  We get around this problem by enhancing our language and the interpreter in the following:
  - We add two constructs to the abstract syntax, namely, RecFunDef and RecClosure.
*)

(* Expression - begin *)
type env =
    EmptyEnv
  | NonEmptyEnv of (string * expr) * env
and expr =
  | Id          of string
  | IntConst    of int
  | Add         of expr   * expr
  | Mul         of expr   * expr
  | Sub         of expr   * expr
  | If          of expr   * expr * expr
  | Let         of string * expr * expr
  | BoolConst   of bool
  | Not         of expr
  | Or          of expr   * expr
  | And         of expr   * expr
  | Equals      of expr   * expr
  | FunDef      of string * expr
  | FunApp      of expr   * expr
  | RecFunDef   of string * expr
  | Closure     of string * expr * env
  | RecClosure  of string * expr * env
(* Expression - end *)

exception TypeError of string

let getIntConstValue e =
  match e with
    IntConst(c)  -> c
  | _            -> raise (TypeError "getIntConstValue: The expression is not in IntConst normal form.")
 
let getBoolConstValue e =
  match e with
    BoolConst(b) -> b
  | _            -> raise (TypeError "getBoolConstValue: The expression is not in BoolConst normal form.")

let getClosureValue e =
  match e with
    Closure(par, body, env)
  | RecClosure(par, body, env) -> (par, body, env)
  | _            -> raise (TypeError "getFunDefValue: The expression is not in FunDef normal form.")

(* Environment interface - begin *)
let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') ->
    if x = vname then
    (
     (*
        Here is the most crucial design change in the interpreter. The apply function, when matching a 
         RecClosure mustn't return the value as is, because that value is a closure whose environment
         doesn't have a binding to the recursive function (named vname in this function) being called.
         The trick is to return a 
         closure which is the same as the closure to which the searched name is bound, except
         in one point: its environment has an additional binding, that of the searched name (vname) 
         to the value (value).
     *)
      match value with
        | RecClosure(_, _, _) ->
            let (par, body, env'') = getClosureValue(value)
            in
              let env''' = (addBinding vname value env'')
              in
              Closure(par, body, env''')
        | _                   -> value
    )
    else (apply x env')
(* Environment interface - end *)

(* Interpreter - begin *)
let rec eval e env =
  match e with
  | Id(vname)           -> apply vname env
  | IntConst(_)
  | BoolConst(_)
  | Closure(_, _, _) 
  | RecClosure(_, _, _) -> e (* RecClosure is the new normal form added to our language. Thus it evaluates to itself. *)
  | Add(e1, e2)         ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
        in
        IntConst(i1 + i2)
  | Sub(e1, e2)         ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
        in
        IntConst(i1 - i2)
  | Mul(e1, e2)         ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
        in
        IntConst(i1 * i2)
  | If(b, e1, e2)       ->
      if (getBoolConstValue (eval b env)) then
        (eval e1 env)
      else
        (eval e2 env)
  | Let(vname, e1, e2)  ->
      let env' = (addBinding vname (eval e1 env) env)
      in
      (eval e2 env')
  | Not(e)              ->
      let e' = (eval e env)
      in
        let b' = (getBoolConstValue e')
        in
        BoolConst(not b')
  | Or(e1, e2)          ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
        in
        BoolConst(b1' || b2')
  | And(e1, e2)         ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
        in
        BoolConst(b1' && b2')
  | Equals(e1, e2)      ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
      (
        match(e1', e2') with
          (BoolConst(_), BoolConst(_)) ->
            let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
            in
            BoolConst(b1' = b2')
        | (IntConst(_), IntConst(_))   ->
            let i1' = (getIntConstValue e1') and i2' = (getIntConstValue e2')
            in
            BoolConst(i1' = i2')
        | _                            ->
            raise (TypeError "evaluate.Equals: both e1 and e2 should evaluate to expressions of the same type")
      )
  | FunDef(par, body)   -> Closure(par, body, env)
  | FunApp(f, arg)      ->
      let e' = (eval f env)
      in
        let (par, body, env') = (getClosureValue e') and arg' = (eval arg env)
        in
          let env'' = (addBinding par arg' env')
          in
          (eval body env'')
  | RecFunDef(par, body) -> RecClosure(par, body, env)  (* RecFunDef evaluates to a RecClosure. *) 
(* Interpreter - end *)

(* test cases *)
(*
let rec fact n =
  if n = 0 then 1
  else n * (fact (n - 1))
in
fact 6
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

let ans1 = (getIntConstValue (eval e1 (emptyEnv())))
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

let ans2 = (getIntConstValue (eval e2 (emptyEnv())))
