(* Expressions with identifiers *)
(* 
   Here the environment is pre-populated with bindings. This language allows us to 
    evaluate expressions with variables as long as bindings of that variable exist
    in the environment initialised a priori. The language provides no explicit support
    to add new bindings.
*)

type expr =
  | Id     of string
  | Const of int
  | Add   of expr * expr
  | Sub   of expr * expr
  | If    of bool * expr * expr
(* Expression - end *)

(* Environment - begin *)
type env =
    EmptyEnv
  | NonEmptyEnv of (string * int) * env

let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env : int =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), tailenv) ->
    if x = vname then value
    else (apply x tailenv)
(* Environment - end *)

(* Interpreter *)
(*
   The most important change made in this version of the interpreter is the addition of
   env parameter to the eval function. To lookup the value of a variable occurring in the
   expression, we need to have this environment.
*)
let rec eval e env =
  match e with
  | Id(vname)      -> apply vname env
  | Const(c)       -> c
  | Add(e1, e2)    ->
      let i1 = (eval e1 env) and i2 = (eval e2 env)
      in
      i1 + i2
  | Sub(e1, e2)    ->
      let i1 = (eval e1 env) and i2 = (eval e2 env)
      in
      i1 - i2
  | If(tf, e1, e2) -> if tf then (eval e1 env) else (eval e2 env)

(* test cases *)















(* 
1. Define an environment with predefined bindings for 3 variables as follows:
  {
    x : 20,
    y : 10,
    z :  5
  }
2. Using your interpreter evaluate the values of the following expression under the
    above environment:
   a) x                   -- answer = 20
   b) y                   -- answer = 10
   c) x + y               -- answer = 30
   d) (x + y) - z         -- answer = 25
*)



























let env =
  (addBinding "z" 5
    (addBinding "y" 10
      (addBinding "x" 20
        (emptyEnv ()))))
























let x  = Id("x")
let y  = Id("y")
let z  = Id("z")

let e1 = Add(x, y)
let e2 = Sub(e1, z)

let ans_a = (eval x env)
let ans_b = (eval y env)
let ans_c = eval e1 env
let ans_d = eval e2 env
















