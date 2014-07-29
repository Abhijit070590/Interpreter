(* Expression - begin *)
type env =
    EmptyEnv
  | NonEmptyEnv of (string * expr) * env
    (* Now env is defined recursively with expr as now the value it stores is also an 
       expression and not an integer as was the case so far. *)
and expr =
  | Id        of string
  | IntConst  of int
  | Add       of expr * expr
  | Sub       of expr * expr
  | If        of expr * expr * expr
  | Let       of string * expr * expr
  | BoolConst of bool
  | Not       of expr
  | Or        of expr * expr
  | And       of expr * expr
  | Equals    of expr * expr
  | Closure   of string * expr * env
    (* Corresponds to the underlying function closure. Note that it is a normal form as IntConst and BoolConst. *)
  | FunDef    of string * expr
    (* The abstract syntactic structure. This doesn't have the information about the environment. *)
  | FunApp    of expr * expr
    (* Application of function *)

exception TypeError of string

let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') ->
    if x = vname then value
    else (apply x env')

let getIntConstValue e =
  match e with
    IntConst(c)  -> c
  | _            -> raise (TypeError "getIntConstValue: The expression is not in IntConst normal form.")
 
let getBoolConstValue e =
  match e with
    BoolConst(b) -> b
  | _            -> raise (TypeError "getBoolConstValue: The expression is not in BoolConst normal form.")

(* This is the new addition to our extractor functions. The only difference here is that it returns a tuple,
   as the normal form Closures 3 and not 1 underlying value. *)
let getClosureValue e =
  match e with
    Closure(par, body, env)    -> (par, body, env)
  | _            -> raise (TypeError "getFunDefValue: The expression is not in FunDef normal form.")

(* Expression - end *)

(* Interpreter - begin *)
let rec eval e env =
  match e with
  | Id(vname)          -> apply vname env
  | IntConst(_)        -> e
  | BoolConst(_)       -> e
  | Closure(_, _, _)   -> e
  | Add(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
        in
        IntConst(i1 + i2)
  | Sub(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
        in
        IntConst(i1 - i2)
  | If(b, e1, e2)      -> if (getBoolConstValue (eval b env)) then (eval e1 env) else (eval e2 env)
  | Let(vname, e1, e2) ->
      let env' = (addBinding vname (eval e1 env) env)
      in
      (eval e2 env')
  | Not(e)             ->
      let e' = (eval e env)
      in
        let b' = (getBoolConstValue e')
        in
        BoolConst(not b')
  | Or(e1, e2)         ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
        in
        BoolConst(b1' || b2')
  | And(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
        in
        BoolConst(b1' && b2')
  | Equals(e1, e2)     ->
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
  | FunDef(par, body) -> Closure(par, body, env)
    (* The only thing to evaluate on seeing a function definition is the closure. *)
  | FunApp(f, arg) ->
    (* The function application evaluates the function f and the argument arg in turn and
       applies f on arg. This is done by creating a new environment e'' by binding the parameter par
       to the arg' (the value to which arg evaluates) and adding the same to env', which is the 
       environment in the closure obtained by evaluating f in env. *)
      let e' = (eval f env)
      in
        let (par, body, env') = (getClosureValue e') and arg' = (eval arg env)
        in
          let env'' = (addBinding par arg' env')
          in
          (eval body env'')
  
(* Interpreter - end *)

(* test cases *)
(*
  let x = 200
  in
    let f = fun z -> z - x
    in
      let x = 100
      in
        let g = fun z -> z - x
        in
        ((f 1) - (g 1))
*)
















let e1 =
  let x = "x"
  and i200 = IntConst(200)
  and letf =
    let f = "f"
    and funz =
      let z = "z"
      and sub1 =
        let idz = Id("z")
        and idx = Id("x")
        in
        Sub(idz, idx)
      in
      FunDef(z, sub1)
    and letx =
      let x = "x"
      and i100 = IntConst(100)
      and letg =
        let g = "g"
        and funz =
          let z = "z"
          and sub2 =
            let idz = Id("z")
            and idx = Id("x")
            in
            Sub(idz, idx)
          in
          FunDef(z, sub2)
        and sub3 =
          let faf =
            let idf = Id("f")
            and i1 = IntConst(1)
            in
            FunApp(idf, i1)
          and fag =
            let idg = Id("g")
            and i1 = IntConst(1)
            in
            FunApp(idg, i1)
          in
          Sub(faf, fag)
        in
        Let(g, funz, sub3)
      in
      Let(x, i100, letg)
    in Let(f, funz, letx)
  in
    let letx = Let(x, i200, letf)
    in
    letx

let ans1 = (getIntConstValue (eval e1 (emptyEnv())))

let e2 =
  Let(
    "x",
    IntConst(200),
    Let(
      "f",
      FunDef(
        "z",
        Sub(
          Id("z"),
          Id("x")
        )
      ),
      Let(
        "x",
        IntConst(100),
        Let(
          "g",
          FunDef(
            "z",
            Sub(
              Id("z"),
              Id("x")
            )
          ),
          Sub(
            FunApp(
              Id("f"),
              IntConst(1)
            ),
            FunApp(
              Id("g"),
              IntConst(1)
            )
          )
        )
      )
    )
  )


let ans2 = (getIntConstValue (eval e2 (emptyEnv())))
