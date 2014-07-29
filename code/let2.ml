(*
  Here we add the following additional features with respect to let.ml:
  - The expr type has several additional constructors:
    . BoolConst
    . Not
    . Or
    . And
    . Equals
  - Further following name changes have been made:
    . Const has been changed to IntConst. The reason is that we no more have only one type of constant, i.e. Int.
       We now also have boolean constants.
  - At this point, we have moved away from the first design of interpreter that used to return the underlying int value
     as the result of expression evaluation. The result of the evaluation now is also an expression, albiet in
     some normal form, viz. IntConst or BoolConst. The reason for making this important design change is that we
     wish to evaluate expressions which don't necessarily evaluate to a single type (e.g. int), but to any of the 
     arbitrarily large number of normal forms our programming language allows. Where required, the underlying value
     of an normal-form expression can be extracted using relevant extractor functions (e.g. getIntConstValue and
     getBoolConstValue).
  - The above change has robbed our abstract syntax specification of its earlier ability to allow all and only the
     'correct' expressions of our PL. By 'correct' we mean, those expressions which can be successfully evaluated.
  - Therefore, we have introduced rudimentary 'type checking'. If an input expression is well-formed as per the abstract
     abstract syntax, and yet is ill-typed, an exception (TypeError) will be raised at an 'appropriate' point.
  - Notice the local changes that have been made in the evaluate function:
    (*----------------------*)
     (* Old *)
     Add(e1, e2) ->
        let i1 = (eval e1 env) and i2 = (eval e2 env)
        in
        i1 + i2

     (* New *)
     Add(e1, e2)        ->
        let e1' = (eval e1 env) and e2' = (eval e2 env)
        in
          let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
          in
          IntConst(i1 + i2)
    (*----------------------*)
     The new implementation necessitates two additional steps:
     . Extract the underlying value using the extractor function, e.g. let i1 = (getIntConstValue e1') etc.
     . Result the result of the evaluation packed in the form of an expression again, e.g. IntConst(i1 + i2)
  - The If constructor has been changed from bool * expr * expr to expr * expr * expr. This is to reflext that we
     now wish to have boolean expressions too which can be evaluated in the same way as int expressions.
  - Study the corresponding change that has been made to the evaluation clause for If constructor.

  - The most important addition to the evaluate function are clauses for Not, Or, And and Equals boolean expressions.
     Note the procedure for evaluating such constructs.
     . Extract the underlying boolean values after evaluating the input expressions.
     . Do the corresponding boolean operation on these underlying values (e.g. && for And, || for Or)
     . return the result packed as a BoolConst.
  - Equals is particularly important, because this enables us to have non-boolean contents within boolean clauses.
     e.g. we can now construct expressions like (x = 3) && (5 = 4) in our language.
*)

(* Expression - begin *)
type expr =
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
(* Expression - end *)

exception TypeError of string

let getIntConstValue e =
  match e with
    IntConst(c) -> c
  | _        -> raise (TypeError "getIntConstValue: The expression is not in IntConst normal form.")
 
let getBoolConstValue e =
  match e with
    BoolConst(b) -> b
  | _            -> raise (TypeError "getBoolConstValue: The expression is not in BoolConst normal form.")
 
(* Environment - begin *)
type env =
    EmptyEnv
  | NonEmptyEnv of (string * expr) * env

let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') ->
    if x = vname then value
    else (apply x env')
(* Environment - end *)

(* Interpreter *)
let rec eval e env =
  match e with
  | Id(vname)          -> apply vname env
  | IntConst(_)        -> e
  | BoolConst(_)       -> e
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

(* test cases *)
(* let x = 7
   in
     let y = 2
     in
       let y =
         let x = (x - 1)
         in
         (x - y)
       in
       (x - 8) - y
 *)

let letx_1 = 
  let varx1    = "x"
  and const7_1 = IntConst(7)
  and lety1 = 
    let vary1    = "y"
    and const2_1 = IntConst(2)
    and lety2    =
      let vary2 = "y"
      and letx2 =
        let varx2 = "x"
        and sub1  =
          let varx3    = Id("x")
          and const1_1 = IntConst(1)
          in
          Sub(varx3, const1_1)
        and sub2  =
          let varx4 = Id("x")
          and vary3 = Id("y")
        in
        Sub(varx4, vary3)
      in
      Let(varx2, sub1, sub2)
      and sub3      =
        let sub4 =
          let varx5    = Id("x")
          and const8_1 = IntConst(8)
        in
        Sub(varx5, const8_1)
        and vary4 = Id("y")
      in
      Sub(sub4, vary4)
    in
    Let(vary2, letx2, sub3)
  in
  Let(vary1, const2_1, lety2)
in
Let(varx1, const7_1, lety1)

(* driver *)
let ans1 = (getIntConstValue (eval letx_1 (emptyEnv())))

(*
  let z = 5
  in
    let x = 3
    in
      let y = x - 1
      in
        let x = 4
        in
        (z - (x - y))
*)
let e2 =
  let letz =
    let z = "z"
    and i5 = IntConst(5)
    and letx =
      let x = "x"
      and i3 = IntConst(3)
      and lety =
        let y = "y"
        and sub1 =
          let idx = Id("x")
          and i1 = IntConst(1)
          in
          Sub(idx, i1)
        and letx =
          let x = "x"
          and i4 = IntConst(4)
          and sub2 =
            let idz = Id("z")
            and sub3 =
              let idx = Id("x")
              and idy = Id("y")
              in
              Sub(idx, idy)
            in
            Sub(idz, sub3)
          in
          Let(x, i4, sub2)
        in
        Let(y, sub1, letx)
      in
      Let(x, i3, lety)
    in
    Let(z, i5, letx)
  in letz

let ans2 = (getIntConstValue (eval e2 (emptyEnv())))
