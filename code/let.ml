(* Expression - begin *)
type expr =
  | Id    of string
  | Const of int
  | Add   of expr * expr
  | Sub   of expr * expr
  | If    of bool * expr * expr
  | Let   of string * expr * expr
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
  | NonEmptyEnv((vname, value), env') ->
    if x = vname then value
    else (apply x env')
(* Environment - end *)















(* Interpreter *)
let rec eval e env =
  match e with
  | Id(vname) -> apply vname env
  | Const(c) -> c
  | Add(e1, e2) ->
      let i1 = (eval e1 env) and i2 = (eval e2 env)
      in
      i1 + i2
  | Sub(e1, e2) ->
      let i1 = (eval e1 env) and i2 = (eval e2 env)
      in
      i1 - i2
  | If(tf, e1, e2) -> if tf then (eval e1 env) else (eval e2 env)
  | Let(vname, e1, e2) ->
      let env' = (addBinding vname (eval e1 env) env)
      in
      (eval e2 env')

































(* test cases *)

(*
let x = 20
in
  let y = 10
    in
      let sum1 = x + y
      in
        let z = 5
        in
          sum2 = sum1 - z
*)























let letx =
  let x = "x"
  and i20 = Const(20)
  and lety =
    let y = "y"
    and i10 = Const(10)
    and letsum1 =
      let idsum1 = "sum1"
      and sum1 =
        let x = Id("x")
        and y = Id("y")
        in
        Add(x, y)
      and letz =
        let z = "z"
        and i5 = Const(5)
        and sum2 =
          let sum1 = Id("sum1")
          and idz = Id("z")
          in
          Sub(sum1, idz)
        in
        Let(z, i5, sum2)
      in
      Let(idsum1, sum1, letz)
    in
    Let(y, i10, letsum1)
  in
  Let(x, i20, lety)

let ans1 = (eval letx (emptyEnv()))






















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
  and const7_1 = Const(7)
  and lety1 = 
    let vary1    = "y"
    and const2_1 = Const(2)
    and lety2    =
      let vary2 = "y"
      and letx2 =
        let varx2 = "x"
        and sub1  =
          let varx3    = Id("x")
          and const1_1 = Const(1)
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
          and const8_1 = Const(8)
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

let _ = let c = (eval letx_1 (emptyEnv())) in (print_int c); print_newline()
