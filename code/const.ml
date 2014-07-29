(* Expressions with only constant values and simple arithmetic expressions *)
(* Expression - begin *)
type expr =
  | Const of int
  | Add   of expr * expr
  | Sub   of expr * expr
(* Expression - end *)

(* Interpreter *)
let rec eval e =
  match e with
  | Const(c) -> c
  | Add(e1, e2) ->
      let i1 = (eval e1) and i2 = (eval e2)
      in
      i1 + i2
  | Sub(e1, e2) ->
      let i1 = (eval e1) and i2 = (eval e2)
      in
      i1 - i2

(* test cases *)
let e1 = Const(1)
let e2 = Const(2)
let e3 = Add(e1, e2)
let e4 = Sub(e2, e1)

let _ =
  let c1 = (eval e3)
  and c2 = (eval e4)
in
(print_string ("c1 = " ^ (string_of_int c1) ^ ", c2 = " ^ (string_of_int c2) ^ "\n"))
