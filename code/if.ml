(* Expressions with if constructs *)
(*
  If expressions in this language have to be booleans in the implementation language (OCaml).
  e.g. true, false, not true, true && false etc. are all native booleans.
  Even 2 = 1, 3 <> 4 || 5 > 3 etc. also native booleans.
*)

(* Expression - begin *)
type expr =
  | Const of int
  | Add   of expr * expr
  | Sub   of expr * expr
  | If    of bool * expr * expr
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
  | If(tf, e1, e2) -> if tf then (eval e1) else (eval e2)

(* test cases *)
let e1 = Const(1)
let e2 = Const(2)
let e3 = Add(e1, e2)
let e4 = Sub(e2, e1)

let if1 = If(true, e1, e2)
let if2 = If((true && false), e3, e4)

let _ =
  let c1 = (eval if1)
  and c2 = (eval if2)
in
(print_string 
    ("c1 = " ^ (string_of_int c1)
  ^ ", "
  ^ ("c2 = " ^ (string_of_int c2)
  ^ "\n")))
