type env =
    EmptyEnv
  | NonEmptyEnv of (string * int) * env

let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), tailenv) ->
    if x = vname then value
    else apply x tailenv
