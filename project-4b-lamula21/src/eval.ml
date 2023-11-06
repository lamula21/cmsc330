open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(* type values = Int of int|Bool of bool|String of string *)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

(* let extend env x v = (x,v)::env *)

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

(* let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x *)

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
(* let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x); *)


(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  | Value(v) -> v

  | ID(id) -> ref_lookup env id

  | Not(x) -> 
    let v = eval_expr env x in
    (match v with
    | Bool(b) -> Bool(not b)
    | _ -> raise (TypeError("Expected type bool"))) 

  | Binop(op, e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match (op, v1, v2) with
    | (Add, Int(x), Int(y)) -> Int(x + y)
    | (Sub, Int(x), Int(y)) -> Int(x - y)
    | (Mult, Int(x), Int(y)) -> Int(x * y)
    | (Div, Int(x), Int(y)) -> if y = 0 then raise (DivByZeroError) else Int(x / y)
    | (Concat, String(x), String(y)) -> String(x ^ y)
    | (Greater, Int(x), Int(y)) -> Bool(x > y)
    | (Less, Int(x), Int(y)) -> Bool(x < y)
    | (GreaterEqual, Int(x), Int(y)) -> Bool(x >= y)
    | (LessEqual, Int(x), Int(y)) -> Bool(x <= y)
    | (Equal, Int(x), Int(y)) -> if x = y then Bool(true) else Bool(false)
    | (Equal, Bool(x), Bool(y)) -> if x = y then Bool(true) else Bool(false)
    | (Equal, String(x), String(y)) -> if x = y then Bool(true) else Bool(false)
    | (NotEqual, Int(x), Int(y)) -> if x <> y then Bool(true) else Bool(false)
    | (NotEqual, Bool(x), Bool(y)) -> if x <> y then Bool(true) else Bool(false)
    | (NotEqual, String(x), String(y)) -> if x <> y then Bool(true) else Bool(false)
    | (Or, Bool(x), Bool(y)) -> Bool(x || y)
    | (And, Bool(x), Bool(y)) -> Bool(x && y)
    | (_, _, _) -> raise (TypeError ("INVALID BINOP OPERATION, ERROR")))

  | If(e1, e2, e3) -> 
    let v1 = eval_expr env e1 in
    (match v1 with
    | Bool (x) -> if x then eval_expr env e2 else eval_expr env e3
    | _ -> raise (TypeError ("INVALID IF OPERATION, ERROR")))
  
  | Let(s, false, e1, e2) -> 
    let v = eval_expr env e1 in
    let new_env = ref_extend env s v in
    eval_expr new_env e2
  
  | Let(s, true, e1, e2) ->
    let new_env = ref_extend_tmp env s in
    let v = eval_expr new_env e1 in
    ref_update new_env s v;
    let new_env' = new_env in
    eval_expr new_env' e2


  | Fun(s, e) -> (Closure(env, s, e))

  | FunctionCall(e1, e2) ->
    let v1 = eval_expr env e1 in
    (match v1 with
    | Closure(env', x, e) ->
      let v2 = eval_expr env e2 in
      let new_env = ref_extend env' x v2 in
      eval_expr new_env e
    | _ -> raise (TypeError ("INVALID FUNCTION CALL, ERROR")))
  

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  (match m with
  | Def (x, e) ->
    let env' = ref_extend_tmp env x in
    let v = eval_expr env' e in
    ref_update env' x v;
    (env', Some v)
  | Expr e ->
    let v = eval_expr env e in
    (env, Some v)
  | NoOp ->
    (env, None))

