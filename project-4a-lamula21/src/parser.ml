open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None






(* Part 2: Parsing expressions *)
let rec parse_expr toks = 
  let (toks, ast) = parse_Expr toks in 
  if toks = [] then
    (toks, ast)
  else
    raise (InvalidInputException("Not All Tokens Parsed")) 

and parse_Expr toks = 
  match lookahead toks with
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks

and parse_LetExpr toks =
  let nextoks = match_token toks Tok_Let in
  let (toks1, recursion) =
    match lookahead nextoks with
    | Some Tok_Rec -> (match_token nextoks Tok_Rec, true)
    | _ -> (nextoks, false) in

  let (toks2, id) = 
    match lookahead toks1 with
    | Some (Tok_ID id) -> (match_token toks1 (Tok_ID id), id)
    | _ -> raise (InvalidInputException("Expected TOK_ID"))  in 

  let nextoks1 = match_token toks2 Tok_Equal in
  let (toks3, expression3) = parse_Expr nextoks1 in
  let nextoks2 = match_token toks3 Tok_In in
  let (toks4,expression4) = parse_Expr nextoks2 in
  (toks4, Let(id,recursion,expression3,expression4))
   

and parse_FunctionExpr toks = 
  let nextoks = match_token toks Tok_Fun in
  let (toks1, id) = 
    match lookahead nextoks with
    | Some (Tok_ID id) -> (match_token nextoks (Tok_ID id), id)
    | _ -> raise (InvalidInputException("Expected TOK_ID"))  in 
  let nextoks1 = match_token toks1 Tok_Arrow in
  let (toks2,expression2) = parse_Expr nextoks1 in
  (toks2, Fun(id,expression2))

and parse_IfExpr toks =
  let nextoks = match_token toks Tok_If in
  let (toks1,expression1) = parse_Expr nextoks in
  let nextoks1 = match_token toks1 Tok_Then in
  let (toks2,expression2) = parse_Expr nextoks1 in
  let nextoks2 = match_token toks2 Tok_Else in
  let (toks3,expression3) = parse_Expr nextoks2 in
  (toks3, If(expression1,expression2,expression3))

and parse_OrExpr toks =
    let (toks1, expression1) = parse_AndExpr toks in
    match lookahead toks1 with
    | Some Tok_Or -> let nextoks = match_token toks1 Tok_Or in
                    let (toks2,expression2) = parse_OrExpr nextoks in
                    (toks2, Binop(Or,expression1, expression2))

    | _ -> (toks1, expression1)

and parse_AndExpr toks =
    let (toks1, expression1) = parse_EqualityExpr toks in
    match lookahead toks1 with
    | Some Tok_And -> let nextoks = match_token toks1 Tok_And in
                      let (toks2, expression2) = parse_AndExpr nextoks in
                      (toks2, Binop(And, expression1, expression2))
    | _ -> (toks1, expression1)

and parse_EqualityExpr toks = 
  let (toks1, expression1) = parse_RelationalExpr toks in
  match lookahead toks1 with
  | Some Tok_Equal -> let nextoks = match_token toks1 Tok_Equal in
                      let (toks2,expression2) = parse_EqualityExpr nextoks in
                      (toks2, Binop(Equal, expression1, expression2))
  | Some Tok_NotEqual -> let nextoks = match_token toks1 Tok_NotEqual in
                        let (toks2,expression2) = parse_EqualityExpr nextoks in
                        (toks2, Binop(NotEqual, expression1, expression2))
  | _ -> (toks1, expression1)

and parse_RelationalExpr toks =
  let (toks1, expression1) = parse_AdditiveExpr toks in
  match lookahead toks1 with
  | Some Tok_Less -> let nextoks = match_token toks1 Tok_Less in
                      let(toks2,expression2) = parse_RelationalExpr nextoks in
                      (toks2, Binop(Less,expression1, expression2))
  | Some Tok_Greater -> let nextoks = match_token toks1 Tok_Greater in
                      let(toks2,expression2) = parse_RelationalExpr nextoks in
                      (toks2, Binop(Greater,expression1, expression2))
  | Some Tok_LessEqual -> let nextoks = match_token toks1 Tok_LessEqual in
                          let(toks2,expression2) = parse_RelationalExpr nextoks in
                          (toks2, Binop(LessEqual,expression1, expression2))
  | Some Tok_GreaterEqual -> let nextoks = match_token toks1 Tok_GreaterEqual in
                            let(toks2,expression2) = parse_RelationalExpr nextoks in
                            (toks2, Binop(GreaterEqual,expression1, expression2))
  | _ -> (toks1, expression1) 


and parse_AdditiveExpr toks =
  let (toks1, expression1) = parse_MultiplicativeExpr toks in
  match lookahead toks1 with
  | Some Tok_Add -> let nextoks = match_token toks1 Tok_Add in
                    let(toks2,expression2) = parse_AdditiveExpr nextoks in
                    (toks2, Binop(Add, expression1, expression2))
  | Some Tok_Sub -> let nextoks = match_token toks1 Tok_Sub in
                    let(toks2,expression2) = parse_AdditiveExpr nextoks in
                    (toks2, Binop(Sub, expression1, expression2))
  | _ -> (toks1, expression1)


and parse_MultiplicativeExpr toks =
  let (toks1, expression1) = parse_ConcatExpr toks in
  match lookahead toks1 with
  | Some Tok_Mult -> let nextoks = match_token toks1 Tok_Mult in
                let (toks2,expression2) = parse_MultiplicativeExpr nextoks in
                (toks2, Binop(Mult, expression1, expression2))
  | Some Tok_Div -> let nextoks = match_token toks1 Tok_Div in
                let (toks2,expression2) = parse_MultiplicativeExpr nextoks in
                (toks2, Binop(Div, expression1, expression2))
  | _ -> (toks1, expression1)



and parse_ConcatExpr toks =
  let (toks1, expression1) = parse_UnaryExpr toks in
  match lookahead toks1 with
  | Some Tok_Concat -> let tok = match_token toks1 Tok_Concat in
                  let (tok2,expression2) = parse_ConcatExpr tok in
                  (tok2, Binop(Concat,expression1,expression2))
  | _ -> (toks1, expression1)


and parse_UnaryExpr toks =
  
  match lookahead toks with
  | Some Tok_Not -> let tok = match_token toks Tok_Not in
                let (toks1,expression1) = parse_UnaryExpr tok in
                (toks1, Not(expression1))
  | _ -> let (toks1, expression1) = parse_FunctionCallExpr toks in
        (toks1, expression1)
          


and parse_FunctionCallExpr toks =
  let (toks1, expression1) = parse_PrimaryExpr toks in
  
  match lookahead toks1 with
  | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen) -> 
                let (toks2, expression2) = parse_PrimaryExpr toks1 in
                  (toks2, FunctionCall (expression1, expression2))
  | _ -> (toks1, expression1)


and parse_PrimaryExpr toks =
  match lookahead toks with
  | Some Tok_Int i -> let toks1 = match_token toks (Tok_Int i) in
                (toks1, Value(Int(i)))

  | Some Tok_Bool b -> let toks1 = match_token toks (Tok_Bool b) in
                    (toks1, Value(Bool(b)))
                    
  | Some Tok_String s -> let toks1 = match_token toks (Tok_String s) in
                    (toks1, Value(String(s)))

  | Some Tok_ID x -> let toks1 = match_token toks (Tok_ID x) in
                (toks1, ID(x))

  | Some Tok_LParen -> let toks1 = match_token toks (Tok_LParen) in
                  let (toks2,expression2) = parse_Expr toks1 in
                  let toks3 = match_token toks2 (Tok_RParen)
                  in 
                  (toks3, expression2)
                  
  | _ -> raise (InvalidInputException("Wrong Token Found/Syntax Error"))



let rec delete_last_element lst =
  match List.rev lst with
  | [] -> []
  | _ :: tl -> List.rev tl

(* Part 3: Parsing mutop *)
let rec parse_mutop toks = 
  let (toks, ast) = parse_Mutop toks in 
  if toks = [] then
    (toks, ast)
  else
    raise (InvalidInputException("Failed MUTOP | Not All Tokens Parsed"))

and parse_Mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> parse_DefMutop toks
  | Some Tok_DoubleSemi -> ([], NoOp)
  | _ -> let new_toks = delete_last_element toks in
          let (toks1,expression1) = parse_expr new_toks in
          (toks1, Expr(expression1))

and parse_DefMutop toks = 
  let nextoks = match_token toks Tok_Def in
  let (toks1, id) = 
  match lookahead nextoks with
  | Some (Tok_ID id) -> (match_token nextoks (Tok_ID id), id)
  | _ -> raise (InvalidInputException("Expected TOK_ID"))  in 
 let nextoks1 = match_token toks1 Tok_Equal in
 let new_toks = delete_last_element nextoks1 in
 let (toks2,expression2) = parse_expr new_toks in
 (toks2, Def(id,expression2))
