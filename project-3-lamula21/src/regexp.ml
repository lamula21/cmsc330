open List
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

let rec elem x a =
  match a with
  | h::t -> (h = x) || (elem x t)
  | [] -> false

let rec insert x a =
  if not (elem x a) then x::a else a

let insert_all xs a =
  List.fold_right insert xs a

let count_occ lst target = 
  List.fold_left (fun acc e -> if e = target then acc+1 else acc) 0 lst

let unique_list lst =
  List.fold_left (fun acc e -> if (count_occ acc e) > 0 then acc else e::acc) [] (lst)

let rec union a b =
  match a with
  | h::t -> insert h (union t b)
  | [] ->
    (match b with
      | h::t -> insert h (union [] t)
      | [] -> [])

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  match regexp with
  | Empty_String ->
    let fresh_state = fresh () in
    {
      sigma = [];
      qs = [fresh_state];
      q0 = fresh_state;
      fs = [fresh_state];
      delta = []
    }
  | Char c ->
    let q0 = fresh () in
    let q1 = fresh () in
    {
      sigma = [c];
      qs = [q0; q1];
      q0 = q0;
      fs = [q1];
      delta = [(q0, Some c, q1)]
    }
  |Union (r1,r2) -> 
    let nfa1 = regexp_to_nfa r1 in 
    let nfa2 = regexp_to_nfa r2 in 
    let fresh_initial_state = fresh () in
    let fresh_final_state = fresh () in
    let epsilons final_states other_states = List.map (fun e -> (e, None, other_states)) final_states in
    {
    sigma = union nfa1.sigma nfa2.sigma;
    qs = union nfa1.qs nfa2.qs @ [fresh_initial_state; fresh_final_state];
    q0 = fresh_initial_state;
    fs = [fresh_final_state];
    delta = unique_list (union nfa1.delta nfa2.delta @ epsilons nfa2.fs fresh_final_state
    @ epsilons nfa1.fs fresh_final_state @
      [(fresh_initial_state, None, nfa1.q0)] @ [(fresh_initial_state, None, nfa2.q0)])
    }
  
  |Concat (r1,r2) -> 
    let nfa1 = regexp_to_nfa r1 in 
    let nfa2 = regexp_to_nfa r2 in 
    let epsilons final_states other_states = List.map (fun e -> (e, None, other_states)) final_states in 
    {
      sigma = union nfa1.sigma nfa2.sigma;
      qs = union nfa1.qs nfa2.qs;
      q0 = nfa1.q0;
      fs = nfa2.fs;
      delta = union nfa1.delta nfa2.delta @ epsilons nfa1.fs nfa2.q0;
    }

  | Star r -> 
    let nfa = regexp_to_nfa r in
    let fresh_initial_state = fresh () in
    let fresh_final_state = fresh () in
    let epsilons final_states other_states = List.map (fun e -> (e, None, other_states)) final_states in 
    {
    sigma = nfa.sigma;
    qs = union nfa.qs [fresh_initial_state; fresh_final_state];
    q0 = fresh_initial_state;
    fs = [fresh_final_state];
    delta = unique_list (nfa.delta @ epsilons nfa.fs fresh_final_state @
    [(fresh_initial_state, None, nfa.q0); (fresh_initial_state, None, fresh_final_state); (fresh_final_state, None, fresh_initial_state)]);
  }


(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
