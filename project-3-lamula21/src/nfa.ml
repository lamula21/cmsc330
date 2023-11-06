open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []
  
let count_occ lst target = 
  List.fold_left (fun acc e -> if e = target then acc+1 else acc) 0 lst

let unique_list lst =
  List.fold_left (fun acc e -> if (count_occ acc e) > 0 then acc else e::acc) [] (lst)



(****************)
(* Part 1: NFAs *)
(****************)
let rec move_rec delta_list states val_check =
  match delta_list with
    [] -> []
  | (x, y, z)::t ->
     if (List.length (intersection [x] states) > 0) then
          if val_check = y then
            z::(move_rec t states val_check)
          else
            (move_rec t states val_check)
     else
       (move_rec t states val_check)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
    move_rec nfa.delta qs s

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =  
  (*
    qs = [0;1]
    let acc = []
    Loop nfa.qs times
      result = do move nfa qs None
      acc.push(result)
      qs = result
    => list of all reachable states by doing e-transitions

    return unique_list acc@qs  //unique reachable states by doing e-transitions
  *)
  let acc = 
    let rec recursive qs_list output = 
      match qs_list with
      | [] -> []
      | h::t -> 
        let result = move nfa output None in
        (recursive t result)@result
      in 
      recursive nfa.qs qs
  in 
  unique_list (acc@qs)


(* Helper function to traverse the nfa *)




(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  (* 
    qs = given list of states 
    let list = []
    for each in nfa.sigma
      reached = move qs (Some each)
      result = e_closure nfa reached // result is a list
      arr.push(result)
  *)
  let initial_qs = e_closure nfa qs in

  List.fold_left 
  ( fun acc e  -> acc@[e_closure nfa (move nfa initial_qs (Some e))] ) 
  [] 
  nfa.sigma



let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  (*
    qs = states = [0;1]
    nfa.sigma = [a;b;c]
    new_states = [ [1] ; [0] ; [2] ]
    output = [ ([0;1],Some 'a',[1]) ; ([0;1],Some 'b',[0]) ; ([0;1],Some 'c',[2]) ]
  *)
  let new_states = new_states nfa qs in

  let rec output sigma states =
    match sigma, states with
    | [] , [] -> []
    | [], _ | _, [] -> failwith "sigma and new_states should have the same length"
    | h1::t1, h2::t2 ->  (qs, Some h1, h2)::output t1 t2
  in 
  output nfa.sigma new_states


let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (List.length (intersection qs nfa.fs) > 0) then [qs] else []
 

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
    match work with
    | [] -> dfa
    | marked::unmarked ->
    if subset [marked] (unique_list dfa.qs) then
      nfa_to_dfa_step nfa dfa unmarked
    else 
      let new_states = new_states nfa marked in
      let new_transitions = new_trans nfa marked in
      let new_finals = new_finals nfa marked in
      let updated_dfa = {   sigma = dfa.sigma;
                            q0 = dfa.q0;
                            qs = List.append dfa.qs [marked];
                            delta = List.append dfa.delta new_transitions;
                            fs = List.append dfa.fs new_finals } in
      let updated_worklist = insert_all unmarked new_states in
      nfa_to_dfa_step nfa updated_dfa updated_worklist

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let start = e_closure nfa [nfa.q0] in
  let start_worklist = [start] in
  let dfa = { sigma = nfa.sigma; qs = []; q0 = start; fs = []; delta = [] } in
  nfa_to_dfa_step nfa dfa start_worklist


let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  let dfa = nfa_to_dfa nfa in
  let rec accept_helper current alphabetList  = 
    match alphabetList with
    | [] -> List.mem current dfa.fs
    | h::t -> 
      if List.mem h dfa.sigma = true then 
        let reached_states = move dfa [current] (Some h) in
          match reached_states with 
          | [] -> false
          | h2::t2 -> accept_helper h2 t  
      else 
        false
  
  in 
  accept_helper dfa.q0 (explode s)  

