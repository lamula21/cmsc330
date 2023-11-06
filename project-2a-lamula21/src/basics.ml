open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

(* 1. Reverse tuple *)
let rev_tup (a, b, c) = (c, b, a)

(* 2. Is even? 2 -> true *)
let is_even x = 
  if x mod 2 == 0 then 
    true 
  else false

(* 3. Get area from 2coordinates *)
let area (a, b) (x, y) = abs ((y - b) * (x - a))
(* if (y-b)*(x-a) < 0 then (y-b)*(x-a)*(-1) else (y-b)*(x-a);; *)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

(* 4. Fibonnaci sum at term n*)
let rec fibonacci n = 
  if n = 1 || n = 0 then 
    n 
  else fibonacci (n - 1) + fibonacci (n - 2)


(* 5. Power of x^p *)
let rec pow x p = match p with 
| _ when p = 0 -> 1 
| _ -> x * pow x (p - 1)

(* Helper: factorial *)
(* let rec factorial n =
   if n == 0 then 1 else  n * factorial(n-1);; *)

(* Helper: nonDivisible for is_prime *)
let rec nonDivisible num next =
  match next with
  | 1 -> true
  | _ -> num mod next <> 0 && nonDivisible num (next - 1)

(* 6. Is prime? 31 -> true *)
let is_prime x =
  match x with
  | _ when x < 0 -> false
  | 0 -> false
  | 1 -> false
  | _ -> nonDivisible x (x - 1)

(* Helper function *)
let rec kadane_algo listChain =
  match listChain with
  | [] -> 0
  | h::[] -> h
  | h::t -> let a = kadane_algo t in
    if h < a then
      a
    else
      h


(* 7. MaxProcChain *)
let rec maxFuncChain init funcs = match funcs with 
    | [] -> init 
    | h::t -> max (maxFuncChain init t) (maxFuncChain (h init) t)

(*****************)
(* Part 3: Lists *)
(*****************)

(* 8. Reverse a list *)
let rec reverse lst = match lst with 
  | [] -> [] 
  | h :: t -> reverse t @ [ h ]


(* 9. Merge two lists sorted *)
let rec merge lst1 lst2 = 
  match lst1, lst2 with
    | [], [] -> []
    | [], lst -> lst
    | lst, [] -> lst
    | (h1::t1 as lst1), (h2::t2 as lst2) ->
      if (h1 < h2) then
        h1:: merge t1 lst2
      else
        h2:: merge t2 lst1


(* 10. is_palindrome? [1;2;1] -> true *)
let is_palindrome lst =
  let list2_rev = reverse lst 

  in
  
  let rec is_palindrome_aux lst list2_rev =
    match lst, list2_rev with
      | [], [] -> true
      | [], lst -> true
      | lst, [] -> true
      | (h1::t1 as lst), (h2::t2 as list2_rev) ->
        if (h1 = h2) then
          true && is_palindrome_aux t1 t2
        else
          false
  in is_palindrome_aux lst list2_rev


let rec get_odd_first lst = 
  match lst with
  | [] -> []
  | [(a,b)] -> []
  | (a, b):: (c,d) :: t -> c :: get_odd_first t

let rec get_even_second lst =  
  match lst with
  | [] -> []
  | [(a,b)] -> [b]
  | (a, b):: (c,d) :: t -> b :: get_even_second t


let rec length lst = match lst with
	[]-> 0
	|_::t -> 1+(length t) 


let same_length lst1 lst2 =
  let len1 = length lst1 in
  let len2 = length lst2 in
  let count = ref 0 in
  if len1 > len2 then 
    fold (fun acc e -> if !count < len2 then (count := !count + 1; e::acc) else acc) [] (lst1)
  else
    reverse lst1

let new_and_reverse lst1 lst2 = 
  let arr = same_length lst1 lst2 
  
  in
  reverse arr 

let rec take list n =
  match n, list with
    | 0, _ -> []
    | _, [] -> []
    | n, h::t -> h::take t (n-1) 

(* 11. Jumping tuples *)
let jumping_tuples lst1 lst2 = 
  let arr1 = get_odd_first lst1 in
  let arr2 = get_even_second lst2 in
  let min_length = min (length lst1) (length lst2) in
  let final_array = 
    let rec final_output arr2 arr1 = 
      match arr2, arr1 with
      | [], [] -> []
      | [], a -> a
      | a, [] -> a
      | _ when length arr2 = length arr1 -> arr2 @ arr1
      | (h2::t2 as arr2) , (h1::t1 as arr1) -> 
        [h2] @ final_output arr1 t2
      in final_output arr2 arr1
    
    in take final_array min_length

  

    
(* 12. Flatten *)
let rec flatten lst =
  match lst with
  | []-> []
  | h::t -> h @ flatten t

(* Helper: primes values? [1;2;3;4] -> [2;3] *)
let rec prime_values lst =
  match lst with
    | [] -> []
    | h::t -> 
      if is_prime h then
        h::(prime_values t) 
      else 
        prime_values t

(* 13. Square_primes [1;2;3] -> [(2,4) ; (3,9)]  *)
let square_primes lst = 
  
  let list_of_primes = 
    let rec prime_values lst =
      match lst with
        | [] -> []
        | h::t -> 
          if is_prime h then
            h::(prime_values t) 
          else 
            prime_values t
    in prime_values lst
  
  in 

  let rec return_tuple list_of_primes = 
    match list_of_primes with
    | [] -> []
    | h::t ->  [(h , pow h 2)] @ return_tuple t;

  in 
  
  return_tuple list_of_primes
  

  (* Helper: noPrime? [1;2;3;4] -> [1;4] *)
let rec no_prime_values lst =
  match lst with
    | [] -> []
    | h::t -> 
      if is_prime h then
        no_prime_values t 
      else 
        h::(no_prime_values t)


(* 14. Partition prime? [1;2;3] -> ([2;3] , [1])*)
let partition p lst =
  let satisfy = 
    let rec predicate lst = 
      match lst with
      | [] -> []
      | h::t -> 
        if p h = true then
          h::predicate t        
        else
          predicate t
    in predicate lst
  
  in
  
  let no_satisfy = 
    let rec predicate lst =
      match lst with
      | [] -> []
      | h::t ->
        if p h = false then
          h::predicate t
        else
          predicate t
    in predicate lst
  
  in
  
  (satisfy , no_satisfy)
        

(*****************)
(* Part 4: HOF *)
(*****************)

(* 15. Is present [1;2;3] 1 => [1;0;0] *)
let is_present lst x = map (fun e -> if e = x then 1 else 0) lst


(* 16. count_occurrence [1; 2; 2; 1; 3]  1  => 2 *)
let count_occ lst target = 
  fold (fun acc e -> if e = target then acc+1 else acc) 0 lst

(* let in_List lst x -> *)


(* 17. unique_list [1;2;2;1;3] => [2;1;3]*)
let uniq lst =
  fold (fun acc e -> if (count_occ acc e) > 0 then acc else e::acc) [] (lst)

