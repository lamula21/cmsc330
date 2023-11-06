type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree =
  match tree with
  | Leaf -> init
  | Node (left, value, right) ->
      let left = tree_fold f init left in
      let right = tree_fold f init right in
      f left value right

let map tree f = 
  let map_f left value right = 
    Node (left, f value, right) 
    in
    tree_fold map_f Leaf tree

let mirror tree = 
  let mirror_f left value right = 
    Node (right, value, left) 
    (* since tree_fold takes f left value right, this passes f right value left *)
    in
    tree_fold mirror_f Leaf tree 

let in_order tree = 
  tree_fold (fun l v r -> l@v::r) [] tree 


let pre_order tree = 
  tree_fold (fun l v r -> v::l@r) [] tree


let compose tree = 
    
  tree_fold  
    (fun left_func value right_func -> fun x -> right_func (value (left_func x)))
    (fun x -> x) 
    tree


let depth tree = 
  tree_fold
    (fun l v r -> 1 + max l r)
    0
    tree

(* Destructuring left value of a tuple *)
let tuple_value tuple = 
  let (value,depth) = tuple
  in
    value
  
(* Destructuring right value of a tuple *)
let tuple_depth tuple = 
  let (value,depth) = tuple
  in 
    depth


(* Assume complete tree *)
let trim tree n =
  let new_tree = (* Build a new tree to store a tuple: (value,depth) at each node *)
    tree_fold 
    (
      fun l v r -> 
        Node( l , 
              ( v, (depth tree) - (depth l)),  (* (value,depth) *)
              r)
    )
    Leaf 
    tree

  in 
    tree_fold
    (
      fun l v r ->    (* v is a tuple: (value,depth) *)
        if (tuple_depth v) <= n then    (* If depth is less than desired depth to trim *)
          Node(l , tuple_value v, r)    (* Replace that node without tuple_depth *)
        else
          Leaf    (* Once it passed the depth, replace with a LEAF to trim tree *)
    )
    Leaf
    new_tree

let rec tree_init f v = 
  match f v with
  | None -> Leaf
  | Some (v1, v2, v3) ->
      let left_subtree = tree_init f v1 in
      let right_subtree = tree_init f v3 in
      Node(left_subtree, v2, right_subtree)

let rec split lst v =
  match lst with
  | [] -> ([], [])
  | h :: t ->
      if h = v then
        ([], t)
      else
        let (left, right) = split t v in
        (h :: left, right)

let rec take n lst =
  match n, lst with
  | 0, _ -> []
  | _, [] -> []
  | n, h::t -> h::take (n - 1) t


let rec drop n lst =
  match n, lst with
  | 0, lst -> lst
  | _, [] -> []
  | n, h::t -> drop (n - 1) t

let rec from_pre_in pre in_ord = 
  match pre with
  | [] -> Leaf
  | root :: t ->
      let left_in_ord, right_in_ord = split in_ord root in
      let left_subtree = from_pre_in (take (List.length left_in_ord) t) left_in_ord in
      let right_subtree = from_pre_in (drop (List.length left_in_ord) t) right_in_ord in
      Node(left_subtree, root, right_subtree)

  
let rec add_n_times k n =
  match n with
  | 0 -> []
  | _ -> k::(add_n_times k (n-1))

let add_k_n_times lst k n i =
  if i >= List.length lst then
    lst @ add_n_times k n
  else
    let (result, idx) =
      List.fold_left
      (fun (acc, counter) e -> 
        if i == counter then
          (acc@add_n_times k n @ [e] , counter+1)
        else
          (acc @ [e], counter + 1)
      )
      ([],0)
      lst
        in result
