type person = { name: string;
                age: int;
                hobbies: string list }

(* Define the type of db below *)
type db = person list(* ref = makes db mutable *)

(* :db -> type db *)
(* ref [] -> ref to an empty list *)
let newDatabase:db = []

let insert person db =
  db @ [person]

(* List. filter -> Selects that meet up conditions *)
(* <> -> not equal *)
let rec remove name db =
  List.filter (fun e -> e.name <> name) db 

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

let rec query condition db = 
  match condition with
    | True -> db
    | False -> []
    | Age func -> List.filter (fun person -> func person.age) db
    | Name func -> List.filter (fun person -> func person.name) db
    | Hobbies func -> List.filter (fun person -> func person.hobbies) db
    | And (c1, c2) -> 
        List.filter 
          (fun person -> 
            List.mem person (query c1 db) 
            && 
            List.mem person (query c2 db)
          ) 
          db
    | Or (c1, c2) -> List.fold_left 
                    (fun acc person -> if List.mem person acc then acc else person::acc)
                    [] 
                    ((query c1 db)@(query c2 db))
    | Not c -> List.filter (fun person -> not (List.mem person (query c db))) db
    | If (c1, c2, c3) -> 
        List.fold_left 
        (fun acc person -> 
          if List.length (query c1 [person]) > 0 then 
            (query c2 [person])@acc 
          else 
            (query c3 [person])@acc 
        ) 
        [] 
        db

type comparator = person -> person -> int

let rec sort comparator db = 
  List.sort comparator db


let queryBy condition db comparator = 
  List.sort comparator (query condition db)

  
let update condition db personData =
  List.map (fun person ->
    if List.length (query condition [person]) > 0 then
      personData person
    else
      person
  ) db


let deleteAll condition db = 
  query (Not condition) db

