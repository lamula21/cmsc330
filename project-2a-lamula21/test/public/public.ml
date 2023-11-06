open OUnit2
open Project2a.Basics
open TestUtils

let test_area _ =
  assert_equal 1 (area (1, 1) (2, 2)) ~msg:"area (1)";
  assert_equal 2 (area (1, 1) (2, 3)) ~msg:"area (2)";
  assert_equal 2 (area (1, 1) (3, 2)) ~msg:"area (3)";
  assert_equal 4 (area (1, 1) (3, 3)) ~msg:"area (4)"

let test_fibonacci _ =
  assert_equal 1 (fibonacci 1) ~msg:"fibonacci (1)";
  assert_equal 1 (fibonacci 2) ~msg:"fibonacci (2)";
  assert_equal 8 (fibonacci 6) ~msg:"fibonacci (3)";
  assert_equal 144 (fibonacci 12) ~msg:"fibonacci (4)"

let test_pow _ =
  assert_equal 2 (pow 2 1) ~msg:"pow (1)";
  assert_equal 4 (pow 2 2) ~msg:"pow (2)";
  assert_equal 3 (pow 3 1) ~msg:"pow (3)";
  assert_equal 27 (pow 3 3) ~msg:"pow (4)";
  assert_equal 625 (pow 5 4) ~msg:"pow (5)";
  assert_equal (-27) (pow (-3) 3) ~msg:"pow (6)"

let test_is_prime _ =
  assert_equal false (is_prime 1) ~msg:"is_prime (1)";
  assert_equal true (is_prime 2) ~msg:"is_prime (2)";
  assert_equal true (is_prime 3) ~msg:"is_prime (3)";
  assert_equal false (is_prime 4) ~msg:"is_prime (4)";
  assert_equal true (is_prime 5) ~msg:"is_prime (5)";
  assert_equal false (is_prime 60) ~msg:"is_prime (6)";
  assert_equal true (is_prime 61) ~msg:"is_prime (7)"

let test_maxFuncChain _ =
  assert_equal 8 (maxFuncChain 2 [ (fun x -> x + 6) ]) ~msg:"maxFuncChain (1)";
  assert_equal 24
    (maxFuncChain 2 [ (fun x -> x + 4); (fun x -> x * 4) ])
    ~msg:"maxFuncChain (2)";
  assert_equal (-1)
    (maxFuncChain (-4) [ (fun x -> x * 4); (fun x -> x + 3) ])
    ~msg:"maxFuncChain (3)";
  assert_equal 14
    (maxFuncChain 4 [ (fun x -> x - 2); (fun x -> x + 10) ])
    ~msg:"maxFuncChain (4)";
  assert_equal 501
    (maxFuncChain 0 [ (fun x -> x - 1); (fun x -> x * -500); (fun x -> x + 1) ])
    ~msg:"maxFuncChain (5)"

let test_reverse _ =
  assert_equal [ 1 ] (reverse [ 1 ]) ~msg:"reverse (1)";
  assert_equal [ 3; 2; 1 ] (reverse [ 1; 2; 3 ]) ~msg:"reverse (2)";
  assert_equal [] (reverse []) ~msg:"reverse (3)";
  assert_equal [ "c"; "b"; "a" ] (reverse [ "a"; "b"; "c" ]) ~msg:"reverse (4)"

let test_merge _ =
  assert_equal [ 1; 2 ] (merge [ 1 ] [ 2 ]) ~msg:"merge (1)";
  assert_equal [] (merge [] []) ~msg:"merge (2)";
  assert_equal [ 1; 2; 3; 4 ] (merge [ 1; 4 ] [ 2; 3 ]) ~msg:"merge (3)";
  assert_equal [ 0; 1 ] (merge [ 1 ] [ 0 ]) ~msg:"merge (4)";
  assert_equal
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    (merge [ 1; 4; 5 ] [ 2; 3; 6; 7; 8; 9 ])
    ~msg:"merge (5)"

let test_jumping_tuples _ =
  assert_equal [ 8; 3; 12 ]
    (jumping_tuples [ (1, 2); (3, 4); (5, 6) ] [ (7, 8); (9, 10); (11, 12) ])
    ~msg:"jumping tuples (1)";
  assert_equal [ false ]
    (jumping_tuples [ (true, "a"); (false, "b") ] [ (100, false) ])
    ~msg:"jumping tuples (2)";
  assert_equal [] (jumping_tuples [] []) ~msg:"jumping tuples (3)";
  assert_equal [ "sixth"; "third" ]
    (jumping_tuples
       [ ("first", "second"); ("third", "fourth") ]
       [ ("fifth", "sixth"); ("seventh", "eighth") ])
    ~msg:"jumping tuples (4)"

let test_is_palindrome _ =
  assert_equal true (is_palindrome [ 1; 2; 3; 2; 1 ]) ~msg:"is_palindrome (1)";
  assert_equal true
    (is_palindrome [ "a"; "n"; "n"; "a" ])
    ~msg:"is_palindrome (2)";
  assert_equal false
    (is_palindrome [ "N"; "o"; "o"; "n" ])
    ~msg:"is_palindrome (3)";
  assert_equal false
    (is_palindrome [ "O"; "C"; "A"; "M"; "L" ])
    ~msg:"is_palindrome (4)"

let test_flatten _ =
  assert_equal [ 1; 2; 3; 4; 5; 6 ]
    (flatten [ [ 1; 2; 3; 4 ]; [ 5; 6 ] ])
    ~msg:"flatten (1)";
  assert_equal [ 1; 2; 3; 4 ]
    (flatten [ [ 1; 2 ]; [ 3; 4 ]; [] ])
    ~msg:"flatten (2)";
  assert_equal [ 1; 2; 3 ] (flatten [ [ 1 ]; [ 2 ]; [ 3 ] ]) ~msg:"flatten (3)";
  assert_equal [ 6; 5; 4; 3; 2; 1 ]
    (flatten [ [ 6 ]; [ 5 ]; [ 4 ]; [ 3 ]; [ 2 ]; [ 1 ] ])
    ~msg:"flatten (4)"

let test_square_primes _ =
  assert_equal
    [ (2, 4); (3, 9); (5, 25) ]
    (square_primes [ 1; 2; 3; 4; 5 ])
    ~msg:"square_primes (1)";
  assert_equal
    [ (11, 121); (13, 169) ]
    (square_primes [ 10; 11; 12; 13; 14 ])
    ~msg:"square_primes (2)";
  assert_equal [] (square_primes [ 4; 6; 8 ]) ~msg:"square_primes (3)";
  assert_equal
    [ (2, 4); (2, 4); (3, 9) ]
    (square_primes [ 2; 2; 3; 4 ])
    ~msg:"square_primes (4)"

let test_partition _ =
  assert_equal
    ([ 2; 3; 5 ], [ 1; 4 ])
    (partition is_prime [ 1; 2; 3; 4; 5 ])
    ~msg:"partition (1)";
  assert_equal
    ([], [ 10; 12; 14 ])
    (partition is_prime [ 10; 12; 14 ])
    ~msg:"partition (2)";
  assert_equal
    ([ 2; 4 ], [ 1; 3; 5 ])
    (partition is_even [ 1; 2; 3; 4; 5 ])
    ~msg:"partition (3)"

let test_is_present _ =
  let a = [] in
  let b = [ "w"; "x"; "y"; "z" ] in
  let c = [ 14; 20; 42; 1; 81 ] in

  assert_equal ~printer:string_of_int_list [] @@ is_present a 123;
  assert_equal ~printer:string_of_int_list [ 0; 0; 1; 0 ] @@ is_present b "y";
  assert_equal ~printer:string_of_int_list [ 0; 0; 0; 0 ] @@ is_present b "v";
  assert_equal ~printer:string_of_int_list [ 0; 0; 1; 0; 0 ] @@ is_present c 42;
  assert_equal ~printer:string_of_int_list [ 1; 0; 0; 0; 0 ] @@ is_present c 14

let test_count_occ _ =
  let y = [ "a"; "a"; "b"; "a" ] in
  let z = [ 1; 7; 7; 1; 5; 2; 7; 7 ] in
  let a = [ true; false; false; true; false; false; false ] in
  let b = [] in

  assert_equal ~printer:string_of_int_pair (3, 1)
  @@ (count_occ y "a", count_occ y "b");
  assert_equal ~printer:string_of_int_quad (2, 4, 1, 1)
  @@ (count_occ z 1, count_occ z 7, count_occ z 5, count_occ z 2);
  assert_equal ~printer:string_of_int_pair (2, 5)
  @@ (count_occ a true, count_occ a false);
  assert_equal ~printer:string_of_int_pair (0, 0)
  @@ (count_occ b "a", count_occ b 1)

let test_uniq _ =
  let y = [ "a"; "a"; "b"; "a" ] in
  let z = [ 1; 7; 7; 1; 5; 2; 7; 7 ] in
  let a = [ true; false; false; true; false; false; false ] in
  let b = [] in
  let cmp x y = if x < y then -1 else if x = y then 0 else 1 in

  assert_equal ~printer:string_of_string_list [ "a"; "b" ]
  @@ List.sort cmp (uniq y);
  assert_equal ~printer:string_of_int_list [ 1; 2; 5; 7 ]
  @@ List.sort cmp (uniq z);
  assert_equal ~printer:string_of_bool_list [ false; true ]
  @@ List.sort cmp (uniq a);
  assert_equal ~printer:string_of_int_list [] @@ uniq b

let suite =
  "public"
  >::: [
         "area" >:: test_area;
         "fibonacci" >:: test_fibonacci;
         "pow" >:: test_pow;
         "is_prime" >:: test_is_prime;
         "maxFuncChain" >:: test_maxFuncChain;
         "reverse" >:: test_reverse;
         "merge" >:: test_merge;
         "jumping_tuples" >:: test_jumping_tuples;
         "is_palindrome" >:: test_is_palindrome;
         "flatten" >:: test_flatten;
         "square_primes" >:: test_square_primes;
         "partition" >:: test_partition;
         "is_present" >:: test_is_present;
         "count_occ" >:: test_count_occ;
         "uniq" >:: test_uniq;
       ]

let _ = run_test_tt_main suite
