open Core
open OUnit2
open Lib

module DeterministicRandom = struct
  let int (_ : int) = 0
end

module IntItem = struct
  type t = int [@@deriving compare, sexp]
end

module IntDistribution = Distribution (IntItem) (DeterministicRandom)
open IntDistribution

let test_bigram_distribution _ =
  assert_equal [] @@ (make_distribution 2 [] |> to_list);

  assert_equal [ ([ 1 ], [ 1; 1; 1; 1 ]) ]
  @@ (make_distribution 2 [ 1; 1; 1; 1; 1 ] |> to_list);

  assert_equal [ ([ 1 ], [ 2; 1; 1; 1; 1 ]) ]
  @@ (make_distribution 2 [ 1; 1; 1; 1; 1; 2 ] |> to_list);

  assert_equal
    [ ([ 1 ], [ 2 ]); ([ 2 ], [ 3 ]); ([ 3 ], [ 4 ]); ([ 4 ], [ 5 ]) ]
  @@ (make_distribution 2 [ 1; 2; 3; 4; 5 ] |> to_list);

  assert_equal
    [
      ([ 1 ], [ 2 ]);
      ([ 2 ], [ 1; 3 ]);
      ([ 3 ], [ 2; 4 ]);
      ([ 4 ], [ 3; 5 ]);
      ([ 5 ], [ 4 ]);
    ]
  @@ (make_distribution 2 [ 1; 2; 3; 4; 5; 4; 3; 2; 1 ] |> to_list);

  assert_equal
    [
      ([ 1 ], [ 2; 2 ]);
      ([ 2 ], [ 3; 7 ]);
      ([ 3 ], [ 5 ]);
      ([ 5 ], [ 8 ]);
      ([ 7 ], [ 1 ]);
      ([ 8 ], [ 1 ]);
    ]
  @@ (make_distribution 2 [ 1; 2; 7; 1; 2; 3; 5; 8; 1 ] |> to_list)

let test_trigram_distribution _ =
  assert_equal [ ([ 1; 1 ], [ 1; 1; 1 ]) ]
  @@ (make_distribution 3 [ 1; 1; 1; 1; 1 ] |> to_list);

  assert_equal [ ([ 1; 1 ], [ 2; 1; 1; 1 ]) ]
  @@ (make_distribution 3 [ 1; 1; 1; 1; 1; 2 ] |> to_list);

  assert_equal [ ([ 1; 2 ], [ 3 ]); ([ 2; 3 ], [ 4 ]); ([ 3; 4 ], [ 5 ]) ]
  @@ (make_distribution 3 [ 1; 2; 3; 4; 5 ] |> to_list);

  assert_equal
    [
      ([ 1; 2 ], [ 3 ]);
      ([ 2; 3 ], [ 4 ]);
      ([ 3; 2 ], [ 1 ]);
      ([ 3; 4 ], [ 5 ]);
      ([ 4; 3 ], [ 2 ]);
      ([ 4; 5 ], [ 4 ]);
      ([ 5; 4 ], [ 3 ]);
    ]
  @@ (make_distribution 3 [ 1; 2; 3; 4; 5; 4; 3; 2; 1 ] |> to_list);

  assert_equal
    [
      ([ 1; 2 ], [ 3; 3; 7 ]);
      ([ 2; 3 ], [ 5; 5 ]);
      ([ 2; 7 ], [ 1 ]);
      ([ 3; 5 ], [ 1; 8 ]);
      ([ 5; 8 ], [ 1 ]);
      ([ 7; 1 ], [ 2 ]);
      ([ 8; 1 ], [ 2 ]);
    ]
  @@ (make_distribution 3 [ 1; 2; 7; 1; 2; 3; 5; 8; 1; 2; 3; 5; 1 ] |> to_list)

let test_sample _ =
  let simple_distribution = make_distribution 2 [ 1; 1 ] in

  assert_equal [] @@ sample_random_sequence simple_distribution [ 1 ] 0;

  assert_equal [] @@ sample_random_sequence simple_distribution [ 100 ] 5;

  assert_equal [ 1 ] @@ sample_random_sequence simple_distribution [ 1 ] 1;

  assert_equal [ 1; 1; 1; 1; 1 ]
  @@ sample_random_sequence simple_distribution [ 1 ] 5;

  let increasing_distribution = make_distribution 2 [ 1; 2; 3; 4; 5 ] in

  assert_equal [ 2; 3; 4; 5 ]
  @@ sample_random_sequence increasing_distribution [ 1 ] 4;

  let complex_distribution =
    make_distribution 3 [ 1; 2; 7; 1; 2; 3; 5; 8; 1; 2; 3; 5; 1 ]
  in

  assert_equal [ 3; 5; 1 ]
  @@ sample_random_sequence complex_distribution [ 1; 2 ] 10;

  assert_equal [ 1; 2; 3; 5; 1 ]
  @@ sample_random_sequence complex_distribution [ 5; 8 ] 10

let lib_tests =
  "Library Unit Tests"
  >: test_list
       [
         "Make bigram distribution" >:: test_bigram_distribution;
         "Make trigram distribution" >:: test_trigram_distribution;
         "Sample from bigram distribution" >:: test_sample;
       ]

let series = "Assignment4 Tests" >::: [ lib_tests ]
let () = run_test_tt_main series
