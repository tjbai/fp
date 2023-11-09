open Core
open OUnit2
open Lib

module DeterministicRandom = struct
  let int (_ : int) : int = 0
end

module IntItem = struct
  type t = int [@@deriving compare, sexp]
end

open Distribution (IntItem) (DeterministicRandom)

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

let test_sanitize _ =
  assert_equal "abcd" @@ sanitize "ABCD";
  assert_equal "121" @@ sanitize "12@1";
  assert_equal "abcd" @@ sanitize "AbC!!!!@#$D"

let test_sanitize_random _ =
  let is_sanitized (s : string) : bool =
    String.fold s ~init:true ~f:(fun acc el ->
        acc && Char.is_alphanum el && Char.( = ) (Char.lowercase el) el)
  in

  Quickcheck.test (List.quickcheck_generator String.quickcheck_generator)
    ~f:(fun res ->
      assert_bool "Something is not sanitized!"
        (List.map res ~f:(fun x -> x |> sanitize |> is_sanitized)
        |> List.fold ~init:true ~f:(fun acc el -> acc && el)))

let test_parse _ =
  assert_equal [] @@ parse_tokens "";

  assert_equal [ "a"; "b"; "hello"; "test" ]
  @@ parse_tokens "a b\n\nhello\ttest";

  assert_equal [ "abcd"; "hello"; "test" ]
  @@ parse_tokens "abCD### heLLo\t\n test";

  assert_equal [ "in"; "the"; "end"; "it"; "doesnt"; "even"; "matter" ]
  @@ parse_tokens "In the end, it doesn't even matter."

module IntDistributionB =
  Distribution
    (IntItem)
    (struct
      let int (n : int) : int = n - 1
    end)

module IntDistributionC =
  Distribution
    (IntItem)
    (struct
      let int (n : int) : int = n / 2
    end)

let test_random_context _ =
  let short_corpus = [ 1; 2; 3; 4; 5 ] in

  make_distribution 2 short_corpus
  |> sample_random_context |> assert_equal [ 1 ];

  IntDistributionB.(
    make_distribution 2 short_corpus
    |> sample_random_context |> assert_equal [ 4 ]);

  IntDistributionC.(
    make_distribution 2 short_corpus
    |> sample_random_context |> assert_equal [ 3 ]);

  let long_corpus = [ 1; 2; 7; 1; 2; 3; 5; 8; 1; 2; 3; 5; 1 ] in

  make_distribution 3 long_corpus
  |> sample_random_context
  |> assert_equal [ 1; 2 ];

  IntDistributionB.(
    make_distribution 3 long_corpus
    |> sample_random_context
    |> assert_equal [ 8; 1 ]);

  IntDistributionC.(
    make_distribution 3 long_corpus
    |> sample_random_context
    |> assert_equal [ 2; 7 ])

open Distribution (String) (Random)

let test_most_frequent _ =
  [ "a"; "b"; "c"; "d" ] |> make_distribution 2
  |> Fn.flip most_frequent_ngrams 1
  |> assert_equal [ ([ "a"; "b" ], 1) ];

  [ "a"; "b"; "c"; "d" ] |> make_distribution 3
  |> Fn.flip most_frequent_ngrams 1
  |> assert_equal [ ([ "a"; "b"; "c" ], 1) ];

  [ "a"; "b"; "c"; "d" ] |> make_distribution 2
  |> Fn.flip most_frequent_ngrams 2
  |> assert_equal [ ([ "a"; "b" ], 1); ([ "b"; "c" ], 1) ];

  [ "a"; "b"; "c"; "d"; "c"; "d" ]
  |> make_distribution 2
  |> Fn.flip most_frequent_ngrams 2
  |> assert_equal [ ([ "c"; "d" ], 2); ([ "a"; "b" ], 1) ]

let lib_tests =
  "Library Unit Tests"
  >: test_list
       [
         "Make bigram distribution" >:: test_bigram_distribution;
         "Make trigram distribution" >:: test_trigram_distribution;
         "Sample from bigram distribution" >:: test_sample;
         "Sanitize string" >:: test_sanitize;
         "Sanitize random corpus" >:: test_sanitize_random;
         "Parse tokens from file" >:: test_parse;
         "Choose random starting context" >:: test_random_context;
         "Most frequent n-grams" >:: test_most_frequent;
       ]

let series = "Assignment4 Tests" >::: [ lib_tests ]
let () = run_test_tt_main series
