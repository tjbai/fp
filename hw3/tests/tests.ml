(*
  Part II: Tests
 
  In this part, you will need to create and run your own tests.  Tests should
  cover both common cases and edge cases.  In previous assignments, we only
  asked for a specified number of additional tests, but in this assignment we
  will be grading based on code coverage.
 
  Aim for complete code coverage on all functions, and we will check 
  by running the bisect tool on your code.  For that reason, you need 
  to add the following line in the dune file for your library:
      
      (preprocess (pps bisect_ppx))
 
  or else your tests will not run in the autograder.

 Additionally, you will need to write a special suite of tests here which
 verifies some invariants.  See the assignment for details.
 
*)

open OUnit2
open Finite_group

let of_int_unsafe (n : int) : Z5_add.t =
  match Z5_add.of_int n with Some c -> c | None -> assert false

let test_id _ = assert_equal 0 @@ Z5_add.to_int @@ Z5_add.id

let test_inverse _ =
  assert_equal 2 @@ Z5_add.to_int @@ Z5_add.inverse @@ of_int_unsafe 3;

  assert_equal 3 @@ Z5_add.to_int @@ Z5_add.inverse @@ of_int_unsafe 2;

  assert_equal 1 @@ Z5_add.to_int @@ Z5_add.inverse @@ of_int_unsafe 4;

  assert_equal 0 @@ Z5_add.to_int @@ Z5_add.inverse @@ of_int_unsafe 0

let test_op _ =
  assert_equal 2 @@ Z5_add.to_int
  @@ Z5_add.op (of_int_unsafe 1) (of_int_unsafe 1);

  assert_equal 3 @@ Z5_add.to_int
  @@ Z5_add.op (of_int_unsafe 2) (of_int_unsafe 1);

  assert_equal 3 @@ Z5_add.to_int
  @@ Z5_add.op (of_int_unsafe 1) (of_int_unsafe 2);

  assert_equal 1 @@ Z5_add.to_int
  @@ Z5_add.op (of_int_unsafe 3) (of_int_unsafe 3);

  assert_equal 2 @@ Z5_add.to_int
  @@ Z5_add.op (of_int_unsafe 3) (of_int_unsafe 4)

let p1_tests =
  "Assingment3 Part1 Tests"
  >: test_list
       [ "id" >:: test_id; "inverse" >:: test_inverse; "op" >:: test_op ]

let series = "Assignment3 Tests" >::: [ p1_tests ]
let () = run_test_tt_main series
