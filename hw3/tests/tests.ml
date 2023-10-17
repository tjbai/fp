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
open Ring

module Test_group = Finite_group.Make (struct
  let op (a : int) (b : int) : int = a + b - 2
  let n = 5
end)

let z5_of_int (n : int) : Z5_add.t =
  match Z5_add.of_int n with Some c -> c | None -> assert false

let z4_of_string (n : string) : Z4.t =
  match Z4.of_string n with Some c -> c | None -> assert false

let test_finite_group_id _ =
  assert_equal 0 @@ Z5_add.to_int Z5_add.id;
  assert_equal 2 @@ Test_group.to_int Test_group.id

let test_finite_group_inverse _ =
  assert_equal 2 @@ Z5_add.to_int @@ Z5_add.inverse @@ z5_of_int 3;
  assert_equal 3 @@ Z5_add.to_int @@ Z5_add.inverse @@ z5_of_int 2;
  assert_equal 1 @@ Z5_add.to_int @@ Z5_add.inverse @@ z5_of_int 4;
  assert_equal 0 @@ Z5_add.to_int @@ Z5_add.inverse @@ z5_of_int 0

let test_finite_group_op _ =
  assert_equal 2 @@ Z5_add.to_int @@ Z5_add.op (z5_of_int 1) (z5_of_int 1);
  assert_equal 3 @@ Z5_add.to_int @@ Z5_add.op (z5_of_int 2) (z5_of_int 1);
  assert_equal 3 @@ Z5_add.to_int @@ Z5_add.op (z5_of_int 1) (z5_of_int 2);
  assert_equal 1 @@ Z5_add.to_int @@ Z5_add.op (z5_of_int 3) (z5_of_int 3);
  assert_equal 2 @@ Z5_add.to_int @@ Z5_add.op (z5_of_int 3) (z5_of_int 4)

module Memo_Z5_add = Memoize (Z5_add)

let test_memo_inverse _ =
  assert_equal 2 @@ Memo_Z5_add.to_int @@ Memo_Z5_add.inverse @@ z5_of_int 3;
  assert_equal 3 @@ Memo_Z5_add.to_int @@ Memo_Z5_add.inverse @@ z5_of_int 2;
  assert_equal 1 @@ Memo_Z5_add.to_int @@ Memo_Z5_add.inverse @@ z5_of_int 4;
  assert_equal 0 @@ Memo_Z5_add.to_int @@ Memo_Z5_add.inverse @@ z5_of_int 0

let test_ring_plus _ =
  assert_equal "2" @@ Z4.to_string
  @@ Z4.( + ) (z4_of_string "1") (z4_of_string "1");

  assert_equal "1" @@ Z4.to_string
  @@ Z4.( + ) (z4_of_string "2") (z4_of_string "3");

  assert_equal "1" @@ Z4.to_string
  @@ Z4.( + ) (z4_of_string "3") (z4_of_string "2")

let test_ring_times _ =
  assert_equal "1" @@ Z4.to_string
  @@ Z4.( * ) (z4_of_string "1") (z4_of_string "1");

  assert_equal "2" @@ Z4.to_string
  @@ Z4.( * ) (z4_of_string "2") (z4_of_string "3");

  assert_equal "1" @@ Z4.to_string
  @@ Z4.( * ) (z4_of_string "3") (z4_of_string "3")

let p1_tests =
  "Assingment3 Part1 Tests"
  >: test_list
       [
         "finite_group_id" >:: test_finite_group_id;
         "finite_group_inverse" >:: test_finite_group_inverse;
         "finite_group_op" >:: test_finite_group_op;
         "memoized_inverse" >:: test_memo_inverse;
         "ring_plus" >:: test_ring_plus;
         "ring_times" >:: test_ring_times;
       ]

let test_z4_next _ =
  (assert_equal ("", "0")
  @@
  match Postfix_calc.Z4_data.next "0" with
  | Some (exp, value) -> (exp, Postfix_calc.Z4_data.to_string value)
  | None -> ("", ""));

  (assert_equal ("+", "0")
  @@
  match Postfix_calc.Z4_data.next "0+" with
  | Some (exp, value) -> (exp, Postfix_calc.Z4_data.to_string value)
  | None -> ("", ""));

  (assert_equal (" +", "0")
  @@
  match Postfix_calc.Z4_data.next "0 +" with
  | Some (exp, value) -> (exp, Postfix_calc.Z4_data.to_string value)
  | None -> ("", ""));

  (assert_equal ("_asdfasdfasdf", "3")
  @@
  match Postfix_calc.Z4_data.next "3_asdfasdfasdf" with
  | Some (exp, value) -> (exp, Postfix_calc.Z4_data.to_string value)
  | None -> ("", ""));

  assert_equal None @@ Postfix_calc.Z4_data.next "";
  assert_equal None @@ Postfix_calc.Z4_data.next "123+";
  assert_equal None @@ Postfix_calc.Z4_data.next "-1 2 - 123+";
  assert_equal None @@ Postfix_calc.Z4_data.next "+1";
  assert_equal None @@ Postfix_calc.Z4_data.next "*1"

let test_int_next _ =
  (assert_equal ("", "-1")
  @@
  match Postfix_calc.Int_data.next "-1" with
  | Some (exp, value) -> (exp, Postfix_calc.Int_data.to_string value)
  | None -> ("", ""));

  (assert_equal (" 12345", "12345")
  @@
  match Postfix_calc.Int_data.next "12345 12345" with
  | Some (exp, value) -> (exp, Postfix_calc.Int_data.to_string value)
  | None -> ("", ""));

  assert_equal (" 12345", "-12345")
  @@
  match Postfix_calc.Int_data.next "-12345 12345" with
  | Some (exp, value) -> (exp, Postfix_calc.Int_data.to_string value)
  | None -> ("", "")

let test_rat_next _ =
  (assert_equal ("", "1/2")
  @@
  match Postfix_calc.Rat_data.next "1/2" with
  | Some (exp, value) -> (exp, Postfix_calc.Rat_data.to_string value)
  | None -> ("", ""));

  (assert_equal ("", "3/2")
  @@
  match Postfix_calc.Rat_data.next "18/12" with
  | Some (exp, value) -> (exp, Postfix_calc.Rat_data.to_string value)
  | None -> ("", ""));

  (assert_equal ("", "-1/2")
  @@
  match Postfix_calc.Rat_data.next "-2/4" with
  | Some (exp, value) -> (exp, Postfix_calc.Rat_data.to_string value)
  | None -> ("", ""));

  (assert_equal ("", "0/1")
  @@
  match Postfix_calc.Rat_data.next "0/1" with
  | Some (exp, value) -> (exp, Postfix_calc.Rat_data.to_string value)
  | None -> ("", ""));

  (assert_equal ("", "5/1")
  @@
  match Postfix_calc.Rat_data.next "5" with
  | Some (exp, value) -> (exp, Postfix_calc.Rat_data.to_string value)
  | None -> ("", ""));

  assert_equal None @@ Postfix_calc.Rat_data.next "/";
  assert_equal None @@ Postfix_calc.Rat_data.next "/50";
  assert_equal None @@ Postfix_calc.Rat_data.next "5/0"

let test_z4_eval _ =
  (assert_equal "2"
  @@
  match Postfix_calc.Z4_eval.eval "1 1 +" with
  | Ok res -> Postfix_calc.Z4_data.to_string res
  | Error msg -> msg);

  (assert_equal "2"
  @@
  match Postfix_calc.Z4_eval.eval "1\r1\t+" with
  | Ok res -> Postfix_calc.Z4_data.to_string res
  | Error msg -> msg);

  (assert_equal "2"
  @@
  match Postfix_calc.Z4_eval.eval "1\r1 +\n" with
  | Ok res -> Postfix_calc.Z4_data.to_string res
  | Error msg -> msg);

  (assert_equal "unmatched"
  @@
  match Postfix_calc.Z4_eval.eval "" with
  | Ok res -> Postfix_calc.Z4_data.to_string res
  | Error msg -> msg);

  assert_equal "unmatched"
  @@
  match Postfix_calc.Z4_eval.eval "1 1 1 1" with
  | Ok res -> Postfix_calc.Z4_data.to_string res
  | Error msg -> msg

let test_int_eval _ =
  (assert_equal "3"
  @@
  match Postfix_calc.Int_eval.eval "1 2 +" with
  | Ok res -> Postfix_calc.Int_data.to_string res
  | Error msg -> msg);

  (assert_equal "8"
  @@
  match Postfix_calc.Int_eval.eval "1 2 3++ 2 +" with
  | Ok res -> Postfix_calc.Int_data.to_string res
  | Error msg -> msg);

  (assert_equal "8"
  @@
  match Postfix_calc.Int_eval.eval "   2    2 2**" with
  | Ok res -> Postfix_calc.Int_data.to_string res
  | Error msg -> msg);

  (assert_equal "illegal character"
  @@
  match Postfix_calc.Int_eval.eval "2 2+ __" with
  | Ok res -> Postfix_calc.Int_data.to_string res
  | Error msg -> msg);

  (assert_equal "unmatched"
  @@
  match Postfix_calc.Int_eval.eval "1 2 + +" with
  | Ok res -> Postfix_calc.Int_data.to_string res
  | Error msg -> msg);

  assert_equal "unmatched"
  @@
  match Postfix_calc.Int_eval.eval "1 + +" with
  | Ok res -> Postfix_calc.Int_data.to_string res
  | Error msg -> msg

let test_rat_eval _ =
  (assert_equal "9/1"
  @@
  match Postfix_calc.Rat_eval.eval "5 4 +" with
  | Ok res -> Postfix_calc.Rat_data.to_string res
  | Error msg -> msg);

  (assert_equal "1/1"
  @@
  match Postfix_calc.Rat_eval.eval "1/2 1/2 +" with
  | Ok res -> Postfix_calc.Rat_data.to_string res
  | Error msg -> msg);

  (assert_equal "1/1"
  @@
  match Postfix_calc.Rat_eval.eval "1/2 1/2 * 3/4 +" with
  | Ok res -> Postfix_calc.Rat_data.to_string res
  | Error msg -> msg);

  (assert_equal "2/1"
  @@
  match Postfix_calc.Rat_eval.eval "4/4 1 +" with
  | Ok res -> Postfix_calc.Rat_data.to_string res
  | Error msg -> msg);

  assert_equal "1/4"
  @@
  match Postfix_calc.Rat_eval.eval "1/2 1/2 *" with
  | Ok res -> Postfix_calc.Rat_data.to_string res
  | Error msg -> msg

let p2_tests =
  "Assignment3 Part2 Tests"
  >: test_list
       [
         "z4_next" >:: test_z4_next;
         "int_next" >:: test_int_next;
         "rat_next" >:: test_rat_next;
         "z4_eval" >:: test_z4_eval;
         "int_eval" >:: test_int_eval;
         "rat_eval" >:: test_rat_eval;
       ]

let series = "Assignment3 Tests" >::: [ p1_tests; p2_tests ]
let () = run_test_tt_main series
