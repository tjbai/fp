open Core
open OUnit2
module D = Simpledict
module T = D.Tree
open D.Dict_item

let t1 =
  T.(
    Branch
      {
        item = "d";
        left = Branch { item = "a"; left = Leaf; right = Leaf };
        right = Branch { item = "e"; left = Leaf; right = Leaf };
      })

let t1_out_of_order =
  T.(
    Branch
      {
        item = "d";
        left = Branch { item = "z"; left = Leaf; right = Leaf };
        right = Branch { item = "e"; left = Leaf; right = Leaf };
      })

let t2_out_of_order =
  T.(
    Branch
      {
        item = "d";
        left =
          Branch
            {
              item = "d";
              left = Branch { item = "a"; left = Leaf; right = Leaf };
              right = Branch { item = "e"; left = Leaf; right = Leaf };
            };
        right = Branch { item = "e"; left = Leaf; right = Leaf };
      })

let t3 =
  T.(
    Branch
      {
        item = "d";
        left =
          Branch
            {
              item = "d";
              left =
                Branch
                  {
                    item = "d";
                    left = Branch { item = "a"; left = Leaf; right = Leaf };
                    right = Branch { item = "e"; left = Leaf; right = Leaf };
                  };
              right = Branch { item = "e"; left = Leaf; right = Leaf };
            };
        right = Branch { item = "e"; left = Leaf; right = Leaf };
      })

let test_tree_size _ =
  assert_equal 0 @@ T.size T.Leaf;
  assert_equal 3 @@ T.size t1

let test_tree_height _ =
  assert_equal 1 @@ T.height t1;
  assert_equal 2 @@ T.height t2_out_of_order

let test_tree_is_balanced _ =
  assert_equal true @@ T.is_balanced t1;
  assert_equal false @@ T.is_balanced t3

let test_tree_to_list _ =
  assert_equal [] @@ T.to_list T.Leaf;
  assert_equal [ "a"; "d"; "e" ] @@ T.to_list t1

let test_tree_is_ordered _ =
  assert_equal true @@ T.is_ordered t1 ~compare:String.compare;
  assert_equal false @@ T.is_ordered t1_out_of_order ~compare:String.compare;
  assert_equal false @@ T.is_ordered t2_out_of_order ~compare:String.compare

let tree_tests =
  "Tree tests"
  >::: [
         "Tree.size" >:: test_tree_size;
         "Tree.height" >:: test_tree_height;
         "Tree.is_balanced" >:: test_tree_is_balanced;
         "Tree.to_list" >:: test_tree_to_list;
         "Tree.is_ordered" >:: test_tree_is_ordered;
       ]

let d1 =
  T.(
    Branch
      {
        item = { key = "d"; value = 0 };
        left =
          Branch { item = { key = "a"; value = 1 }; left = Leaf; right = Leaf };
        right =
          Branch { item = { key = "e"; value = 2 }; left = Leaf; right = Leaf };
      })

let example_dict1 =
  T.(
    Branch
      {
        item = { key = "9"; value = 1 };
        left =
          Branch
            {
              item = { key = "8"; value = 3 };
              left =
                Branch
                  { item = { key = "1"; value = 5 }; left = Leaf; right = Leaf };
              right = Leaf;
            };
        right = Leaf;
      })

let example_dict2 =
  T.(
    Branch
      {
        item = { key = "8"; value = 13 };
        left =
          Branch { item = { key = "1"; value = 2 }; left = Leaf; right = Leaf };
        right =
          Branch { item = { key = "99"; value = 2 }; left = Leaf; right = Leaf };
      })

let test_size _ =
  assert_equal 3 @@ D.size d1;
  assert_equal 0 @@ D.size Leaf

let test_to_list _ =
  assert_equal [ ("a", 1); ("d", 0); ("e", 2) ] @@ D.to_list d1;
  assert_equal [ ("1", 5); ("8", 3); ("9", 1) ] @@ D.to_list example_dict1

let test_lookup _ =
  assert_equal (Some 1) @@ D.lookup d1 ~key:"a";
  assert_equal None @@ D.lookup d1 ~key:"garbage"

let test_lookup_exn _ =
  assert_equal 1 @@ D.lookup_exn d1 ~key:"a";
  assert_raises (Invalid_argument "Key not found") @@ fun () ->
  D.lookup_exn d1 ~key:"garbage"

let test_insert _ =
  assert_equal
    T.(Branch { item = { key = "5"; value = 5 }; left = Leaf; right = Leaf })
  @@ D.insert T.Leaf ~key:"5" ~value:5;

  (assert_equal [ ("5", 5) ] @@ D.(insert T.Leaf ~key:"5" ~value:5 |> to_list));

  (assert_equal [ ("10", 10); ("5", 5) ]
  @@ D.(
       T.Leaf |> insert ~key:"10" ~value:10 |> insert ~key:"5" ~value:5
       |> to_list));

  (assert_equal [ ("10", 10); ("5", 5); ("8", 8) ]
  @@ D.(
       T.Leaf |> insert ~key:"5" ~value:5 |> insert ~key:"10" ~value:10
       |> insert ~key:"8" ~value:8 |> to_list));

  assert_equal [ ("10", 10); ("5", 5); ("8", 6) ]
  @@ D.(
       T.Leaf |> insert ~key:"5" ~value:5 |> insert ~key:"10" ~value:10
       |> insert ~key:"8" ~value:8 |> insert ~key:"8" ~value:6 |> to_list)

let test_map _ =
  assert_equal [ ("a", 11); ("d", 10); ("e", 12) ]
  @@ (d1 |> D.map ~f:(fun _ x -> x + 10) |> D.to_list);

  assert_equal [ ("a", 100); ("d", 100); ("e", 100) ]
  @@ (d1 |> D.map ~f:(fun _ _ -> 100) |> D.to_list)

let test_of_list _ =
  assert_equal d1 D.(d1 |> to_list |> of_list);

  assert_equal
    [ ("d", 0); ("e", 1); ("f", 2); ("g", 3) ]
    D.([ ("d", 0); ("e", 1); ("f", 2); ("g", 3) ] |> of_list |> to_list);

  assert_equal
    [ ("d", 0); ("e", 1); ("f", 2); ("g", 3) ]
    D.([ ("e", 1); ("d", 0); ("g", 3); ("f", 2) ] |> of_list |> to_list);

  assert_equal true
    ([ ("d", 0); ("e", 1); ("f", 2); ("g", 3) ] |> D.of_list |> T.is_balanced)

let test_of_list_multi _ =
  assert_equal
    [ ("hello", [ 0; 2 ]); ("world", [ 1 ]) ]
    D.([ ("hello", 0); ("world", 1); ("hello", 2) ] |> of_list_multi |> to_list);

  assert_equal
    [ ("a", [ 0; 1 ]); ("hello", [ 0; 2 ]); ("world", [ 1 ]) ]
    D.(
      [ ("a", 0); ("hello", 0); ("world", 1); ("hello", 2); ("a", 1) ]
      |> of_list_multi |> to_list);

  assert_equal true
    D.(
      [ ("a", 0); ("hello", 0); ("world", 1); ("hello", 2); ("a", 1) ]
      |> of_list_multi |> T.is_balanced)

let test_map_one _ =
  let d = D.map_one d1 ~key:"a" ~f:(fun _ x -> x + 10) in
  assert_equal 11 @@ D.lookup_exn d ~key:"a";
  assert_equal 2 @@ D.lookup_exn d ~key:"e"

let merge_fun l r =
  match (l, r) with
  | None, None -> failwith "should not get here!"
  | Some _, None -> 0
  | None, Some _ -> 1
  | Some a, Some b -> a * b

let merge_uniform l r = match (l, r) with _, _ -> 100

let test_merge _ =
  assert_equal
    [ ("1", 2); ("8", 13); ("9", 1); ("99", 2) ]
    D.(merge example_dict1 example_dict2 |> to_list);

  assert_equal
    [ ("1", 2); ("8", 13); ("99", 2) ]
    D.(merge Leaf example_dict2 |> to_list);

  assert_equal
    [ ("1", 2); ("8", 13); ("99", 2) ]
    D.(merge example_dict2 Leaf |> to_list)

let test_merge_with _ =
  assert_equal
    [ ("1", 10); ("8", 39); ("9", 0); ("99", 1) ]
    D.(merge_with ~merger:merge_fun example_dict1 example_dict2 |> to_list);

  assert_equal
    [ ("1", 100); ("8", 100); ("9", 100); ("99", 100) ]
    D.(merge_with ~merger:merge_uniform example_dict1 example_dict2 |> to_list);

  assert_equal
    [ ("1", 100); ("8", 100); ("99", 100) ]
    D.(merge_with ~merger:merge_uniform example_dict2 Leaf |> to_list);

  assert_equal
    [ ("1", 2); ("8", 13); ("99", 2) ]
    D.(merge Leaf example_dict2 |> to_list)

let dict_tests =
  "dict tests"
  >: test_list
       [
         "D.size" >:: test_size;
         "D.list" >:: test_to_list;
         "D.lookup" >:: test_lookup;
         "D.lookup_exn" >:: test_lookup_exn;
         "D.insert" >:: test_insert;
         "D.map" >:: test_map;
         "D.map_one" >:: test_map_one;
         "D.merge" >:: test_merge;
         "D.merge_with" >:: test_merge_with;
         "D.of_list" >:: test_of_list;
         "D.of_list_multi" >:: test_of_list_multi;
       ]

(* Add another suite for any of your part II functions needing testing as well.  Make sure to put those functions in utils.ml and headers in utils.mli as only libraries are unit tested; keywordcount.ml is an executable not a library. *)
let series = "Assignment2 Tests" >::: [ tree_tests; dict_tests ]
let () = run_test_tt_main series
