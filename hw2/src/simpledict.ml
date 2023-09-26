(*

FPSE Assignment 2

Name                  : TJ Bai
List of Collaborators :

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.

See file simpledict.mli for the specification of Part I of the assignment, and keywordcount.ml for Part II.  Recall from lecture that .mli files are module signatures aka module types and you will need to provide implementations of all the functions listed there in this file. 

Your Part I answers go here, and the Part II application should go in the keywordcount.ml file. No helper functions for Part II should exist in this file beyond what is required for Part I.

Hint: start by reading the code we've put here, and then copy over parts of the .mli file to make dummy headers and fill them with `unimplemented ()`. Note the changes in syntax between .mli and .ml files.

Note that .ml files need to include all `type` declarations in .mli files.

You must leave all `[@@deriving show]` annotations, or your autograder won't work. We use this to pretty-print your results.

*)

open Core

module Tree = struct
  type 'a t = Leaf | Branch of { item : 'a; left : 'a t; right : 'a t }
  [@@deriving show]

  let max (a : int) (b : int) = if a >= b then a else b
  let abs (a : int) : int = if a < 0 then -a else a

  let rec size (tree : 'a t) : int =
    match tree with
    | Leaf -> 0
    | Branch { left; right; _ } -> 1 + size left + size right

  let rec height (tree : 'a t) : int =
    match tree with
    | Leaf -> -1
    | Branch { left; right; _ } -> 1 + max (height left) (height right)

  let rec is_balanced (tree : 'a t) : bool =
    match tree with
    | Leaf -> true
    | Branch { left; right; _ } ->
        is_balanced left && is_balanced right
        && abs (height left - height right) <= 1

  let to_list (tree : 'a t) : 'a list =
    let rec aux (tree : 'a t) (acc : 'a list) : 'a list =
      match tree with
      | Leaf -> acc
      | Branch { item; left; right } -> aux left (item :: aux right acc)
    in

    aux tree []

  let to_list_explicit (tree : 'a t) ~(serialize : 'a -> 'b) : 'b list =
    let rec aux (tree : 'a t) (acc : 'b list) : 'b list =
      match tree with
      | Leaf -> acc
      | Branch { item; left; right } ->
          aux left (serialize item :: aux right acc)
    in

    aux tree []

  let is_ordered (tree : 'a t) ~(compare : 'a -> 'a -> int) : bool =
    let rec aux (tree : 'a t) (min : 'a option) (max : 'a option) : bool =
      match tree with
      | Leaf -> true
      | Branch { item; left; right } -> (
          let aux_next =
            aux left min (Some item) && aux right (Some item) max
          in
          match (min, max) with
          | Some a, Some b ->
              if compare item a > 0 && compare item b < 0 then aux_next
              else false
          | Some a, None -> if compare item a > 0 then aux_next else false
          | None, Some b -> if compare item b < 0 then aux_next else false
          | None, None -> aux_next)
    in

    aux tree None None
end
(* module Tree *)

module Dict_item = struct
  type 'a t = { key : string; value : 'a } [@@deriving show]

  let compare (x : 'a t) (y : 'a t) : int = String.compare x.key y.key
  let serialize (x : 'a t) : string * 'a = (x.key, x.value)
end
(* module Dict_item *)

type 'a t = 'a Dict_item.t Tree.t [@@deriving show]

let size (dict : 'a t) : int = Tree.size dict

let to_list (dict : 'a t) : (string * 'a) list =
  Tree.to_list_explicit dict ~serialize:Dict_item.serialize

let rec lookup (dict : 'a t) ~(key : string) : 'a option =
  match dict with
  | Leaf -> None
  | Branch { item; left; right } ->
      if String.compare item.key key = 0 then Some item.value
      else if String.compare item.key key < 0 then lookup right ~key
      else lookup left ~key

let rec lookup_exn (dict : 'a t) ~(key : string) : 'a =
  match dict with
  | Leaf -> raise (Invalid_argument "Key not found")
  | Branch { item; left; right } ->
      if String.compare item.key key = 0 then item.value
      else if String.compare item.key key < 0 then lookup_exn right ~key
      else lookup_exn left ~key

let rec insert (dict : 'a t) ~(key : string) ~(value : 'a) : 'a t =
  match dict with
  | Leaf -> Branch { item = { key; value }; left = Leaf; right = Leaf }
  | Branch { item; left; right } ->
      if String.compare item.key key = 0 then
        Branch { item = { key; value }; left; right }
      else if String.compare item.key key > 0 then
        Branch { item; left = insert left ~key ~value; right }
      else Branch { item; left; right = insert right ~key ~value }

let rec create_balanced_tree (ls : (string * 'a) list) : 'a t =
  match ls with
  | [] -> Leaf
  | [ (key, value) ] ->
      Branch { item = { key; value }; left = Leaf; right = Leaf }
  | _ -> (
      match List.split_n ls (List.length ls / 2) with
      | a, (key, value) :: bt ->
          Branch
            {
              item = { key; value };
              left = create_balanced_tree a;
              right = create_balanced_tree bt;
            }
      | _, [] -> assert false)

let of_list (ls : (string * 'a) list) : 'a t =
  ls
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> create_balanced_tree

let of_list_multi (ls : (string * 'a) list) : 'a list t =
  ls
  |> List.Assoc.sort_and_group ~compare:String.compare
  |> create_balanced_tree

let rec map (dict : 'a t) ~(f : string -> 'a -> 'b) : 'b t =
  match dict with
  | Leaf -> Leaf
  | Branch { item; left; right } ->
      Branch
        {
          item = { key = item.key; value = f item.key item.value };
          left = map left ~f;
          right = map right ~f;
        }

let map_one (dict : 'a t) ~(key : string) ~(f : string -> 'a -> 'a) : 'a t =
  match lookup dict ~key with
  | None -> dict
  | Some value -> insert dict ~key ~value:(f key value)

let rec merge (a : 'a t) (b : 'a t) : 'a t =
  match b with
  | Leaf -> a
  | Branch { item; left; right } ->
      let merged_left = merge a left in
      let merged_right = merge merged_left right in
      insert ~key:item.key ~value:item.value merged_right

let merge_with (a : 'a t) (b : 'b t) ~(merger : 'a option -> 'b option -> 'c) :
    'c t =
  let new_a = map a ~f:(fun key item -> merger (Some item) (lookup b ~key)) in
  let new_b = map b ~f:(fun key item -> merger (lookup a ~key) (Some item)) in
  merge new_a new_b
