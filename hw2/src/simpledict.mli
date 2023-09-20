(* 

Part I: A Binary Tree Based Dictionary

This file specifies the interface for your code and must not be edited (it will also not be included in your zip submission). 
Your actual implementation should go in the file `simpledict.ml`  which satisfies this interface appropriately.

Mutation operations of OCaml are not allowed or required.

*)

module Tree : sig
  type 'a t =
    | Leaf
    | Branch of
        { item:  'a
        ; left:  'a t
        ; right: 'a t }

    (*
        Return the number of branch items in the tree. Leaf nodes do not count towards size.   
    *)
    val size : 'a t -> int

    (* 
       Return the depth of the deepest non-Leaf node. The depth of the root node is zero.

       If a tree is *only* a Leaf, then say it has height -1 because a Leaf is not a "real node".
    *)
    val height : 'a t -> int

    (*
        Decide if a tree is balanced. A tree is balanced if for every branch in the tree, the height
        of the left and right subtrees does not differ by more than 1.

        It comes in handy here to say a Leaf has height -1.
    *)
    val is_balanced : 'a t -> bool

    (*
        Given a dict, flatten it into a list of items which retains the tree's ordering (the 'inorder' traversal). This should take O(n) time; recall that each (::) is O(1) and each (@) is O(n).
    *)
    val to_list : 'a t -> 'a list

    (*
        Check whether a tree is *ordered*, i.e. that for every branch in the tree, all left subtree items are strictly less than the branch item, and all right subtree items are strictly greater.

        Note that this requirement guarantees (by induction) that the tree has no duplicate items.
    *)
    val is_ordered : 'a t -> compare:('a -> 'a -> int) -> bool

end (* module Tree *)

module Dict_item : sig
    (*
        The Dict_item module describes a type so that a Tree can represent a dictionary. 
        The type holds a key and a value.
    *)

    (* 
        For simplicity, we will restrict the keys to be `string`s only; the values are type 'a.   
    *)
    type 'a t = { key: string ; value: 'a }

    (*
        Our `compare` will only compare the keys. Values are ignored. This is implemented for you.
    *)
    val compare : 'a t -> 'a t -> int

end (* module Dict_item *)


(* 
    The dict type will be a tree of Dict_item.t.

    Note that because this type is a tree, all Tree module functions will work on it.

    You may ignore the `[@@deriving show]`. You will not need to work with it, and it is to pretty-print your autograder results.
*)
type 'a t = 'a Dict_item.t Tree.t [@@deriving show]

(*
    We will now define several natural operations on these dictionaries.

    We will implicitly require all dicts provided to and created by the functions below to obey the
    `Tree.is_ordered` requirement. You do not need to check that the dicts are ordered when they are
    provided to a function.
*)

(*
    Return the number of key, value pairs in the given dictionary.
*)
val size : 'a t -> int

(*
    Given a dict, flatten it into a list of (key,value) pairs which retains the tree's ordering (the 'inorder' traversal). This should take O(n) time; recall that each (::) is O(1) and each (@) is O(n).
*)
val to_list : 'a t -> (string * 'a) list

(*
    Given a string and a dictionary, look up the associated value, if any. Your implementation must be O(log n) on average since you can take advanatge of the is_ordered requirement.
*)
val lookup : 'a t -> key:string -> 'a option

(*
    Look up the value for the given key, raising an exception if it's not found.
*)
val lookup_exn : 'a t -> key:string -> 'a

(*
    Given a string key and a value, insert the pair into the dictionary, overwriting any existing value attached to the key in the dict if present.  This should also be O(log n) as we will cover in lecture.
*)
val insert : 'a t -> key:string -> value:'a -> 'a t

(*
    STAFF NOTES:
    Issue: if we have them make this, then they're encouraged to always map a dict
        to a list, use the List module functions, and them map back to a dict. Is this
        really how we want them to solve all the remaining problems? This could be amended
        by requiring a specific time complexity and assuming they can't solve this in linear time.
    Note: This concern has been amended by the "IMPORTANT" statement below.

    Given a list of (key, value) pairs where each key is unique, create an ordered balanced dict.
    The resulting dict must pass `Tree.is_balanced`.

    Note: this is not precisely the inverse of `to_list` because tree structure is ambiguous.

    IMPORTANT: this function `of_list` may not be used as a helper function anywhere else in this
    file `simpledict.mli` except for in `of_list_multi`. No points will be awarded to any other function
    using `of_list`.
*)
val of_list : (string * 'a) list -> 'a t

(*
    Given a list of (key, value) pairs, create an ordered balanced tree where each key maps to a list of values.
    The resulting dict must pass `Tree.is_balanced`.

    e.g. of_list_multi [("hello", 0); ("world", 1); ("hello", 2)] gives a tree that looks like
        { key:"hello" ; value:[0; 2] }
            /               \
          Leaf         { key:"world" ; [1] }
*)
val of_list_multi : (string * 'a) list -> 'a list t

(*
    Given a dict and some transforming operation, apply the operation to each value within the dictionary to produce a new dict, but keeping the keys constant. Note that the mapping function f can use the key in its calculation so we also pass it as an argument.
*)
val map : 'a t -> f:(string -> 'a -> 'b) -> 'b t

(*
    Given a dict and some transforming operation, apply the operation to the value in the dictionary corresponding to exactly the given key.
    If the key does not exist in the dict, then return the original dict.

    e.g. [("hello", 0); ("world", 1)]
        |> of_list
        |> map_one ~key:"world" ~f:(fun _ x -> x + 10)
        |> to_list
        evaluates to [("hello", 0); ("world", 11)]

    Hint: use lookup and insert.
*)
val map_one : 'a t -> key:string -> f:(string -> 'a -> 'a) -> 'a t

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    If both contain a value associated to a key, retain the second dict's value in the result.
*)
val merge : 'a t -> 'a t -> 'a t

(*
    Given two dictionaries, merge them into one, preserving the ordering property.
    Compute the new values for each dictionary key using the supplied merger 
    function, feeding it Some or None depending on the presence of each key in the first and second dict.

    Note since we have a merger function here the dictionaries may in principle have different types.
*)
val merge_with : 'a t -> 'b t -> merger:('a option -> 'b option -> 'c) -> 'c t
