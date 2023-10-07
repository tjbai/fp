(* *********************************************************** *)
(* ********************** P A R T I ************************** *)
(* *********************************************************** *)

(*
  See group.mli before working with this file. You will create a ring.ml file to
  meet this signature.

  In ring.ml, you will implement one example of a ring. A ring is a set with two
  operations: addition and multiplication.

  As we did with groups, we suggest you refer to the ring (mathematics)
  Wikipedia for more background if you'd like it.

  Instead of converting to and from integers, we'll be more general and allow a
  string representation of the ring elements. This does not imply that the internal
  representation will be a string -- rather, strings will only be used for
  interfacing with the module.
*)

(*
  module type S is the module signature for a ring, similar to Finite_group.S.

  Here is an overview of the ring:
  * It has some `type t`, which is arbitrary and depends on the implementation.
    The type should be whatever is most natural for the desired operations.
  * There is an addition operation.
  * There is a multiplication operation.
  * Ring elements can be created from strings using `of_string`, but if it's not
    possible to get an element from the string, then it returns `None`.
    For example, if we are working with an integer ring, then
     `of_string "4"` => `4 : int` 
     `of_string "hello world"` => `None`
  * Ring elements can be converted back to strings.

  It might also be natural to compare ring elements, but for our basic usage of
  these modules in Part 2, we don't need `compare`.
*)
module type S = sig
  type t

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val of_string : string -> t option
  val to_string : t -> string
end

(*
   We'll implement one simple ring in `ring.ml`. This should be fast and easy
   compared to the exercises given in `finite_group.mli`.

   The ring we'll implement in this file is the integers modulo n.
   e.g. If n=4, then Z/4Z is the set {0, 1, 2, 3} with addition and
     multiplication operators defined naturally as in modular arithmetic.

   You will implement the `Make_modular_ring` functor that takes a module with
   an integer `n` that determines the size of the ring.
   * Addition is regular modular addition of integers mod n
   * Multiplication is regular modular multiplication of integers mod n
   * of_string will convert a string to an integer in Z mod n if possible; `None` if not.
   * to_string will convert an integer to a string representation.
*)
module Make_modular_ring (_ : sig
  val n : int
end) : S

(*
  Now use your functor above to make the integers mod 4.
*)
module Z4 : S

(*
  See `postfix_calc.mli` (Part 2 of this assignment) for some more rings.

  Since these .mli will not be submitted as part of your solution, the rings we
  make in part 2 will not be implemented here (or in `ring.ml`), but instead
  will be in the part 2 file `postfix_calc.ml`.
*)
