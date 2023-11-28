open Core

(*
  ----------
  BACKGROUND
  ----------

  In this assignment you will be refactoring a simple stateful OCaml program to use a state monad (a stateful stack in particular).

  Recall the monads lecture here:
  
  http://pl.cs.jhu.edu/fpse/lecture/encoding-effects.ml
  
  You also may want to re-watch some of the "Encoding Effects" lectures on Panopto, see Dateline for the dates.

  There are a few parts to this homework:
  0. We provide an example of a stateful monad to help you with the pattern.
  1. You will fill in the blanks for a stateful stack monad.
  2. You will use the monad to rewrite a function that previously used mutation.
  3. You will discuss the complexity of your monad and if it is any less efficient than the mutable solution.
  4. There is an optional extra credit problem to make your monad even more functional.

  We will provide some code snippets in this file (`monads.mli`) and some skeleton code in the
  ml file (`monads.ml`) for you to fill in. You may like to go back and forth between the two files
  to see our example implementations.
*)

(*
  -------
  EXAMPLE
  -------

  We provide a `State_int` module as an example. It is a simple state monad we used in lecture: the entire state was just one integer.
  It has been changed slightly here to remove the unit argument to `get` and `inc` because it is already frozen from the state argument.

  Refer to `monads.ml` for the implementation.

  See lecture notes for examples of using this monad.

  The `State_int` module is only an example to help you implement your stack monad later. Your stack monad will likely have a very similar
  structure, but it will not use `State_int` directly.
*)

module State_int : sig
  include Monad.S

  val run : 'a t -> 'a * int
  (** [run x] is the underlying data and the final state of the given monad value [x]. *)

  val set : int -> unit t
  (** [set i] is a monadic value with the new state [i]. *)

  val get : int t
  (** [get] is a monadic value whose underlying data is the state, and the state is unchanged. *)

  val inc : int t
  (** [inc] is a monadic value whose underlying data is the state + 1, and the state is incremented. *)
end

(*
  --------------
  STATEFUL STACK
  --------------

  Suppose instead of a single integer in our "state" as above, we wanted a stack. For this question you are
  to modify the above to make the state be a stack. The main differences are to replace the `int`'s in the
  monad type with a list to represent the stack, and then to replace set/get/inc with push/pop/is_empty
  operations on a stack. There is also one extra type parameter on the monad as we want our stacks to work
  on any type, not just a single type.

  We will use a list and the "cons" operation as our stack, so you might imagine an internal stack module to help.
*)

module Stack_monad : sig
  (*
      We are making a polymorphic stack (unlike the state monad which was monomorphic (int-only), for
      simplicity). This time, the stack holds values of any type 'e.

      But the monad still has underlying data of type 'a like the state monad. So the stack monad type
      depends on two different types, 'a and 'e, and it therefore must use Monad.S2 instead of Monad.S.
    *)
  include Monad.S2

  val run : ('a, 'e) t -> 'a
  (** [run x] takes [x] out of monad land by providing an initial empty state and throwing away the final state.
        Recall that 'a is the underlying data type, and 'e is the type of value in the stack. From this signature,
        we see that the data in the stack is thrown away, and the underlying data is kept. *)

  val push : 'e -> (unit, 'e) t
  (** [push x] is a monadic value that has [x] on the top of the stack. *)

  val pop : ('e, 'e) t
  (** [pop] is a monadic value whose underlying data is the top value of the stack, and the new state
        no longer has the top value. This may throw an exception when run. *)

  val is_empty : (bool, 'e) t
  (** [is_empty] is a monadic value whose underlying data is true if and only if the stack is empty *)
end

(*
  ---------------
  USING THE MONAD
  ---------------

  We'll now refactor a simple mutable-stack program into a program with the same structure
  and functionality by using the above stack monad in place of a mutable stack.

  The program checks if a string `s` has all parentheses '(' and ')' balanced. It uses the
  `Core.Stack` module, which is a mutable stack.
*)

val are_balanced_mutable : string -> bool
(** [are_balanced_mutable s] is true if and only if the string [s] is a string of opening '(' and
    closing ')' parentheses that are balanced.

    See `monads.ml` for the implementation. *)

val are_balanced_monadic : string -> bool
(** [are_balanced_monadic s] is the same as [are_balanced_mutable s] but uses no mutation.
    
    See `monads.ml` for some helper functions, and complete them with your implementation. *)

(*
  ----------   
  DISCUSSION
  ----------

  See the file `discussion.txt` for this section. 
*)

(*
  ------------
  EXTRA CREDIT   
  ------------

  For extra credit, make an `Exception_stack` monad which wraps the stack in an exception monad:
  if pop is attempted on an empty stack, a monadic exception will be generated.
  
  When you have your monad, re-write `are_balanced_monadic` as `are_balanced_more_monadic` so it
  can avoid any OCaml exceptions. You are not to raise any exceptions or use `failwith`; your
  solution must be entirely functional.

  Hint: try changing the returned type of `run`.

  Note: You must name the monad `Exception_stack` and you must name the function
    `are_balanced_more_monadic` for the autograder to properly test your extra credit work.
*)

(*
module Exception_stack :
  sig
    include Monad.S2
    val run : ('a, 'e) t -> (* your choice of return type *)
    val push : 'e -> (unit, 'e) t
    val pop : ('e, 'e) t
    val is_empty : (bool, 'e) t
  end

val are_balanced_more_monadic : string -> bool
*)
