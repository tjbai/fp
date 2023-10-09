(* ************************************************************ *)
(* ********************** P A R T II ************************** *)
(* ************************************************************ *)

(*
  -----
  SETUP
  -----

  This file `postfix_calc.mli` holds the module signatures that you will
  implement in `postfix_calc.ml`. The expected behavior is explained here.
  You are to implement everything defined and explained here. After part 1,
  you should be comfortable with the syntax, so we leave it all to you with
  no starter code.
  
  Read this entire file before you begin to code in `postfix_calc.ml`.

  You will make the `postfix_calc.ml` file to meet this signature.


  ----------
  BACKGROUND
  ----------

  In this part of the assignment, we will develop a generic operator language
  parser / evaluator. We will make it generic by allowing us to "plug in" the
  underlying data type.
  
  Note we will simplify several things so that this is primarily an OCaml
  abstraction exercise and not a parsing exercise. The goal is that this gives
  you more practice with modules and functors and helps you feel more comfortable
  with them--especially how they exist in .mli and .ml files. 
  
  The language is an expression with only `+` and `*` binary operators (think "ring").
  We will parse a string that contains the entire expression. For simplicity, we use the
  postfix (aka RPN) notation:
    e.g. "1 2 +" will evaluate to 3.
    e.g. "1/2 1/3 *" will evaluate to 1/6.
*)

(*
   `Data` is the module type for the underlying data in the expression. It is an
   extension on the Ring module type from the first part of this assignment. The
   key extension is `next`. It reads a `t` off of the front of the string and
   returns the remainder of the string as well as the `t` element.

   Here are some clarifications on how `next` works.
   1. whitespace (space, tab, newline) is a separator; the `t` value ends at that point.
   2. `next` is only responsible for reading a `t` off the front of the string and
     removing it from the front of the string.
   3. It should obey the "maximal munch" principle: read in as many characters as possible
     while still making a `t`. Here are some examples on integers:
     a. `next "12"` => `Some ("", 12)`. It does not evaluate to `Some ("2", 1)`
     b. `next "12@"` => `Some ("@", 12)`.
     Whitespace is a separator, but it is not the only separator. Consider anything that does not directly
     contribute to `t` a separator.
     Points 4 and 5 discusses the error cases.
   4. `next` returns `None` if and only if no possible `t` can be pulled off the front of the string.
   5. `next` will return `None` in four cases:
       a. end of string
       b. an illegal character, e.g. `@`
       c. an operator (`+` and `*` only)
       d. an illegal type (this is defined based on the concrete data type we are implementing)
*)

module type Data = sig
  include Ring.S (* Has everything that ring has, and more! *)

  val next : string -> (string * t) option
end

(*
   The Evaluator for this simple language is then a functor of this Data type. It
   evaluates expressions in the postfix language. It uses `Data.next` to get the
   next value in the input string and does any necessary operations. If the
   entire input cannot be parsed, `eval` will return `Error <error message>`;
   otherwise it will return `Ok <t-value>`.

   Clarifications:
   1. If an illegal character or illegal type is encountered at any point, the evaluator will return `Error "illegal character"`.
   2. If there are too few or too many operators (as in "1 2 + +" or "1 2") return `Error "unmatched"`.
   3. Eval called on empty string should also return `Error "unmatched"`.
   4. Note that operators need not be space-separated, e.g. "1 2 3++" returns `6`.
   5. '+' and '*' are the characters corresponding to the Ring binary operations.
     There are no other operations supported, and all others are illegal characters.
*)
module type Eval = sig
  type t

  val eval : string -> (t, string) result
end

(*
  You will implement Make_eval, which takes the module of type Data and
  implements all functions in the Eval module type accordingly.

  Note that the inner type `t` must be the same as the given `Data.t`.
*)
module Make_eval (Data : Data) : Eval with type t = Data.t

(*
  You will implement the Z4_data, Int_data, and Rat_data modules for parsing integers and rationals.
  These modules are of module type Data. The addition and multiplication operators are the
  natural operators on integers and rationals.
  See the clarifications above module type Data for the general behavior of the functions.

  Below, we have some clarifications for how integers and rationals specifically may be represented in the input string.

  Integers:
  - Integers may be optionally signed, so "-4" is an integer. Note that a "+" is still an
    operator, however, and does not denote a positive integer. Integers are positive by default.
  - Negative signs will never be separated from the integer by whitespace. If they are, then it is invalid.
    i.e. `next "- 1"` will give `Error "some error message"`, but `next "-1"` will give `Ok (-1)`.
  - The same goes for the Z4 ring.
    - As an example, `next "5"` => `None` because no element in Z4 can be parsed from "5". Consequently
      `Z4_eval.eval "5"` => `Error "illegal character"`.

  Rationals:
  - Rationals are written as "3/4", integers separated by a "/"
  - Rationals may also be optionally signed. However, the negative sign is only on the numerator
    and is not separated by whitespace from the numerator.
    e.g. "-1/3" is allowed
    e.g. "1/-3" or "-1/-3" is not allowed, and no rational can be read from either string.
    e.g. "- 1/3" is not allowed.
  - Division by 0 is not allowed.
    e.g. no rational can be read from "5/0". Eventually, the evaluator would interpret this as an illegal character.
  - When a rational is returned from Eval, it must be in simple fractions. Therefore, when a rational is
    read from a string, it should be reduced.
    e.g. `next "3/6"` gives `Ok (1/2)`
*)

(*
  IMPLEMENTATION REQUIREMENT:
    We won't expose it here, but you must use a functor to create the Data
    module from the Ring module. Think about how you can use `of_string` to
    implement `next`.  It is up to you to create and use such functor. We
    suggest it is called `Make_data`, and it is called on modules such as
    `Ring.Z4` or `Rat_ring` (where you'll define operations on rational
    numbers). You can define the necessary rings here and not in `Ring.ml`
    because they will be hidden to the user.
  
  WE WILL CHECK BY HAND THAT YOU USED A FUNCTOR TO MAKE THE DATA MODULES FROM THE RING MODULES.
*)

module Make_data (Ring : Ring.S) : Data with type t = Ring.t
module Z4_data : Data
module Int_data : Data
module Rat_data : Data

(*
  With this, we may now create evaluators for Z4, integers, and rationals.
  These will be created in postfix_calc.ml with the Make_eval functor.   
*)
module Z4_eval : Eval with type t = Z4_data.t
module Int_eval : Eval with type t = Int_data.t
module Rat_eval : Eval with type t = Rat_data.t

(*
  Int_Eval.eval "2 3 +" should now return `Ok (5)`.  Make sure to 
  write a good array of such tests in `tests.ml` to make sure your
  implementation is working.
*)
