(* *********************************************************** *)
(* ********************** P A R T I ************************** *)
(* *********************************************************** *)

(*
  ----------
  MOTIVATION
  ----------
   
  This assignment covers many of the topics in the "More Modules" lecture, so you
  will want to review that for lots of hints of how to proceed here.

  One thing which is ubiquitous in many object-oriented languages is the ability
  to program against an interface and not an underlying class or type, enabling
  dependency injection, easier unit testing, and encapsulation of implementation
  details

  We've seen in OCaml how to use polymorphic functions (those involving types
  like 'a) to operate on any kind of data, but in this case we need to know
  _nothing_ about the object.  We've also seen how to hide parts of our
  implementation by not including certain details about types in our .mli files
  or module signatures (aka module types), and not including functions which are
  only meant to be called by some more limited exposed API.  But how can we
  write functions which operate on types which have a specific interface, rather
  than "any type"? Imperative languages would often use traits or interfaces for
  this, or simply dynamic dispatch and class hierarchies. What's idiomatic in
  OCaml?

  It turns out that module types and functors do the trick of naming our
  dependencies and specifying exactly what can be relied upon.

  Better than that, we aren't restricted to specifying only one "class" and its
  interfaces, we can ask for any number and kind of functions operating over
  multiple types and produce an entire module which makes use of those
  behaviors.

  Using functors in this way decouples our code from the implementation of
  functionality it depends on and from the representation of objects which it
  uses, allowing a very powerful kind of dependency injection.

  
  ----------
  BACKGROUND
  ----------
  
  In this assignment, you will implement a functor to define a finite
  mathematical group.  A group is a set with an operation with three conditions:
  1. There is an identity element.
  2. Every element has an inverse.
  3. The operation is associative.

  You can check out the Wikipedia page for a group (mathematics) for some more background,
  but we think you can get by with just the assignment description.

  We will keep our group simple and make an "integers mod n" i.e. "Z mod n"
  group with some arbitrary operation (like addition). This way, we can create a
  group by giving it a size and an operation on two integers. However, we won't
  promise that the operation is always as easy as addition.

  Then, we will memoize the results in a new module for faster computation.

  After that, for just slightly more practice with modules and a connection to Part 2, you
  will implement a Ring module in the ring.mli file.


  ------------
  MODULE TYPES
  ------------
  
  A module type is a signature for a module. It is much like defining a type `t`.
  Here are two code fragments to serve as an example.
  This first one is something you've seen before in a .mli file that defines a type t
  and claims that some value `n` with type t will be implemented in the .ml file.
  ```
  type t = int
  val n : t
  ```
  And here is an example of a module type T and a module `M` that has type T.
  ```
  module type T = sig type t end
  module M : T
  ```
  In these examples, `t` is analogous to `T`, and `n` is analogous to `M`.

  This promises that the module type T will be defined again in the .ml file in the exact
  same way, and that some module M will have the signature of module type T. In this example,
  it is only promised that `M` will have a `type t` within it.

  Just like every value has a type, every module has a module type. In the .mli file here,
  we declare what that module type is for each module.


  --------
  FUNCTORS
  --------

  Functors are functions on modules. They take a module as an argument, and they
  "return" a module.  Here is an example of a functor signature:
  ```
  module type T = sig type t end
  module type S = sig type t val n : t end
  module F (_ : T) : S
  ```
  This promises that a functor F will be implemented in the .ml file, and it
  will take an argument that has module type T, and the "returned" module will
  have module type S.

  Functors were covered in the More Modules lecture. For an example, see the
  set-example-functor at
    https://pl.cs.jhu.edu/fpse/examples/set-example-functor.zip
  which was also linked in the lecture. This example shows the ".ml-side" of the
  picture, so you can see how the functors here might be implemented.

  With this, we should be ready to carry on to the assignment.
*)

(*
  module type S is a signature for the finite group module (we are within
  `finite_group.mli`, and we use S to refer to its signature because the
  signature will be needed for a functor).

  Note that the type is hidden, so the user can't combine elements from
  different groups.

  This group lets the user operate on elements, invert them, compare them, and
  convert them to and from integers because we assume the elements are ordered.
  There is also an identity element `id` in the group.

  NOTES:
  * type t is always going to be int, but it's hidden. It's hidden so that a
    user can't operate on it like an integer and break the rules of the group.
  * `of_int k` would give the k'th element in the group if it exists, and `None`
    if there is no k'th element.  The k'th element will always exist for k in
    0..(n-1) inclusive for group of size n. 

  There is more help below (above Make) on implementation.
*)
module type S = sig
  type t [@@deriving compare, sexp]

  val id : t
  val op : t -> t -> t
  val inverse : t -> t
  val of_int : int -> t option
  val to_int : t -> int
end

(*
  Finite groups are described uniquely by their operation and their size. The
  operation and size are provided within the module type Params. We are limiting
  ourselves to finite groups whose underlying type is an integer. These groups
  are "Z mod n" with some arbitrary operation.

  The operation here might be something like Int.(+), so it is the job of the
  group to mod the result by n to ensure that the result of the operation is
  still within the group.

  For example, consider a group of size 5 where the given operation is Int.(+).
  Then the elements 3 and 4 are in the group. The operation will yield
  `Int.(3 + 4)` => `7`, but the group will need to represent this as 
  `2` (~= 7 mod 5) because it will always module by n after performing the operation.

  Here is what we promise about the operation:
  * It is associative
  * It will always yield an inverse for every element
  * It will always yield an identity element
  * The result will always be non-negative if the inputs are non-negative

  Nothing else can be promised about the operation.
*)
module type Params = sig
  val op : int -> int -> int
  val n : int
end

(*
  Here is our first module (as opposed to a module type). It is a functor that
  takes some parameters and makes a module with signature S. i.e. it makes a
  group.
  
  All values in module type S must be implemented.

  Here are some tips:
  * Let `type t = int` because you are provided an operation on two integers.
  * Use Params.op and modulo by `n` to implement the group operator `op`.
    Since our type is an integer in "mod n", always modulo by `n` after the operation.
  * The identity element `id` is such that for all x : t in the group, `op id x`
    and `op x id` both return `x` where `op` is the group operator.
    In "Z mod 5" with the "+" operation, the identity element is 0 because
      (x + 0) mod 5 = x  and  (0 + x) mod 5 = x
    for all x.
  * The inverse element `x'` is such that `op x' x` and `op x x'` both return `id`.
    For example, in "Z mod 5" with the "+" operation, the inverses of
      {0,1,2,3,4}
    are
      {0,4,3,2,1}
    respectively because (2 + 3) mod 5 = 0, and similarly for the other elements.
  * You will have to search the set of possible elements to find the inverse and identity.
    Upon creation of the module, search for the identity element by operating on all
    possible pairs of elements. The identity will be unique, and we promise you'll find it
    if you search correctly.
    Do not memoize the inverse of each element, but instead search for it each time
    the function is called.
*)
module Make (_ : Params) : S

(*
  The example usage will be the Z mod 5 group with the addition operation.
  Define the Z5 params with n = 5 and op = Int.(+), then call the Make
  module on these parameters to implement this module.
*)
module Z5_add : S

(*
  Now what if the operation is really time-consuming?  In this case, it's hard
  to find the inverse to an element because we must laboriously search the
  entire space for an inverse.

  We will create a module to memoize the inverse for each element. We expect all
  results to be precomputed upon creation of the module and not become saved only
  once the functions are called.
  * If the group is of size `n`, then the space complexity of the module itself
    will be O(n) because you must store the inverse for each of the `n` elements.
  * The time complexity of the `inverse` function will be no worse than O(log n).

  Start by including the given module `G` because almost all parts will be the same:
    `include G`
  This will copy over everything from `G` into `Memoize`.
  Then go about memoizing `inverse`.

  We suggest you use a Map to memoize the results. Since the module type S has
  `compare` and `sexp` functions derived, it can be used as a key to a map.
  Create a map using `Map.Make (G)` and map each element to its inverse. Then
  implement `inverse` using this map, overwriting the inverse included from G.

  Note that the type of the returned module must be the same as the type of the
  argument module because we claim the module returned is `S with type t = G.t`.
  This will be handled by `include G`.
*)
module Memoize (G : S) : S with type t = G.t
