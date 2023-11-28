open Core

[@@@warning "-27"]
[@@@warning "-32"]

(*
  Please refer to `monads.mli` for most of the assignment description.   
*)

(*
  -------
  EXAMPLE
  -------

  Here is an implementation of the `State_int` monad. Your stack monad later will likely have a very similar structure, but
  it will not use `State_int` directly.
*)

module State_int =
  struct
    module T : Monad.Basic with type 'a t = int -> 'a * int =
      struct
        type 'a t = int -> 'a * int
        (** The type is a function, so we can *thread* the int through computations. 
            Pass the int in like `Reader` and return it like `Logger`. *)

        (*
          Let us now construct bind.
          1) Like Reader, the result is a `fun (i : int) -> ...` since we pass in `i`.
          2) First we pass `i` to the first computation `x`.
          3) `x` returns a pair with a potentially **different** state, `i'`, and a new underlying data `x'`.
          4) Now thread the state onto `f` to be truly stateful.
        *)
        let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t =
          fun (i : int) ->
            let x', i' = x i in
            (f x') i'

        (* To return a value, just wrap it with no state change. It takes a state `i` and returns the same state. *)
        let return (x : 'a) : 'a t = fun i -> (x, i)

        (* We let `Monad.Make` cover this for us. *)
        let map = `Define_using_bind
      end

  include T
  include Monad.Make (T)

  (* Satisfy the rest of the signature... *)

  (* Run needs to pass in an initial i, 0 *)
  let run (i : 'a t) : 'a * int = i 0

  (* return `()` as value and CHANGE state to `i` *)
  let set (i : int) : unit t = fun _ -> (), i

  (* return the state `n` AND propagate `n` as state *)
  let get : int t = fun n -> (n, n)

  (* Lets also build in ++ for fun *)
  let inc : int t = fun n -> (n + 1, n + 1)
end

(*
  --------------
  STATEFUL STACK
  --------------

  You will implement the skeleton below to write a stateful stack monad.

  We give you the skeleton with type signatures below for you to fill in.

  Note that you CANNOT use any OCaml data structures besides List and Option.  No mutable structures in particular!
*)

module Stack_monad :
  sig  
    include Monad.S2
    val run : ('a, 'e) t -> 'a
    val push : 'e -> (unit, 'e) t
    val pop : ('e, 'e) t
    val is_empty : (bool, 'e) t
  end
  =
  struct
    (*
      We believe in abstraction and writing helper types, so you can optionally write a stack to help write the monad.

      We provide a (basically) useless one that you can expand on.

      This Stack module is not visible to any user, and the module type signature above ensures this.
    *)
    module Stack =
      struct
        type 'e t = 'e list (* this is our "heap" type. *)
      end

    module T : Monad.Basic2 with type ('a, 'e) t = 'e Stack.t -> 'a * 'e Stack.t =
      struct
        type ('a,'e) t = 'e Stack.t -> 'a * 'e Stack.t
        (* 'a is the underlying value in the monad, 'e is type of stack elements *)

        let bind (x : ('a,'e) t) ~(f : 'a -> ('b,'e) t) : ('b,'e) t =
          fun (s : 'e Stack.t) ->
            failwith "FILL IN"

        let return (x : 'a) : ('a, 'e) t =
          fun (s : 'e Stack.t) ->
            failwith "FILL IN"

        let map = `Define_using_bind
      end

    include T
    include Monad.Make2 (T) (* Make2 is like Make but for TWO type variable parameters *)

    (* Satisfy the rest of the module signature... *)

    (*
      Run puts us in x's monad-land with an empty stack. Unlike with the int state
      monad above, just throw away the final stack here.

      We implement this for you.
    *)
    let run (x : ('a,'e) t) : 'a = match x [] with a, _ -> a

    (* `push` should "push" the element on the stack and return `()` as the value *)
    let push (x : 'e) : (unit,'e) t =
      fun (s : 'e Stack.t) ->
        failwith "FILL IN"

    (*
      `pop` should pop off and return the top element, i.e. the list head.
      Note for now if the stack was empty you can just `failwith "empty pop"`.
    *)
    let pop : ('e, 'e) t =
      fun (s : 'e Stack.t) ->
        failwith "FILL IN"

    let is_empty : (bool,'e) t =
      fun (s : 'e Stack.t) ->
        failwith "FILL IN"
  end

(* 
  Note that having pop raise an exception on an empty stack is a bit of a cop-out,
  we did not get rid of all effects.  To "do stack 100% monadically" we would
  need to wrap the above in an exception monad. Shudder! We will spare you the pain.
*)

open Stack_monad
open Stack_monad.Let_syntax

(* Here is a simple example based on how we used the integer state monad *)
(* Note this doesn't run yet, the state monad puts a `fun ...` around it all *)
let simple_stack : ('a,char) t =
  let%bind () = push 'a' in
  let%bind () = push 'b' in
  let%bind () = push 'c' in
  let%bind c = pop in
  return Char.(c = 'c')
;;

(* This will now run the above and assert it worked correctly. *)
let r = run simple_stack in
assert r

(*
  ---------------
  USING THE MONAD
  ---------------

  Check out our implementation of `are_balanced_mutable`. Note it returns false if we catch
  an exception from an illegal pop (i.e. a pop on an empty stack).
*)

let are_balanced_mutable s =
  let stack_of_lefts = Stack.create () in
  let match_with s c = Char.(c = Stack.pop_exn s) in
  let parse = function
    | '(' -> Fn.const true @@ Stack.push stack_of_lefts '('
    | ')' -> match_with stack_of_lefts '('
    | _ -> true
  in
  try
    let r = String.fold ~init:true ~f:(fun b c -> b && parse c) s in
    r && Stack.is_empty stack_of_lefts
  with _ -> false

(*
  Now for the exercise:

  Rewrite the above function by turning all of the mutable stack operations into
  Stack_monad ones. You can still use the try/with because `Stack_monad.pop` may
  raise an exception that needs to be caught. However, you may not use any mutable
  state. You must use `Stack_monad` for all stack operations, and you must write the
  program monadically.

  To make things easier we will extract some of the auxiliary functions we had
  above as separate functions with types declared for your benefit.  Pay close
  attention to those types: the auxiliary functions are returning monadic
  values.
*)

let parse (c : char) : (bool, char) Stack_monad.t = failwith "FILL THIS IN"

let main_monadic (s : string) : (bool, char) Stack_monad.t = failwith "FILL THIS IN"

let are_balanced_monadic (s : string) : bool =
  try run @@ main_monadic s with _ -> false

(*
  ------------
  EXTRA CREDIT
  ------------

  Uncomment the final few lines in `monads.mli` and implement them here, if you choose.  
*)