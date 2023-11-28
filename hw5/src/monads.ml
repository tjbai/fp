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

module State_int = struct
  module T : Monad.Basic with type 'a t = int -> 'a * int = struct
    type 'a t = int -> 'a * int

    let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t =
     fun (i : int) ->
      let x', i' = x i in
      (f x') i'

    let return (x : 'a) : 'a t = fun i -> (x, i)
    let map = `Define_using_bind
  end

  include T
  include Monad.Make (T)

  let run (i : 'a t) : 'a * int = i 0
  let set (i : int) : unit t = fun _ -> ((), i)
  let get : int t = fun n -> (n, n)
  let inc : int t = fun n -> (n + 1, n + 1)
end

module Stack_monad : sig
  include Monad.S2

  val run : ('a, 'e) t -> 'a
  val push : 'e -> (unit, 'e) t
  val pop : ('e, 'e) t
  val is_empty : (bool, 'e) t
end = struct
  module Stack = struct
    type 'e t = 'e list

    let push (s : 'e t) (x : 'e) : 'e t = x :: s

    let pop (s : 'e t) : 'e * 'e t =
      match s with x :: s -> (x, s) | [] -> failwith "empty"

    let is_empty (s : 'e t) : bool = match s with [] -> true | _ -> false
  end

  module T : Monad.Basic2 with type ('a, 'e) t = 'e Stack.t -> 'a * 'e Stack.t =
  struct
    type ('a, 'e) t = 'e Stack.t -> 'a * 'e Stack.t

    let bind (x : ('a, 'e) t) ~(f : 'a -> ('b, 'e) t) : ('b, 'e) t =
     fun (s : 'e Stack.t) ->
      let x', s' = x s in
      (f x') s'

    let return (x : 'a) : ('a, 'e) t = fun (s : 'e Stack.t) -> (x, s)
    let map = `Define_using_bind
  end

  include T
  include Monad.Make2 (T)

  let run (x : ('a, 'e) t) : 'a = match x [] with a, _ -> a
  let push (x : 'e) : (unit, 'e) t = fun (s : 'e Stack.t) -> ((), Stack.push s x)
  let pop : ('e, 'e) t = fun (s : 'e Stack.t) -> Stack.pop s
  let is_empty : (bool, 'e) t = fun (s : 'e Stack.t) -> (Stack.is_empty s, s)
end

open Stack_monad
open Stack_monad.Let_syntax

let simple_stack : ('a, char) t =
  let%bind () = push 'a' in
  let%bind () = push 'b' in
  let%bind () = push 'c' in
  let%bind c = pop in
  return Char.(c = 'c')
;;

let r = run simple_stack in
assert r

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

let parse (c : char) : (bool, char) Stack_monad.t =
  match c with
  | '(' ->
      let%bind () = push '(' in
      return true
  | ')' ->
      let%bind c = pop in
      return Char.(c = '(')
  | _ -> return true

let main_monadic (s : string) : (bool, char) t =
  let rec aux (s : char list) =
    match s with
    | [] ->
        let%bind empty = is_empty in
        return empty
    | c :: s ->
        let%bind matched = parse c in
        if matched then aux s else return false
  in

  s |> String.to_list |> aux

let are_balanced_monadic (s : string) : bool =
  Stdio.printf "%s\n" s;
  try run @@ main_monadic s with _ -> false

(*
  ------------
  EXTRA CREDIT
  ------------

  Uncomment the final few lines in `monads.mli` and implement them here, if you choose.  
*)
