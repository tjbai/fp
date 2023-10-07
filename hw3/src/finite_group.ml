(* *********************************************************** *)
(* ********************** P A R T I ************************** *)
(* *********************************************************** *)

open Core

(*
  Your implementation goes here. We provide just a little bit of starter code to give you
  the syntax, but you're expected to do the rest without modifying the .mli files.

  See finite_group.mli for the details.
*)

module type S = sig
  type t [@@deriving compare, sexp]

  val id : t
  val op : t -> t -> t
  val inverse : t -> t
  val of_int : int -> t option
  val to_int : t -> int
end

module type Params = sig
  val op : int -> int -> int
  val n : int
end

module Make (P : Params) : S = struct
  type t = int [@@deriving compare, sexp]

  let els = List.init P.n ~f:Fn.id

  let id =
    List.fold els ~init:(-1) ~f:(fun acc el1 ->
        if
          List.fold els ~init:0 ~f:(fun tot el2 ->
              if P.op el1 el2 = el2 && P.op el2 el1 = el2 then tot + 1 else tot)
          = P.n
        then el1
        else acc)

  let op (a : t) (b : t) : t = P.op a b mod P.n

  let inverse (n : t) : t =
    List.fold els ~init:(-1) ~f:(fun acc el ->
        if op n el = id && op el n = id then el else acc)

  let of_int (n : int) : t option = if n >= 0 && n < P.n then Some n else None
  let to_int (n : t) : int = n
end

module Z5_params : Params = struct
  let op (a : int) (b : int) : int = a + b
  let n = 5
end

module Z5_add : S = Make (Z5_params)

module Memoize (G : S) : S with type t = G.t = struct
  include G
  module GMap = Map.Make (G)

  let map : t GMap.t =
    let rec aux (acc : t GMap.t) (i : int) =
      match G.of_int i with
      | None -> acc
      | Some n -> aux (Map.add_exn acc ~key:n ~data:(G.inverse n)) (i + 1)
    in
    aux GMap.empty 0

  let inverse (n : t) : t = Map.find_exn map n
end
