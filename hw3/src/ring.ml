open Core

module type S = sig
  type t

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val of_string : string -> t option
  val to_string : t -> string
end

module Make_modular_ring (P : sig
  val n : int
end) : S = struct
  type t = int

  let ( + ) (a : t) (b : t) = (a + b) mod P.n
  let ( * ) (a : t) (b : t) = a * b mod P.n
  let of_string (s : string) : t option = int_of_string_opt s
  let to_string (n : t) : string = string_of_int n
end

module Z4 = Make_modular_ring (struct
  let n = 4
end)
