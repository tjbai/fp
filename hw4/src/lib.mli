open Core

module type R = sig
  val int : int -> int
end

module Distribution
    (Item : Map.Key)
    (Random : R) : sig
  module Random : R

  type t

  val to_list : t -> (Item.t list * Item.t list) list
  val make_distribution : Item.t list -> int -> t
  val sample_random_sequence : t -> Item.t list -> int -> Item.t list
end
with module Random = Random
