open Core

module type R = sig
  val int : int -> int
end

module type D = sig
  module Ngram_map : Map.S
  module Random : R

  type item_list
  type t

  val make_distribution : item_list -> int -> t
  val sample_random_sequence : int -> item_list
end

module Distribution (Item : Map.Key) (Random : R) :
  D with type item_list = Item.t list with module Random = Random
