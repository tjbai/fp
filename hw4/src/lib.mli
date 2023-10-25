open Core

module type D = sig
  module Item_list : Map.Key
  module Item_map : Map.S

  type item_list
  type t

  val make_distribution : item_list -> int -> t
  val sample_random_sequence : int -> item_list
end

module Distribution (Item: Map.Key) : D with type item_list = Item.t list