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
  val make_distribution : int -> Item.t list -> t
  val sample_random_sequence : t -> Item.t list -> int -> Item.t list
  val most_frequent_ngrams : t -> (Item.t list * int) list
  val sample_random_context : t -> Item.t list
end
with module Random = Random

(* Reads all tokens from a file, delimited by whitespace *)
val parse_tokens : string -> string list
