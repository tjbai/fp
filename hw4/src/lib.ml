open Core

module type R = sig
  val int : int -> int
end

module Distribution (Item : Map.Key) (Random : R) = struct
  module Item_list : Map.Key with type t = Item.t list = struct
    type t = Item.t list [@@deriving compare, sexp]
  end

  module Ngram_map = Map.Make (Item_list)
  module Random = Random

  type t = Item.t list Ngram_map.t

  let to_list (ngrams : 'a Ngram_map.t) : (Item.t list * 'a) list =
    ngrams |> Map.to_sequence |> Sequence.to_list

  let make_distribution (n : int) (list : Item.t list) : t =
    let rec aux (ls : Item.t list) (context : Item.t list) (ngrams : t) : t =
      match ls with
      | [] -> ngrams
      | hd :: tl ->
          if List.length context < n - 1 then aux tl (context @ [ hd ]) ngrams
          else
            let next_ngrams =
              Map.update ngrams context ~f:(fun result ->
                  match result with Some cur -> hd :: cur | None -> [ hd ])
            in
            let next_context = List.tl_exn context @ [ hd ] in
            aux tl next_context next_ngrams
    in
    aux list [] Ngram_map.empty

  let sample_random_sequence (ngrams : t) (context : Item.t list) (n : int) :
      Item.t list =
    let rec aux (context : Item.t list) (result : Item.t list) : Item.t list =
      if List.length result = n then result
      else
        match Map.find ngrams context with
        | None -> result
        | Some cands ->
            let new_item =
              List.nth_exn cands (List.length cands |> Random.int)
            in
            let next_context = List.tl_exn context @ [ new_item ] in
            let next_result = result @ [ new_item ] in
            aux next_context next_result
    in
    aux context []

  let most_frequent_ngrams (ngrams : t) (k : int) : (Item.t list * int) list =
    let count_freqs ~(key : Item.t list) ~(data : Item.t list)
        (freqs : int Ngram_map.t) : int Ngram_map.t =
      List.fold data ~init:freqs ~f:(fun acc el ->
          Map.update acc (key @ [ el ]) ~f:(fun result ->
              match result with None -> 1 | Some f -> f + 1))
    in

    let freqs = Map.fold ngrams ~init:Ngram_map.empty ~f:count_freqs in
    let compare (ls1, f1) (ls2, f2) =
      match compare f2 f1 with 0 -> Item_list.compare ls1 ls2 | res -> res
    in

    freqs |> to_list |> List.sort ~compare |> Fn.flip List.take k

  let sample_random_context (ngrams : t) : Item.t list =
    let r =
      ngrams |> to_list
      |> List.fold ~init:0 ~f:(fun acc (_, value) -> acc + List.length value)
      |> Random.int
    in

    match
      ngrams |> to_list
      |> List.fold ~init:(0, []) ~f:(fun (index, acc) (key, value) ->
             if index <= r && r <= index + List.length value then
               (index + List.length value, key)
             else (index + List.length value, acc))
    with
    | _, res -> res
end

let sanitize (s : string) : string =
  let rec aux (acc : string) (i : int) : string =
    if String.length s = i then acc
    else if Char.is_alpha s.[i] || Char.is_digit s.[i] then
      aux (acc ^ (Char.lowercase s.[i] |> String.make 1)) (i + 1)
    else aux acc (i + 1)
  in
  aux "" 0

let parse_tokens (file : string) : string list =
  let is_whitespace (c : char) : bool =
    Char.( = ) c ' ' || Char.( = ) c '\t' || Char.( = ) c '\n'
  in

  let rec aux (acc : string list) (cur : string) (s : string) : string list =
    if String.length s = 0 then List.rev acc
    else
      let next_s = String.sub s ~pos:1 ~len:(String.length s - 1) in
      if is_whitespace s.[0] then aux (cur :: acc) "" next_s
      else aux acc (cur ^ String.make 1 s.[0]) next_s
  in

  Stdio.In_channel.read_all file
  |> aux [] "" |> List.map ~f:sanitize
  |> List.filter ~f:(fun s -> String.length s > 0)
