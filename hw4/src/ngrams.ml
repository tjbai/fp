open Core
open Lib
open Distribution (String) (Random)

let get_sample (sample : int) (start : string list) (d : t) =
  let context =
    if List.length start > 0 then
      start |> List.rev |> Fn.flip List.take 1 |> List.rev
    else sample_random_context d
  in

  let seq = sample_random_sequence d context (sample - List.length start) in

  start @ seq
  |> List.fold ~init:"" ~f:(fun acc elem ->
         if String.( = ) acc "" then elem else acc ^ " " ^ elem)
  |> Stdio.printf "%s\n"

type freq = { ngram : string list; frequency : int } [@@deriving sexp]
type freq_list = freq list [@@deriving sexp]

let get_most_frequent (most_frequent : int) (d : t) =
  most_frequent |> most_frequent_ngrams d
  |> List.map ~f:(fun (ngram, frequency) -> { ngram; frequency })
  |> sexp_of_freq_list |> Sexp.to_string |> Stdio.printf "%s\n"

let main =
  Command.basic ~summary:"Generate n-grams from a corpus file"
    Command.Let_syntax.(
      let%map_open n = anon ("n" %: int)
      and corpus_file = anon ("corpus-file" %: string)
      and sample =
        flag "--sample"
          (optional_with_default 0 int)
          ~doc:"Sample n-grams from provided text"
      and most_frequent =
        flag "--most-frequent"
          (optional_with_default 0 int)
          ~doc:"Most frequent n-grams in provided text"
      and start = anon (sequence ("..." %: string)) in
      fun () ->
        let d =
          Stdio.In_channel.read_all corpus_file
          |> parse_tokens |> make_distribution n
        in
        if sample > 0 then get_sample sample start d
        else if most_frequent > 0 then get_most_frequent most_frequent d
        else Stdio.printf "No option passed")

let () = Command_unix.run main
