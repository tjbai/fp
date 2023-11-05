(*
  Part II:

  Implement an executable `ngrams.exe` which can use n-gram models in several ways.   It should expect to be called with the following arguments, with bracketed ones optional:

    $ ngrams.exe N CORPUS-FILE [--sample SAMPLE-LENGTH [INITIAL-WORDS...]] [--most-frequent N-MOST-FREQUENT]

  
  Functionality should be as follows:

  - Load the file specified by `CORPUS-FILE` and split its contents into a sequence of strings based on whitespace. Treat newlines and spaces, etc. equally.

  - Sanitize each of the strings in this sequence by sending to lowercase and removing non-alphanumeric characters. Say a "sanitized" word is lowercase and uses only alphanumeric characters.

  - Initialize an n-gram distribution using `N` and the sanitized sequence of words. The `N` is the length of the n-grams used when building the distribution, so N = 3 means two items of context because the last item is used for sampling, and the first two are used for context of the sampled element.

  
  If the option `--sample SAMPLE-LENGTH` is provided:

    To stdout, output a sequence of `SAMPLE-LENGTH` words randomly sampled from the n-gram model.  Print them out separated by single spaces. 
    
    To begin the sequence, use the `INITIAL-WORDS` arguments provided after `--sample` to seed the sequence, or if none are provided, choose a random starting n-gram to begin. You may assume that the words provided as `INITIAL-WORDS` are already sanitized, and that there are at least `N - 1` of them.

  If the option `--most-frequent N-MOST-FREQUENT` is provided:
  
    To stdout, output a sorted sexp-formatted list of length `N-MOST-FREQUENT` containing information about the most common n-grams seen in the `CORPUS-FILE`, like so:

      (((ngram(hello world goodbye))(frequency 5))...)

    Where the ["hello"; "world"; "goodbye"] n-gram showed up 5 times, and "..." is for the remaining, less-frequent n-grams. In this example, `N` = 3.

    Higher frequency n-grams should come first, and frequency ties should be broken by n-gram alphabetical order. 

  You may assume that only one of `--sample` or `--most-frequent` will be supplied at a time, and that at least one will be given.

  To parse command line arguments, we recommend looking into the `Core.Command` module.  See Real World OCaml Chapter 14 https://dev.realworldocaml.org/command-line-parsing.html for some examples of Core.Command in action.
  If these feels cumbersome, you can parse them manually, which is not a bad option for an executable of this scale.

  We will reveal only one test for each option to help you get the right output format, but you are expected to thoroughly test your own code. Testing is an important part of software development.
*)

open Core
open Lib
open Distribution (String) (Random)

type freq = { ngram : string list; frequency : int } [@@deriving sexp]
type freq_list = freq list [@@deriving sexp]

let main =
  Command.basic ~summary:"Generate n-grams from a corpus file"
    Command.Let_syntax.(
      (* define arguments *)
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

      (* dispatch *)
      fun () ->
        let d = parse_tokens corpus_file |> make_distribution n in
        if sample > 0 then
          let context =
            if List.length start > 0 then
              start |> List.rev |> Fn.flip List.take 1 |> List.rev
            else sample_random_context d
          in
          let seq =
            sample_random_sequence d context (sample - List.length start)
          in
          start @ seq
          |> List.fold ~init:"" ~f:(fun acc elem ->
                 if String.( = ) acc "" then elem else acc ^ " " ^ elem)
          |> Stdio.printf "%s\n"
        else if most_frequent > 0 then
          most_frequent |> most_frequent_ngrams d
          |> List.map ~f:(fun (ngram, frequency) -> { ngram; frequency })
          |> sexp_of_freq_list |> Sexp.to_string |> Stdio.printf "%s\n"
        else Stdio.printf "No option passed")

let () = Command_unix.run main
