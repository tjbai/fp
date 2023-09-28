open Core

(* Returns a list of file paths corresponding to .ml and .mli files *)
let rec get_targets (path : string) : string list =
  match Sys_unix.is_directory path with
  | `No ->
      if Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli"
      then [ path ]
      else []
  | `Yes ->
      Sys_unix.ls_dir path
      |> List.fold ~init:[] ~f:(fun acc elt ->
             acc @ get_targets (path ^ "/" ^ elt))
  | `Unknown -> failwith "Unknown if path is directory"

(* Generates a string representing the contents of the file *)
let file_string (path : string) : string = Stdio.In_channel.read_all path

(* Counts occurrence of a single keyword in the body of text *)
let count_keyword (keyword : string) (text : string) : int =
  let kw_len = String.length keyword in

  let rem_n (text : string) (n : int) =
    String.sub ~pos:n ~len:(String.length text - n) text
  in

  let first_n (text : string) (n : int) = String.sub ~pos:0 ~len:n text in

  let non_char (c : char) : bool =
    not (Char.is_alpha c || Char.is_digit c || Char.( = ) c '_')
  in

  let kw_match (text : string) : bool =
    String.( = ) (String.sub ~pos:0 ~len:kw_len text) keyword
  in

  let rec count_rec (acc : int) (text : string) (open_comments : int)
      (open_quote : bool) (prev_char : char) : int =
    match String.length text with
    | n when n < kw_len -> acc
    | _ ->
        if String.( = ) (first_n text 2) "(*" && not open_quote then
          count_rec acc (rem_n text 2) (open_comments + 1) open_quote '*'
        else if String.( = ) (first_n text 2) "*)" && not open_quote then
          count_rec acc (rem_n text 2) (open_comments - 1) open_quote ')'
        else if String.( = ) (first_n text 1) "\"" then
          count_rec acc (rem_n text 1) open_comments (not open_quote) '\"'
        else if open_comments > 0 || open_quote then
          count_rec acc (rem_n text 1) open_comments open_quote text.[0]
        else if
          kw_match text && non_char prev_char
          && (String.length text = kw_len || non_char text.[kw_len])
        then
          count_rec (acc + 1) (rem_n text kw_len) open_comments open_quote
            text.[kw_len - 1]
        else count_rec acc (rem_n text 1) open_comments open_quote text.[0]
  in

  count_rec 0 text 0 false ' '

(* Counts all the keywords in a file and returns a Simpledict *)
let count_all_keywords ~(keywords : string list) ~(path : string) :
    int Simpledict.t =
  let text = file_string path in
  List.fold keywords ~init:Simpledict.Tree.Leaf ~f:(fun acc kw ->
      Simpledict.insert acc ~key:kw ~value:(count_keyword kw text))

(* Sorts by int frequencies, breaking ties lexicographically, and filters out 0s *)
let sort_and_filter (freqs : (string * int) list) : (string * int) list =
  List.sort freqs ~compare:(fun (str_a, num_a) (str_b, num_b) ->
      let int_compare = compare num_a num_b in
      if int_compare <> 0 then -int_compare else compare_string str_a str_b)
  |> List.filter ~f:(fun (_, num) -> num <> 0)

type fr = { keyword : string; count : int } [@@deriving sexp]
type fr_list = fr list [@@deriving sexp]

(* Converts list representation to desired sexp *)
let to_sexp_string (freqs : (string * int) list) : string =
  List.fold freqs ~init:[] ~f:(fun acc (str, num) ->
      { keyword = str; count = num } :: acc)
  |> List.rev |> sexp_of_fr_list |> Sexplib.Sexp.to_string
