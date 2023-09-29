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

(*
   Counts occurrence of a single keyword in the body of text

   Note: Rather than removing comments and quotes from the string,
     we recursively iterate over a string and track whether we are in a
     comment or quote "context". This way, we can read the contents from 
     of a file as single string and check for matches in a single pass.
*)
let count_keyword (keyword : string) (text : string) : int =
  let kw_len = String.length keyword in

  (* Take substring after first n characters *)
  let rem_n (text : string) (n : int) =
    String.sub ~pos:n ~len:(String.length text - n) text
  in

  (* Take substring of first n characters *)
  let first_n (text : string) (n : int) = String.sub ~pos:0 ~len:n text in

  (* Check if a character is NOT alphanumeric or _ *)
  let non_char (c : char) : bool =
    not (Char.is_alpha c || Char.is_digit c || Char.( = ) c '_')
  in

  (* Last character of the keyword *)
  let last_char = keyword.[kw_len - 1] in

  (* Recursively iterate over text, tracking comment and quote context *)
  let rec count_rec ~(text : string) (acc : int) (comments : int) (quote : bool)
      (prev_char : char) : int =
    match String.length text with
    | n when n < kw_len -> acc
    | _ ->
        (* Start of comment *)
        if String.( = ) (first_n text 2) "(*" && not quote then
          count_rec ~text:(rem_n text 2) acc (comments + 1) quote '*'
          (* End of comment *)
        else if String.( = ) (first_n text 2) "*)" && not quote then
          count_rec ~text:(rem_n text 2) acc (comments - 1) quote ')'
          (* Start/end of quote *)
        else if String.( = ) (first_n text 1) "\"" then
          count_rec ~text:(rem_n text 1) acc comments (not quote) '\"'
          (* Inside comment or quote context *)
        else if comments > 0 || quote then
          count_rec ~text:(rem_n text 1) acc comments quote text.[0]
          (* Keyword match and surrounded by noncharacters *)
        else if
          String.( = ) (first_n text kw_len) keyword
          && non_char prev_char
          && (String.length text = kw_len || non_char text.[kw_len])
        then
          count_rec ~text:(rem_n text kw_len) (acc + 1) comments quote last_char
          (* No match, go to next character *)
        else count_rec ~text:(rem_n text 1) acc comments quote text.[0]
  in

  count_rec ~text 0 0 false ' '

(* Counts all the keywords in a file and returns a Simpledict *)
let count_all_keywords ~(keywords : string list) ~(path : string) :
    int Simpledict.t =
  let text = file_string path in

  (* Insert frequency node into dictionary *)
  let accumulate (freqs : int Simpledict.t) (kw : string) : int Simpledict.t =
    Simpledict.insert freqs ~key:kw ~value:(count_keyword kw text)
  in

  List.fold keywords ~init:Simpledict.Tree.Leaf ~f:accumulate

(* Sorts by int frequencies, breaking ties lexicographically, and filters out 0s *)
let sort_and_filter (freqs : (string * int) list) : (string * int) list =
  (* Compare 2 string * int tuples *)
  let compare_tups ((str_a, num_a) : string * int)
      ((str_b, num_b) : string * int) : int =
    let int_compare = compare num_a num_b in
    if int_compare <> 0 then -int_compare else compare_string str_a str_b
  in

  freqs
  |> List.sort ~compare:compare_tups
  |> List.filter ~f:(fun (_, num) -> num <> 0)

(* Type definitions to derive s-expression representation *)
type fr = { keyword : string; count : int } [@@deriving sexp]
type fr_list = fr list [@@deriving sexp]

(* Converts list representation to desired sexp *)
let to_sexp_string (freqs : (string * int) list) : string =
  freqs
  |> List.fold ~init:[] ~f:(fun acc (str, num) ->
         { keyword = str; count = num } :: acc)
  |> List.rev |> sexp_of_fr_list |> Sexplib.Sexp.to_string
