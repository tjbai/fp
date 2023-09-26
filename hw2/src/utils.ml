open Core

(* Checks file path for .ml or .mli suffix *)
let valid_ocaml_target (path : string) : bool =
  match String.length path with
  | n when n >= 3 -> String.( = ) (String.sub ~pos:(n - 3) ~len:3 path) ".ml"
  | n when n >= 4 -> String.( = ) (String.sub ~pos:(n - 4) ~len:4 path) ".mli"
  | _ -> false

(* Returns a list of file paths corresponding to .ml and .mli files *)
let rec get_targets (path : string) : string list =
  match Sys_unix.is_directory path with
  | `No -> if valid_ocaml_target path then [ path ] else []
  | `Yes ->
      Sys_unix.ls_dir path
      |> List.fold ~init:[] ~f:(fun acc elt ->
             acc @ get_targets (path ^ "/" ^ elt))
  | `Unknown -> failwith "Unknown if path is directory"

(* Generates a string representing the contents of the file *)
let file_string (path : string) : string = Stdio.In_channel.read_all path

(* Counts occurrence of a single keyword in the body of text *)
let count_keyword ~(keyword : string) (text : string) : int =
  let kw_len = String.length keyword in

  (* First n character substring *)
  let first_n (text : string) (n : int) = String.sub ~pos:0 ~len:n text in

  (* Substring after removing first n characters *)
  let after_n (text : string) (n : int) =
    String.sub ~pos:n ~len:(String.length text - n) text
  in

  (* Criteria for non-character *)
  let non_char (c : char) : bool =
    not (Char.is_alpha c || Char.is_digit c || Char.( = ) c '_')
  in

  (* Check if the text starts with the keyword, accounting for non-character after *)
  let starts_with_kw (text : string) : bool =
    match String.length text with
    | n when n = kw_len -> String.( = ) text keyword
    | n when n > kw_len ->
        String.( = ) (first_n text kw_len) keyword && non_char text.[kw_len]
    | _ -> false
  in

  (* Recursively parse text, tracking comment and quote context *)
  let rec aux (acc : int) (text : string) (open_comments : int) (open_q : int)
      (prev_char : char) : int =
    match String.length text with
    | n when n < kw_len || n < 2 -> acc
    | _ ->
        (* If quotation mark *)
        if String.( = ) (first_n text 1) "\"" then
          aux acc (after_n text 1) open_comments ((open_q + 1) % 2) '\"'
          (* If opening comment *)
        else if String.( = ) (first_n text 2) "(*" then
          aux acc (after_n text 2) (open_comments + 1) open_q '*'
          (* If  closing comment *)
        else if String.( = ) (first_n text 2) "*)" then
          aux acc (after_n text 2) (open_comments - 1) open_q ')'
          (* If in context of comment or quote, then continue *)
        else if open_comments > 0 || open_q > 0 then
          aux acc (after_n text 1) open_comments open_q text.[0]
          (* If we find a keyword match! *)
        else if starts_with_kw text && non_char prev_char then
          aux (acc + 1) (after_n text kw_len) open_comments open_q
            keyword.[kw_len - 1]
        else aux acc (after_n text 1) open_comments open_q text.[0]
  in

  aux 0 text 0 0 ' '

(* Counts all the keywords in a file and returns a Simpledict *)
let count_all_keywords ~(keywords : string list) ~(path : string) :
    int Simpledict.t =
  let text = file_string path in
  List.fold keywords ~init:Simpledict.Tree.Leaf ~f:(fun acc elt ->
      Simpledict.insert acc ~key:elt ~value:(count_keyword ~keyword:elt text))

(* Sorts by int frequencies, breaking ties lexicographically *)
let sort_frequencies (freqs : (string * int) list) : (string * int) list =
  List.sort freqs ~compare:(fun (str_a, num_a) (str_b, num_b) ->
      let int_compare = compare num_a num_b in
      if int_compare <> 0 then -int_compare else compare_string str_a str_b)
