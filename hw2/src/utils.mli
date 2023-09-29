(* Returns a list of file paths corresponding to .ml and .mli files *)
val get_targets : string -> string list

(* Counts occurrence of a single keyword in the body of text *)
val count_keyword : string -> string -> int

(* Counts all the keywords in a file and returns a Simpledict *)
val count_all_keywords : keywords:string list -> path:string -> int Simpledict.t

(* Sorts by int frequencies, breaking ties lexicographically, and filters out 0s *)
val sort_and_filter : (string * int) list -> (string * int) list

(* Converts list representation to desired sexp *)
val to_sexp_string : (string * int) list -> string
