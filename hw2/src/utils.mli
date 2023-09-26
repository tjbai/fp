(* Returns a list of file paths corresponding to .ml and .mli files *)
val get_targets : string -> string list

(* Counts all the keywords in a file and returns a Simpledict *)
val count_all_keywords : keywords:string list -> path:string -> int Simpledict.t

(* Sorts by frequencies, breaking ties lexicographically *)
val sort_frequencies : (string * int) list -> (string * int) list
