(*
    Part II: Sys_unix, Stdio, and making a useful utility
*)

(*
    You will be developing a simple command-line application which will count how many occurrences of each OCaml language keyword occurs in a directory containing OCaml code.

    This comment describing the assignment is long, but it contains valuable information, and you'll need to read it all.

    ------------
    GENERAL FLOW
    ------------
    
    Given a directory path on the command line (or the current directory if none is given), you should:

    - traverse the directory recursively to find each OCaml source file (.ml or .mli)

    - filter out any comments and literal strings from the code

    - count the number of occurences of each keyword in the file: sum up all
    of the occurrences over all files and sort the resulting dictionary
    from most to least common occurrence.

    - report the total sum of keyword counts to stdout in s-expression format terminated by a newline:

      (((keyword <word>)(count <number>))((keyword <word>)(count <number>)) ... )


    ---------------------
    HOW TO COUNT KEYWORDS
    ---------------------

    Before counting keywords you should remove all comments and string literals; for example 

    (* 
       remove this text in case there is a match with a keyword here by mistake
    *)
    
    and 
    
    "remove this text too in case there is a match with a keyword in this literal string"

    Once comments/literals are removed, you need to count each occurence of a keyword in the file which is delimited on both ends by non-characters. So for example `fun iffy -> 0` does not contain the keyword `if` since it is not delimited on both ends by non-characters, but `if x > 0` does.

    
    ------
    OUTPUT
    ------

    Your output will be a printed s-expression terminated by a newline. The s-expression should be derived from the type
    ```
    type t = { keyword : string ; count : int } list
    ```

    You'll convert an instance of this type to an s-expression, convert the s-expression to a string, and print with a newline.

    Refer to the "libraries" section below for our advice on converting to s-expressions.


    --------------
    CLARIFICATIONS
    --------------
    
    Here are some clarifications you might find useful. Before asking for clarification on Courselore, please read all of the items below.
    - Nested comments are supported in OCaml, so be sure to keep track of nesting.
    - Comments can span multiple lines like this one we're in right now.
    - We don't require whitespace around the comment characters, so the following line would be a valid comment:
      (* this is a valid comment even though there is no space before it closes*)
    - You can assume literal strings are on one input line only.
    - You can assume there are no escaped double quotes (which allow string literals to themselves contain double quotes).
    - Literal strings take precedence over comments, e.g.
      (* this is a single valid comment despite the " *) ". See how it doesn't mess up the rest of the file below *)
    - Do not output anything for keywords that don't appear.
    - A "non-character" is anything that is not alphanumeric and is not an underscore. Examples:
      - `not (if x = 0 then true else false)` contains the keywords {if, then, true, else, false} because '(' and ')' are "non-characters".
      - `fun if_if_if(*comment here*) -> true` contains only the keywords {fun, true} because '_' is not a "non-character".
    - You should handle invalid input directories gracefully, but we will not test your code on invalid directories.
    - Your code will only be tested on compiling OCaml files.
    - Report keywords in order of occurrence. If multiple keywords have the same count, then report those keywords in alphabetical order.
    - Your output must be terminated by a newline to pass the Gradescope tests.
      

    ---------
    LIBRARIES
    ---------

    Any of the default set of libraries for the course may be used.
    Core.Sys, Core_unix and Stdio provide basics for filesystem usage and accessing argv.

    You are of course welcome to use your Part I dict functions.

    We highly suggest using a pre-processor extension for s-expression conversions. `ppx_jane` helps you derive s-expression conversions with `[@@deriving sexp]`. See it here: https://github.com/janestreet/ppx_sexp_conv
    
    Note that if you are using `ppx_jane` to derive sexp conversions, you'll need to add
      
    (preprocess
    (pps ppx_jane))

    to the executable build in the dune file to enable the ppx macros. If you are informally testing in the top loop, you need to `#require ppx_jane;;` to get these to work.

    Note: don't use any other opam libraries beyond the official opam libraries
    on the FPSE Coding page. Add approved libraries to your dune file as needed.


    -------
    TESTING
    -------

    Since this file compiles to an executable, the functions defined within it cannot be tested. Any testable helper functions you write should be in `utils.ml` and `utils.mli` so that you can
    test them.

    Add your tests to `tests/tests.ml` and note the README for testing requirements. The tests on your helper functions will be considered in your grade. We don't expect you to test your functions that read files from a directory, but we do expect you to test functions that, for example, remove comments from a string.


    ---------------
    GETTING STARTED
    ---------------

    We are providing you with some template code below.

    OCaml executables work by simply evaluating each top-level expression in the file, similar conceptually to a scripting language. So all the let () = code will run. Feel free to modify the code below as much as you want.

    A rough idea of what the top-level program could be is something like:

		target_dir
    |> get_path_elts
    |> count_keywords
    |> keyword_counts_to_sexp
    |> print_endline

    Please make sure to break the tasks down into separate functions for each distinct task.  Put any auxiliary functions in utils.ml and utils.mli so they can be tested as well.
*)

open Core

(* Here is the official keyword list from the docs *)
(* ["and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";]
*)

(* 
  As with C, the first argv is always the name of the executable, that's why we match on the second element in the list instead 
*)
let () =
  let target_dir = 
    match Sys.get_argv () |> Array.to_list with
    | _ :: dir :: _ -> dir
    | _ -> Core_unix.getcwd ()
  in
  (* your code here instead of the following line *)
  Stdio.printf "Target dir: %s\n" target_dir
