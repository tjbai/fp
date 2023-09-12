(*

FPSE Assignment 1
 
Name                  : 
List of Collaborators :

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.  CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make it recursive. In some cases, you will find it helpful to define auxillary functions, feel free to.

You must not use any mutation operations of OCaml for any of these questions (which we have not taught yet in any case): no arrays, for- or while-loops, references, etc.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(*
   You are required to use the Core libraries, don't remove the following line. If the editor is not recognizing Core (red squiggle under it for example), run a "dune build" from the shell -- the first time you build it will create some .merlin files which tells the editor where the libraries are.
*)
open Core

(* Here is a simple function which gets passed unit, (), as argument and raises an exception.  It is the initial implementation below. *)

let unimplemented () = failwith "unimplemented"

(*
   This homework is structured into modules. You are to complete module Part1 and all sections within it by the first due date.

   These modules act as containers for your functions.

   You will not need to modify any lines with 'module', 'end', or 'include'. You WILL need to modify the functions within the modules.
*)

module Part1 = struct
  (*
		Part I Section 1: simple numeric recursions.
		
		All functions must be total for the specified domain;	overflow is excluded from this restriction but should be avoided.
		
	*)
  module Section1 = struct
    (*
			Given a non-negative integer `n`, compute `0+1+2+ ... +n` using recursion
			(don't use the closed-form solution, do the actual addition).
		*)
    let rec summate (n : int) : int = if n = 0 then 0 else n + summate (n - 1)

    (*
			Given non-negative integers `n` and `m`, compute their least common multiple.
		*)
    let lcm (n : int) (m : int) : int =
      let rec aux (n_cum : int) (m_cum : int) (n : int) (m : int) =
        if n_cum = m_cum then n_cum
        else if n_cum < m_cum then aux (n_cum + n) m_cum n m
        else aux n_cum (m_cum + m) n m
      in
      aux n m n m

    (*
			Given a non-negative integer `n`, compute the n-th fibonacci number.	Give an implementation that does not take exponential time; the naive version from lecture is exponential	since it has two recursive calls for each call.
		*)
    let fibonacci (n : int) : int =
      let rec aux (n_1 : int) (n_2 : int) (index : int) (goal : int) : int =
        if index = goal then n_2 else aux n_2 (n_1 + n_2) (index + 1) goal
      in
      if n = 0 then 0 else if n = 1 then 1 else aux 1 1 2 n
  end
  (* module Section1 *)

  include Section1

  module Section2 = struct
    (*
			Part I Section 2: building and modifying lists. The List. module functions may NOT be used (yet).
		*)

    (*
			Given a non-negative integer `n`, produce a list [n; n-1; ...; 2; 1].
		*)
    let rec iota1 (n : int) : int list = if n = 0 then [] else n :: iota1 (n - 1)

    (*
			Given a non-negative integer `n`, produce a list [1; 2; ...; n-1; n],	without taking O(n^2) time.
		*)
    let iota2 (n : int) : int list =
      let rec aux (i : int) (n : int) : int list =
        if i > n then [] else i :: aux (i + 1) n
      in
      aux 1 n

    (*
			Given a positive integer `n`, produce the list of integers in the range (0, n] which it is divisible by, in ascending order.
		*)
    let factors (n : int) : int list =
      let rec aux (i : int) (n : int) : int list =
        if i > n then []
        else if n % i = 0 then i :: aux (i + 1) n
        else aux (i + 1) n
      in
      aux 1 n

    (*
       Reverse a list. Your solution must be in O(n) time. Note: the solution in lecture is O(n^2).
    *)
    let reverse (ls : 'a list) : 'a list =
      let rec aux (res : 'a list) (ls : 'a list) =
        match ls with [] -> res | h :: t -> aux (h :: res) t
      in
      aux [] ls

    (*
       Rotate a list to the right. That is, move index i to index i + k, and wrap the list around
       where necessary.
       e.g. rotate_list [1;2;3;4;5] 3 evaluates to [3;4;5;1;2]
       `k` is non-negative with no further constraints.
    *)
    let rotate_list (ls : 'a list) (k : int) : 'a list =
      let rec sz (ls : 'a list) (acc : int) : int =
        match ls with [] -> acc | h :: t -> sz t (acc + 1)
      in

      let rec slice (ls : 'a list) (i : int) (s : int) (e : int) (acc : 'a list)
          : 'a list =
        match ls with
        | [] -> reverse acc
        | h :: t ->
            if i >= s && i <= e then slice t (i + 1) s e (h :: acc)
            else slice t (i + 1) s e acc
      in

      let n = sz ls 0 in
      let norm_k = k % n in
      let a = slice ls 0 0 (n - norm_k - 1) [] in
      let b = slice ls 0 (n - norm_k) (n - 1) [] in
      b @ a
  end
  (* module Section2 *)

  include Section2

  module Section3 = struct
    (*
			Part I Section 3: strings, lists, and sorting.  The List module functions cannot be used.	String comparisons operations such as String.(<=) can be used, but no other String module functions.
		*)

    (*
			Given a list of strings, check to see if it is ordered, i.e. whether earlier elements are less than or equal to later elements.
		*)
    let rec is_ordered (ls : string list) : bool =
      match ls with
      | [] -> true
      | [ _ ] -> true
      | a :: b :: t -> String.( <= ) a b && is_ordered (b :: t)

    (*
			Define a function to remove the lexicographically maximum string in a list of strings.
			Return
			Error("empty list") if the input list is empty (and has no max)
			Ok(s,s_list) for s the maximum string and s_list the list with s removed, 
				if the list is not empty.

			If there are more than one max string, remove the max string that occurs last in the list.
			Relative order of the list must be maintained except for the single removed element.
		*)
    let remove_max (l : string list) : (string * string list, string) result =
      let rec id_max (ls : string list) (max : string) (max_i : int)
          (cur_i : int) : string * int =
        match ls with
        | [] -> (max, max_i)
        | h :: t ->
            if String.( <= ) max h then id_max t h cur_i (cur_i + 1)
            else id_max t max max_i (cur_i + 1)
      in

      let rec rem_idx (ls : string list) (idx : int) (target : int)
          (acc : string list) : string list =
        match ls with
        | [] -> reverse acc
        | h :: t ->
            if idx = target then rem_idx t (idx + 1) target acc
            else rem_idx t (idx + 1) target (h :: acc)
      in

      match id_max l "" 0 0 with
      | max, max_i -> (
          match l with
          | [] -> Error "empty list"
          | [ _ ] -> Ok (max, [])
          | _ -> Ok (max, rem_idx l 0 max_i []))

    (*
			Write a sort routine by repeated invocations of remove_max to pull out the largest
			elements one-by-one.  You should never need to invoke `remove_max` on an empty
			list, and you can thus `assert false` (an invariant failure) if the `Error`
			case is ever returned from `remove_max`.  
			This problem shows how we can manually encode the exceptional condition in `
			remove_max` with `Ok`/`Error` but convert it to an actual side effect here
			(the `assert false` will raise an exception if hit).

			Max sort on an empty list should return an empty list, not throw an exception.
			The sorted list should be sorted from smallest to largest string lexicographically
		*)
    let max_sort (l : string list) : string list =
      let rec aux (ls : string list) (acc : string list) =
        match ls with
        | [] -> acc
        | _ -> (
            match remove_max ls with
            | Error s -> assert false
            | Ok (max, t) -> aux t (max :: acc))
      in
      aux l []

    (*
       Split a list `ls` into two pieces, the first of which is the first `n` elements of `ls`,
       and the second is all remaining elements.
       e.g. split_list [1;2;3;4;5] 3 evaluates to ([1;2;3], [4;5])
       Note that this function returns a tuple. Here is an example of a tuple.
       ```
       let f x = (x, 10)
       ```
       Assume `n` is non-negative.
    *)
    let split_list (ls : 'a list) (n : int) : 'a list * 'a list =
      let rec aux (acc_a : 'a list) (acc_b : 'a list) (ls : 'a list) (i : int)
          (n : int) =
        match ls with
        | [] -> (reverse acc_a, reverse acc_b)
        | h :: t ->
            if i < n then aux (h :: acc_a) acc_b t (i + 1) n
            else aux acc_a (h :: acc_b) t (i + 1) n
      in
      aux [] [] ls 0 n

    (*
       Sort an int list using merge sort. Your solution must be O(n log n).
    *)
    let merge_sort (ls : int list) : int list =
      let rec sz (ls : int list) (acc : int) : int =
        match ls with [] -> acc | h :: t -> sz t (acc + 1)
      in

      let cdiv n : int = if n % 2 = 1 then (n / 2) + 1 else n / 2 in

      let rec merge (ls_a : int list) (ls_b : int list) (acc : int list) :
          int list =
        match ls_a with
        | [] -> (
            match ls_b with
            | [] -> reverse acc
            | h_b :: t_b -> merge ls_a t_b (h_b :: acc))
        | h_a :: t_a -> (
            match ls_b with
            | [] -> merge t_a ls_b (h_a :: acc)
            | h_b :: t_b ->
                if h_a <= h_b then merge t_a ls_b (h_a :: acc)
                else merge ls_a t_b (h_b :: acc))
      in

      let rec aux (ls : int list) (sz : int) : int list =
        match ls with
        | [] -> []
        | [ _ ] -> ls
        | _ -> (
            match split_list ls (sz / 2) with
            | a, b -> merge (aux a (sz / 2)) (aux b (cdiv sz)) [])
      in

      aux ls (sz ls 0)
  end
  (* module Section3 *)

  include Section3
end
(* module Part1 *)

include Part1

(* *************
    END PART I
   ************* *)

(* ***************
   BEGIN PART II
   *************** *)

module Part2 = struct
  module Section1 = struct
    (*
			Part II Section 1: for selected functions in Part I, provide a reimplementation of your previous code by refactoring the definition to use combinators provided by the List module.
			
			Care should be taken to use a concise, elegant combination of these provided functions to best express the task.
			
			These new implementations should not be explicitly recursive.	Note that the autograder is not aware if you cheated and used recursion; we will manually inspect your code and give you negative points if 
			you used recursion.
		*)
    let iota1' (n : int) : int list = List.init n ~f:(fun i -> n - i)
    let iota2' (n : int) : int list = List.init n ~f:(fun i -> i + 1)

    let factors' (n : int) : int list =
      iota2' n |> List.filter ~f:(fun i -> i > 0 && n % i = 0)

    let split_list' (ls : 'a list) (n : int) : 'a list * 'a list =
      List.split_n ls n
  end
  (* module Section1 *)

  include Section1

  module Section2 = struct
    (*
			Part II Section 2: primes. In this section we focus on numeric recursive functions.
		*)

    (*
       Check if positive integer `n` is prime. This should be able to quickly handle numbers up to 2^32.
    *)
    let is_prime (n : int) : bool =
      let rec aux i =
        if i * i > n then true else if n mod i = 0 then false else aux (i + 1)
      in
      aux 2

    (*
       Given a positive integer `n`, return the prime factor of `n` that has the greatest multiplicity.
       e.g. if n = 120, then its prime factors are 2, 2, 2, 3, 5, so the factor with the greatest
       	multiplicity is 2.
       If two prime factors have the same multiplicity, return the largest factor.
    *)
    let prime_factor_with_greatest_multiplicity (n : int) : int =
      let rec multiplicity (acc : int) (dividend : int) (divisor : int) =
        if dividend mod divisor <> 0 then acc
        else multiplicity (acc + 1) (dividend / divisor) divisor
      in

      let rec best_prime (i : int) (res : int) (res_ct : int) =
        if i * i > n then res
        else if (not (is_prime i)) || n mod i <> 0 then
          best_prime (i + 1) res res_ct
        else
          let i_ct = multiplicity 0 n i in
          if i_ct >= res_ct then best_prime (i + 1) i i_ct
          else best_prime (i + 1) res res_ct
      in

      if is_prime n then n else best_prime 2 0 0
  end
  (* module Section2 *)

  include Section2

  module Section3 = struct
    (*
		Part II Section 3: Checking validity of a Towers game solution

		Towers is a simple puzzle game, for rules and to play the game see
		https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/towers.html

		Here is a description taken from that site:

		Your task is to build a tower on every square, in such a way that:
		* Each row contains every possible height of tower once
		* Each column contains every possible height of tower once
		* Each numeric clue describes the number of towers that can be seen if you look into the square from that direction, assuming that shorter towers are hidden behind taller ones. For example, in a 5×5 grid, a clue marked ‘5’ indicates that the five tower heights must appear in increasing order (otherwise you would not be able to see all five towers), whereas a clue marked ‘1’ indicates that the tallest tower (the one marked 5) must come first.
			
		Since this assignment is an exercise to become familiar with functional programming, we are going to give appropriate auxiliary functions which will decompose the problem into smaller pieces.  You should be able to compose these short functions to accomplish the ultimate goal of verifying whether a completely-filled grid is a solution based on the clues around the edges.  We will only be verifying the "easy" games which don't leave out any of the clues around the grid edges.
			
		Use the List combinators, pipelining, etc. when possible and whenever it improves the readability of the solution.  All but the last function are directly code-able with the List combinators and no `rec`.
		As in Part I feel free to write your own helper functions if needed.

		If you are unsure on what the requirements of the functions are, look at the test cases we provide.

		*)

    (* We can represent the tower board itself as a list of lists of ints.
       	See `tower_board_example` in `tests.ml` for an example.

       The first task is to determine if a list of lists of integers is "square", i.e. each list is the same length and there are the same number of lists as there are elements in any of the lists. If it is, return Ok(the dimension of the array).  If not, return Error "not square". *)
    let square_size (grid : int list list) : (int, string) result =
      let hd_sz =
        List.length (match List.hd grid with Some c -> c | None -> [])
      in

      let same_sz (grid : int list list) =
        List.map grid ~f:(fun ls -> List.length ls = hd_sz)
        |> List.fold ~init:true ~f:(fun accum elt -> elt && accum)
      in

      if List.length grid = hd_sz && same_sz grid then Ok hd_sz
      else Error "not square"

    (* Given a list of integers of length n, determine if the list has exactly one occurrence
       of each number in 1 .. n in it. Return false if not *)
    let elements_span_range (l : int list) : bool =
      let len = List.length l in

      let count i =
        List.fold l ~init:0 ~f:(fun accum elt ->
            if elt = i then accum + 1 else accum)
      in

      let rec aux i =
        if i = len + 1 then true else if count i <> 1 then false else aux (i + 1)
      in

      aux 1

    (* In order to check the clues on all four edges, we will rotate the grid counter-clockwise and call the above function at each rotation.
       There many ways we can rotate our grid.  Here we suggest using a combination of transpose (like for a matrix: flip rows and columns), and reflect.  Note you can assume the grid is well-formed. *)
    let rec transpose (grid : int list list) : int list list =
      match grid with
      | [] -> []
      | [] :: _ -> []
      | (a :: b) :: c ->
          List.map ~f:List.hd_exn grid
          :: transpose (List.map ~f:List.tl_exn grid)

    let reflect_vertical_axis (grid : int list list) : int list list =
      List.map ~f:List.rev grid

    (* Now it should not be difficult to define a function to rotate the grid counterclockwise *)
    let rotate_ccw (grid : int list list) : int list list =
      grid |> reflect_vertical_axis |> transpose

    (* Check to see if a towers grid is well-formed, namely
       1) it is square as per above,
       2) it is at least 1x1 in size (no 0x0 degenerates are allowed)
       2) each row and column spans the range as per above *)
    let well_formed_grid (grid : int list list) : bool =
      let check_all_span (grid : int list list) =
        List.fold grid ~init:true ~f:(fun accum elt ->
            if elements_span_range elt then accum else false)
      in

      match grid with
      | [] -> false
      | _ -> (
          match square_size grid with
          | Error _ -> false
          | Ok _ ->
              grid |> check_all_span && grid |> transpose |> check_all_span)

    (* The next six auxiliary functions should only be called on well-formed grids, or rows from well-formed
       grids, and so you don't need to deal with ill-formed cases there such as 0x0 or non-spanning grid rows. *)

    (* The lowest level of the validity check for towers requires finding the number of
       local maxima going down a given row of the grid (i.e. down a list of integers).  Define
       a function local_max_count to find that value. *)

    let local_max_count (row : int list) : int =
      let rec aux (row : int list) (acc : int) (cur_max : int) =
        match row with
        | [] -> acc
        | h :: t -> if h > cur_max then aux t (acc + 1) h else aux t acc cur_max
      in
      aux row 0 0

    (* Now we need to apply the above function to each row/column around the grid.  There
       are many reasonable ways to solve that task, but here we ask you to first write a
       function verify_left_clues to check the "left side grid" clues only are correct.  For
       this function the `edge_clues` list is only the left-side clues. *)

    let verify_left_clues (grid : int list list) (edge_clues : int list) : bool
        =
      List.zip_exn grid edge_clues
      |> List.fold ~init:true ~f:(fun accum elt ->
             match elt with
             | ls, clue -> if local_max_count ls = clue then accum else false)

    (* Finally, write a function verify_towers_solution which given a grid and the clues all around the grid, verifies that the solution is correct with respect to the clues: the grid is well-formed as per above, and the clues are all satisfied by the solution.
       The clues are a list of lists, the first element of the list is the edge_clues for the original board orientation, and the subsequent lists correspond to clues for successive rotations of the grid.
       If either the grid is ill-formed or the clues are not all satisfied, return false. *)

    let verify_towers_solution (grid : int list list)
        (four_edge_clues : int list list) : bool =
      if not (well_formed_grid grid) then false
      else
        let rec rotate_ccw_i (i : int) (grid : int list list) =
          match i with 0 -> grid | _ -> rotate_ccw_i (i - 1) (rotate_ccw grid)
        in

        let grids = List.init 4 ~f:(fun i -> rotate_ccw_i i grid) in

        List.zip_exn grids four_edge_clues
        |> List.fold ~init:true ~f:(fun acc elt ->
               match elt with
               | grid, edge_clues ->
                   if verify_left_clues grid edge_clues then acc else false)
  end
  (* module Section3 *)

  include Section3
end
(* module Part2 *)

include Part2
