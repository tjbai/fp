module type Data = sig
  include Ring.S

  val next : string -> (string * t) option
end

module type Eval = sig
  type t

  val eval : string -> (t, string) result
end

module Make_data (Ring : Ring.S) : Data with type t = Ring.t = struct
  include Ring

  let next (s : string) : (string * t) option =
    let is_separator (c : char) : bool =
      ((int_of_char c >= int_of_char '0' && int_of_char c <= int_of_char '9')
      || Char.equal c '/' || Char.equal c '-')
      |> not
    in

    let rec find_sep (h : string) (t : string) : string * string =
      if String.length t = 0 || is_separator t.[0] then (h, t)
      else
        find_sep
          (h ^ String.make 1 t.[0])
          (String.sub t 1 (String.length t - 1))
    in

    if String.length s = 0 then None
    else if s.[0] = '+' || s.[0] = '*' then None
    else
      let h, t = find_sep "" s in
      match Ring.of_string h with None -> None | Some n -> Some (t, n)
end

module Make_eval (Data : Data) : Eval with type t = Data.t = struct
  type t = Data.t

  let is_whitespace (c : char) : bool =
    c = ' ' || c = '\t' || c = '\n' || c = '\r'

  let is_operator (c : char) : bool = c = '+' || c = '*'

  let eval (s : string) : (t, string) result =
    let rec aux (exp : string) (stk : t Stack.t) : (t, string) result =
      (* base case: end of string *)
      if String.length exp = 0 then
        if Stack.length stk > 1 || Stack.length stk == 0 then Error "unmatched"
        else Ok (Stack.top stk)
      else
        match Data.next exp with
        (* if we can pull a value, add it to the stack *)
        | Some (new_exp, value) ->
            Stack.push value stk;
            aux new_exp stk
        (* otherwise, match with whitespace, operators, or illegal characters *)
        | None -> (
            let new_exp = String.sub exp 1 (String.length exp - 1) in
            match exp.[0] with
            | c when is_operator c ->
                if Stack.length stk < 2 then Error "unmatched"
                else
                  let a = Stack.pop stk in
                  let b = Stack.pop stk in
                  if exp.[0] = '+' then (
                    Stack.push (Data.( + ) a b) stk;
                    aux new_exp stk)
                  else (
                    Stack.push (Data.( * ) a b) stk;
                    aux new_exp stk)
            | c when is_whitespace c -> aux new_exp stk
            | _ -> Error "illegal character")
    in

    aux s (Stack.create ())
end

module Z4_data = Make_data (Ring.Z4)

module Int_data = Make_data (struct
  type t = int

  let ( + ) (a : t) (b : t) = a + b
  let ( * ) (a : t) (b : t) = a * b
  let of_string (s : string) : t option = int_of_string_opt s
  let to_string (n : t) : string = string_of_int n
end)

module Rat_data = Make_data (struct
  type t = int * int

  let reduce ((p, q) : t) : int * int =
    let rec gcd (a : int) (b : int) : int =
      if a < b then gcd b a else if b = 0 then a else gcd b (Int.rem a b)
    in

    let rec aux (p : int) (q : int) : int * int =
      let d = if p < 0 then gcd (-p) q else gcd p q in
      if d = 1 then (p, q) else aux (Int.div p d) (Int.div q d)
    in

    aux p q

  let ( + ) ((ap, aq) : t) ((bp, bq) : t) =
    reduce ((ap * bq) + (bp * aq), aq * bq)

  let ( * ) ((ap, aq) : t) ((bp, bq) : t) = reduce (ap * bp, aq * bq)

  let of_string (s : string) : t option =
    (* let i = String.index_opt s '/' in *)
    (* Stdio.printf "i: %d" i; *)
    match String.index_opt s '/' with
    | None -> Some (int_of_string s, 1)
    | Some i -> (
        match
          ( String.sub s 0 i |> int_of_string_opt,
            String.sub s (Int.add i 1) (String.length s - i - 1)
            |> int_of_string_opt )
        with
        | Some a, Some b -> if b = 0 then None else Some (reduce (a, b))
        | _ -> None)

  let to_string ((p, q) : t) : string = string_of_int p ^ "/" ^ string_of_int q
end)

module Z4_eval = Make_eval (Z4_data)
module Int_eval = Make_eval (Int_data)
module Rat_eval = Make_eval (Rat_data)
