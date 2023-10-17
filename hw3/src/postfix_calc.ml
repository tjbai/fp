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
    let is_separator (c : char) (h : string) : bool =
      ((int_of_char c >= int_of_char '0' && int_of_char c <= int_of_char '9')
      || Char.equal c '/'
      || (String.length h = 0 && Char.equal c '-'))
      |> not
    in

    (* Split string by first separator *)
    let rec find_sep (h : string) (t : string) : string * string =
      if String.length t = 0 || is_separator t.[0] h then (h, t)
      else
        find_sep
          (h ^ String.make 1 t.[0])
          (String.sub t 1 (String.length t - 1))
    in

    let h, t = find_sep "" s in
    match Ring.of_string h with None -> None | Some n -> Some (t, n)
end

module Make_eval (Data : Data) : Eval with type t = Data.t = struct
  type t = Data.t

  let is_whitespace (c : char) : bool =
    c = ' ' || c = '\t' || c = '\n' || c = '\r'

  let is_operator (c : char) : bool = c = '+' || c = '*'

  let rec eval_rec (exp : string) (stk : t Stack.t) : (t, string) result =
    if String.length exp = 0 then
      (* Base case: end of string *)
      if Stack.length stk > 1 || Stack.length stk == 0 then Error "unmatched"
      else Ok (Stack.top stk)
    else if is_whitespace exp.[0] then
      (* If whitespace, skip *)
      eval_rec (String.sub exp 1 (String.length exp - 1)) stk
    else if is_operator exp.[0] && Stack.length stk < 2 then Error "unmatched"
    else if is_operator exp.[0] then
      (* If operator and enough values, pop and compute *)
      let a = Stack.pop stk in
      let b = Stack.pop stk in
      if exp.[0] = '+' then (
        Stack.push (Data.( + ) a b) stk;
        eval_rec (String.sub exp 1 (String.length exp - 1)) stk)
      else (
        Stack.push (Data.( * ) a b) stk;
        eval_rec (String.sub exp 1 (String.length exp - 1)) stk)
    else
      (* Try to read number off string *)
      match Data.next exp with
      | Some (new_exp, value) ->
          Stack.push value stk;
          eval_rec new_exp stk
      | None -> Error "illegal character"

  let eval (s : string) : (t, string) result = eval_rec s (Stack.create ())
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

  (* Compute gcd of two integers *)
  let rec gcd ((a, b) : t) : int =
    if a < b then gcd (b, a) else if b = 0 then a else gcd (b, a mod b)

  (* Reduce fraction using gcd *)
  let reduce ((p, q) : t) : int * int =
    let d = if p < 0 then gcd (-p, q) else gcd (p, q) in
    (Int.div p d, Int.div q d)

  let ( + ) ((ap, aq) : t) ((bp, bq) : t) =
    reduce ((ap * bq) + (bp * aq), aq * bq)

  let ( * ) ((ap, aq) : t) ((bp, bq) : t) = reduce (ap * bp, aq * bq)

  let of_string (s : string) : t option =
    match String.index_opt s '/' with
    (* Read rational with no / *)
    | None -> (
        match int_of_string_opt s with Some a -> Some (a, 1) | None -> None)
    (* If / found, read numerator and denominator *)
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
