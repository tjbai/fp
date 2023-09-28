let square_size (grid : int list list) : (int, string) result =
  grid
  |> List.map ~f:List.length (* let this be a comment in a line *)
  |> List.fold
       ~init:(Ok (List.length @@ List.hd_exn grid))
       ~f:(fun acc elt ->
         Result.(acc >>= fun a -> if a = elt then Ok a else Error "not square"))
  |> Result.bind ~f:(fun r ->
         if List.length grid = r then Ok r else Error "not square")
