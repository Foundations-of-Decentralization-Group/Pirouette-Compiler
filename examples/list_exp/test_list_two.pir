main :=
  A.(match [1, 2, 3] with
   | [1, 2, 3] -> let x : int := 5 in x
   | [] -> 7);