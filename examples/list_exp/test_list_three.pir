main :=
  A.(match [1, 2, 3] with
   | [x, y, z] -> x
   | [] -> 0);