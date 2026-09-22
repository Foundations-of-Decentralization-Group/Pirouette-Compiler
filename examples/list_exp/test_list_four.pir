main :=
  A.(match [1, 2, 3] with
   | head :: tail -> head
   | [] -> 0);