main :=
  A.(lfun length x :=
    match x with
    | [] -> 0
    | head :: tail -> 1 + length tail
  in length [1; 2; 3]);