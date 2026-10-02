main :=
  let A.result :=
    A.(lfun merge input :=
      match input with
      | (head_l :: tail_l, head_r :: tail_r) ->
          (match head_l <= head_r with
           | true -> head_l :: merge (tail_l, head_r :: tail_r)
           | false -> head_r :: merge (head_l :: tail_l, tail_r))
      | ([], rest) -> rest
      | (rest, []) -> rest
    in merge ([1; 3], [2; 4])); in
  A.();