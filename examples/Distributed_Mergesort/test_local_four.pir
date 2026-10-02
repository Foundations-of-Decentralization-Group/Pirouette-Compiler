foreign sort : unit -> unit := "Listutils:sort";

split_A original_list list_one list_two :=
  match original_list with
  | A.[] -> A.(list_one, list_two)
  | A.(head_one :: head_two :: tail) ->
      split_A A.tail A.(head_one :: list_one) A.(head_two :: list_two)
  | A.(head :: []) ->
      A.(head :: list_one, list_two);

split_B original_list list_one list_two :=
  match original_list with
  | B.[] -> B.(list_one, list_two)
  | B.(head_one :: head_two :: tail) ->
      split_B B.tail B.(head_one :: list_one) B.(head_two :: list_two)
  | B.(head :: []) ->
      B.(head :: list_one, list_two);

split_C original_list list_one list_two :=
  match original_list with
  | C.[] -> C.(list_one, list_two)
  | C.(head_one :: head_two :: tail) ->
      split_C C.tail C.(head_one :: list_one) C.(head_two :: list_two)
  | C.(head :: []) ->
      C.(head :: list_one, list_two);

main :=
  let A.unsplit_list_0 := A.[4; 3; 2; 1; 8; 7; 6; 5]; in

  let A.return_val := split_A A.unsplit_list_0 A.[] A.[]; in
  let A.first_list_0 := A.(fst return_val); in
  let A.second_list_0 := A.(snd return_val); in

  let B.unsplit_list_1 := [A] A.first_list_0 ~> B; in
  let C.unsplit_list_1 := [A] A.second_list_0 ~> C; in

  let B.return_val := split_B B.unsplit_list_1 B.[] B.[]; in
  let B.first_list_0 := B.(fst return_val); in
  let B.second_list_0 := B.(snd return_val); in

  let D.unsplit_list_1 := [B] B.first_list_0 ~> D; in
  let E.unsplit_list_1 := [B] B.second_list_0 ~> E; in

  let C.return_val := split_C C.unsplit_list_1 C.[] C.[]; in
  let C.first_list_0 := C.(fst return_val); in
  let C.second_list_0 := C.(snd return_val); in

  let F.unsplit_list_1 := [C] C.first_list_0 ~> F; in
  let G.unsplit_list_1 := [C] C.second_list_0 ~> G; in

  let D.sorted_list := D.sort D.unsplit_list_1; in
  let E.sorted_list := E.sort E.unsplit_list_1; in
  let F.sorted_list := F.sort F.unsplit_list_1; in
  let G.sorted_list := G.sort G.unsplit_list_1; in

  let C.merge_list_F := [F] F.sorted_list ~> C; in
  let C.merge_list_G := [G] G.sorted_list ~> C; in
  let C.merged_list :=
    C.(lfun merge input :=
      match input with
      | (head_l :: tail_l, head_r :: tail_r) ->
          (match head_l <= head_r with
           | true -> head_l :: merge (tail_l, head_r :: tail_r)
           | false -> head_r :: merge (head_l :: tail_l, tail_r))
      | ([], rest) -> rest
      | (rest, []) -> rest
    in merge (merge_list_F, merge_list_G)); in

  let B.merge_list_D := [D] D.sorted_list ~> B; in
  let B.merge_list_E := [E] E.sorted_list ~> B; in
  let B.merged_list :=
    B.(lfun merge input :=
      match input with
      | (head_l :: tail_l, head_r :: tail_r) ->
          (match head_l <= head_r with
           | true -> head_l :: merge (tail_l, head_r :: tail_r)
           | false -> head_r :: merge (head_l :: tail_l, tail_r))
      | ([], rest) -> rest
      | (rest, []) -> rest
    in merge (merge_list_D, merge_list_E)); in

  let A.merge_list_B := [B] B.merged_list ~> A; in
  let A.merge_list_C := [C] C.merged_list ~> A; in
  let A.merged_list :=
    A.(lfun merge input :=
      match input with
      | (head_l :: tail_l, head_r :: tail_r) ->
          (match head_l <= head_r with
           | true -> head_l :: merge (tail_l, head_r :: tail_r)
           | false -> head_r :: merge (head_l :: tail_l, tail_r))
      | ([], rest) -> rest
      | (rest, []) -> rest
    in merge (merge_list_B, merge_list_C)); in
  A.();