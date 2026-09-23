foreign printf : unit -> unit := "Printf.printf";
foreign add_to_list : unit -> unit := "List.cons";
foreign list_iter : unit -> unit := "List.iter";
foreign merge : unit -> unit := "Listutils:merge";
foreign sort : unit -> unit := "Listutils:sort";

split_A original_list list_one list_two :=

 match original_list with
     | A.[] -> A.(list_one,list_two)
     | A.(head_one::head_two::tail) -> let A.new_list_one := A.add_to_list A.head_one A.list_one; in
                                       let A.new_list_two := A.add_to_list A.head_two A.list_two; in
				       split_A A.tail A.new_list_one A.new_list_two
				       
     | A.(head::[]) -> let A.newlist_one := A.add_to_list A.head A.list_one; in
                       A.(newlist_one,list_two);

split_B original_list list_one list_two :=

 match original_list with
     | B.[] -> B.(list_one,list_two)
     | B.(head_one::head_two::tail) -> let B.new_list_one := B.add_to_list B.head_one B.list_one; in
                                       let B.new_list_two := B.add_to_list B.head_two B.list_two; in
				       split_B B.tail B.new_list_one B.new_list_two
				       
     | B.(head::[]) -> let B.newlist_one := B.add_to_list B.head B.list_one; in
                       B.(newlist_one,list_two);

split_C original_list list_one list_two :=

 match original_list with
     | C.[] -> C.(list_one,list_two)
     | C.(head_one::head_two::tail) -> let C.new_list_one := C.add_to_list C.head_one C.list_one; in
                                       let C.new_list_two := C.add_to_list C.head_two C.list_two; in
				       split_C C.tail C.new_list_one C.new_list_two
				       
     | C.(head::[]) -> let C.newlist_one := C.add_to_list C.head C.list_one; in
                       C.(newlist_one,list_two);

{-merge_G input_tuple sorted_list :=

  match input_tuple with
  | (G.(head_l :: tail_l), G.(head_r :: tail_r)) ->
      if G.(head_l <= head_r)
      then let G.new_sorted_list := G.add_to_list G.head_l G.sorted_list; in
           merge_G (G.tail_l, G.tail_r) G.new_sorted_list
      else
          let G.new_sorted_list := G.add_to_list G.head_r G.sorted_list; in
           merge_G (G.tail_l, G.tail_r) G.new_sorted_list
  | (G.[], G.rest_r) -> G.rest_r
  | (G.rest_l, G.[]) -> G.rest_l;
-}

main :=
 let A.unsplit_list_0 := A.[4,3,2,1]; in
 
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

 let F.merged_list := F.sort F.unsplit_list_1; in
 let G.merged_list := G.sort G.unsplit_list_1; in
 let C.merge_list_F := [F] F.merged_list ~> C; in
 let C.merge_list_G := [G] G.merged_list ~> C; in 
 let C.merged_list := C.merge (C.merge_list_F,C.merge_list_G); in 

 let D.merged_list := D.sort D.unsplit_list_1; in
 let E.merged_list := E.sort E.unsplit_list_1; in
 let B.merge_list_D := [D] D.merged_list ~> B; in
 let B.merge_list_E := [E] E.merged_list ~> B; in
 let B.merged_list := B.merge (B.merge_list_D,B.merge_list_E); in

 let A.merge_list_B := [B] B.merged_list ~> A; in
 let A.merge_list_C := [C] C.merged_list ~> A; in
 let A.merged_list := A.merge (A.merge_list_B,A.merge_list_C); in
 A.();
