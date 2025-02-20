{-- The main motive behind this program is to split up sending a broadcast over multiple participants --}

broadcast_opt input_num :=
     let A.x := A.input_num;  in
     let B.x := [A] A.x ~> B; in
     let C.x := [A] A.x ~> C; in
     let D.x := [B] B.x ~> D; in
     let E.x := [B] B.x ~> E; in
     let F.x := [C] C.x ~> F; in
     let G.x := [C] C.x ~> G; in G.print_endline G."Done with all broadcasting with assist";
     
main := broadcast_opt A.2;