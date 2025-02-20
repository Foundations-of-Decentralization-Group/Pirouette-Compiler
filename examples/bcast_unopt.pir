{-- The main aim of this program is to broadcast a value to multiple participants B,C,D,E,F,G from a single participant A --}

broadcast_unopt input_num :=
     let A.x := A.input_num;  in 
     let B.x := [A] A.x ~> B; in  
     let C.x := [A] A.x ~> C; in
     let D.x := [A] A.x ~> D; in
     let E.x := [A] A.x ~> E; in
     let F.x := [A] A.x ~> F; in
     let G.x := [A] A.x ~> G; in G.print_endline G."A has finished broadcasting all the values";

main := broadcast_unopt A.2;