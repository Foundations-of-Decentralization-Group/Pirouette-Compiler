{-- The main aim of this program is to broadcast a value to multiple participants B,C,D,E,F,G from a single participant A --}

broadcast_unopt input_num :=

     let A.x := A.input_num;  in 
     let B.x := [A] A.x ~> B; in  
     let C.x := [A] A.x ~> C; in
     let D.x := [A] A.x ~> D; in
     let E.x := [A] A.x ~> E; in
     let F.x := [A] A.x ~> F; in
     let G.x := [A] A.x ~> G; in
     let H.x := [A] A.x ~> H; in
     let I.x := [A] A.x ~> I; in
     let J.x := [A] A.x ~> J; in
     let K.x := [A] A.x ~> K; in
     let L.x := [A] A.x ~> L; in
     let M.x := [A] A.x ~> M; in
     let N.x := [A] A.x ~> N; in
     let O.x := [A] A.x ~> O; in 
     let P.x := [A] A.x ~> P; in 
     let Q.x := [A] A.x ~> Q; in 
     let R.x := [A] A.x ~> R; in 
     let S.x := [A] A.x ~> S; in 
     let T.x := [A] A.x ~> T; in 
     let U.x := [A] A.x ~> U; in 
     U.print_endline U."A has finished broadcasting all the values";

main := broadcast_unopt A.2;