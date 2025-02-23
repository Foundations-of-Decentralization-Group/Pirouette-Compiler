{-- The main motive behind this program is to split up sending a broadcast over multiple participants by enlisting some help from other participants --}

broadcast_opt input_num :=

     let A.x := A.input_num;  in
     let B.x := [A] A.x ~> B; in
     let C.x := [A] A.x ~> C; in
     let D.x := [B] B.x ~> D; in
     let E.x := [B] B.x ~> E; in
     let F.x := [C] C.x ~> F; in
     let G.x := [C] C.x ~> G; in
     let H.x := [D] D.x ~> H; in
     let I.x := [D] D.x ~> I; in
     let J.x := [E] E.x ~> E; in
     let K.x := [E] E.x ~> E; in
     let L.x := [F] F.x ~> L; in
     let M.x := [F] F.x ~> M; in
     let N.x := [G] G.x ~> N; in
     let O.x := [G] G.x ~> O; in O.print_endline O."Done with all broadcasting with assist";
     
main := broadcast_opt A.2;