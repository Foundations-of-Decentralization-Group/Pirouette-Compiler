foreign print_val : unit -> unit := "Stdlib:Printf.printf";

ring_all_reduce :=

     let A.x := A.1; in
     let B.x := B.2; in
     let C.x := C.3; in
     let D.x := D.4; in
     let E.x := E.5; in
     let F.x := F.6; in
     let G.x := G.7; in 

     let A.sent_val_one := [G] G.x ~> A; in
     let B.sent_val_one := [A] A.x ~> B; in
     let C.sent_val_one := [B] B.x ~> C; in
     let D.sent_val_one := [C] C.x ~> D; in
     let E.sent_val_one := [D] D.x ~> E; in
     let F.sent_val_one := [E] E.x ~> F; in
     let G.sent_val_one := [F] F.x ~> G; in 

     let A.sent_val_two := [G] G.sent_val_one ~> A; in 
     let B.sent_val_two := [A] A.sent_val_one ~> B; in
     let C.sent_val_two := [B] B.sent_val_one ~> C; in
     let D.sent_val_two := [C] C.sent_val_one ~> D; in
     let E.sent_val_two := [D] D.sent_val_one ~> E; in
     let F.sent_val_two := [E] E.sent_val_one ~> F; in
     let G.sent_val_two := [F] F.sent_val_one ~> G; in 

     let A.sent_val_three := [G] G.sent_val_two ~> A; in 
     let B.sent_val_three := [A] A.sent_val_two ~> B; in
     let C.sent_val_three := [B] B.sent_val_two ~> C; in
     let D.sent_val_three := [C] C.sent_val_two ~> D; in
     let E.sent_val_three := [D] D.sent_val_two ~> E; in
     let F.sent_val_three := [E] E.sent_val_two ~> F; in
     let G.sent_val_three := [F] F.sent_val_two ~> G; in 

     let A.sent_val_four := [G] G.sent_val_three ~> A; in 
     let B.sent_val_four := [A] A.sent_val_three ~> B; in
     let C.sent_val_four := [B] B.sent_val_three ~> C; in
     let D.sent_val_four := [C] C.sent_val_three ~> D; in
     let E.sent_val_four := [D] D.sent_val_three ~> E; in
     let F.sent_val_four := [E] E.sent_val_three ~> F; in
     let G.sent_val_four := [F] F.sent_val_three ~> G; in 

     let A.sent_val_five := [G] G.sent_val_four ~> A; in 
     let B.sent_val_five := [A] A.sent_val_four ~> B; in
     let C.sent_val_five := [B] B.sent_val_four ~> C; in
     let D.sent_val_five := [C] C.sent_val_four ~> D; in
     let E.sent_val_five := [D] D.sent_val_four ~> E; in
     let F.sent_val_five := [E] E.sent_val_four ~> F; in
     let G.sent_val_five := [F] F.sent_val_four ~> G; in 

     let A.sent_val_six := [G] G.sent_val_five ~> A; in 
     let B.sent_val_six := [A] A.sent_val_five ~> B; in
     let C.sent_val_six := [B] B.sent_val_five ~> C; in
     let D.sent_val_six := [C] C.sent_val_five ~> D; in
     let E.sent_val_six := [D] D.sent_val_five ~> E; in
     let F.sent_val_six := [E] E.sent_val_five ~> F; in
     let G.sent_val_six := [F] F.sent_val_five ~> G; in 


     let A.result := A.(x + sent_val_one + sent_val_two + sent_val_three + sent_val_four + sent_val_five + sent_val_six); in
     let B.result := B.(x + sent_val_one + sent_val_two + sent_val_three + sent_val_four + sent_val_five + sent_val_six); in
     let C.result := C.(x + sent_val_one + sent_val_two + sent_val_three + sent_val_four + sent_val_five + sent_val_six); in
     let D.result := D.(x + sent_val_one + sent_val_two + sent_val_three + sent_val_four + sent_val_five + sent_val_six); in
     let E.result := E.(x + sent_val_one + sent_val_two + sent_val_three + sent_val_four + sent_val_five + sent_val_six); in
     let F.result := F.(x + sent_val_one + sent_val_two + sent_val_three + sent_val_four + sent_val_five + sent_val_six); in
     let G.result := G.(x + sent_val_one + sent_val_two + sent_val_three + sent_val_four + sent_val_five + sent_val_six); in      
     
     let A._ := A.print_val A."This is the value of A %d" A.result; in
     let B._ := B.print_val B."This is the value of B %d" B.result; in
     let C._ := C.print_val C."This is the value of C %d" C.result; in
     let D._ := D.print_val D."This is the value of D %d" D.result; in
     let E._ := E.print_val E."This is the value of E %d" E.result; in
     let F._ := F.print_val F."This is the value of F %d" F.result; in
     let G._ := G.print_val G."This is the value of G %d" G.result; in               
     G.();

main := ring_all_reduce;