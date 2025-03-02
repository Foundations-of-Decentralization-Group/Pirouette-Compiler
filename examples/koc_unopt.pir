broadcast_opt freq :=

    if A.(freq > 0) then
        A[L1] ~> B;
        A[L2] ~> C;
        A[L3] ~> D;
        A[L4] ~> E;
        A[L5] ~> F;
        A[L5] ~> G;
    
            let C.x := C.10; in 
            let D.x := D.10; in
            let E.x := E.10; in
            let F.x := F.10; in
            let G.x := G.10; in broadcast_opt A.(freq - 1)

    else 
        A[R1] ~> B;
        A[R2] ~> C;
        A[R3] ~> D;
        A[R4] ~> E;
        A[R5] ~> F;
        A[R6] ~> G;
     
            let B.x := B.9; in
            let C.x := C.9; in 
            let D.x := D.9; in
            let E.x := E.9; in
            let F.x := F.9; in
            let G.x := G.9; in G.print_endline G."Terminate";

main := broadcast_opt A.1000000;