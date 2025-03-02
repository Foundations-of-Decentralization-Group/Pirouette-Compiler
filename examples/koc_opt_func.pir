B.factorial n := if B.(n <= 1) then B.1 else let B.fact := B.factorial B.(n - 1); in B.(fact * n);
C.factorial n := if C.(n <= 1) then C.1 else let C.fact := C.factorial C.(n - 1); in C.(fact * n);
D.factorial n := if D.(n <= 1) then D.1 else let D.fact := D.factorial D.(n - 1); in D.(fact * n);
E.factorial n := if E.(n <= 1) then E.1 else let E.fact := E.factorial E.(n - 1); in E.(fact * n);
F.factorial n := if F.(n <= 1) then F.1 else let F.fact := F.factorial F.(n - 1); in F.(fact * n);

broadcast_opt freq :=

    if A.(freq > 0) then
    A[L1] ~> B;
    A[L2] ~> C;
    B[L3] ~> D;
    B[L4] ~> E;
    C[L5] ~> F;
    C[L6] ~> G;

        let B.result  := B.factorial B.10; in 
        let A.reply_B := [B] B.result ~> A; in

        let C.result  := C.factorial C.10; in
        let A.reply_C := [C] C.result ~> A; in 

        let D.result  := D.factorial D.10; in
        let A.reply_D := [D] D.result ~> A; in 

        let E.result  := E.factorial E.10; in 
        let A.reply_E := [E] E.result ~> A; in 

        let F.result  := F.factorial F.10; in 
        let A.reply_F := [F] F.result ~> A; in 

        broadcast_opt A.(freq - 1)

    else 
    A[R1] ~> B;
    A[R2] ~> C;
    B[R3] ~> D;
    B[R4] ~> E;
    C[R5] ~> F;
    C[R6] ~> G;

        A.print_endline A."Done with all the computation";

main := broadcast_opt A.1000000;