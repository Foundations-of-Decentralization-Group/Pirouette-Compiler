B.factorial n := if B.(n <= 1) then B.1 else let B.fact := B.factorial B.(n - 1); in B.(fact * n);
C.factorial n := if C.(n <= 1) then C.1 else let C.fact := C.factorial C.(n - 1); in C.(fact * n);
D.factorial n := if D.(n <= 1) then D.1 else let D.fact := D.factorial D.(n - 1); in D.(fact * n);
E.factorial n := if E.(n <= 1) then E.1 else let E.fact := E.factorial E.(n - 1); in E.(fact * n);
F.factorial n := if F.(n <= 1) then F.1 else let F.fact := F.factorial F.(n - 1); in F.(fact * n);

G.factorial n := 
if G.(n <= 1) 
then G.1 
else 
let G.fact := G.factorial G.(n - 1); in G.(fact * n);

broadcast_opt freq :=

    if A.(freq > 0) then
        
        A[L1] ~> B;

        let B.result  := B.factorial B.20; in 
        let A.reply_B := [B] B.result ~> A; in

        A[L2] ~> C;

        let C.result  := C.factorial C.20; in
        let A.reply_C := [C] C.result ~> A; in 

        A[L3] ~> D;

        let D.result  := D.factorial D.20; in
        let A.reply_D := [D] D.result ~> A; in 

        A[L4] ~> E;

        let E.result  := E.factorial E.20; in 
        let A.reply_E := [E] E.result ~> A; in

        A[L5] ~> F;

        let F.result  := F.factorial F.20; in 
        let A.reply_F := [F] F.result ~> A; in

        A[L6] ~> G;

        let G.result  := G.factorial G.20; in 
        let A.reply_G := [G] G.result ~> A; in

        broadcast_opt A.(freq - 1)

    else 
    A[R1] ~> B;
    A[R2] ~> C;
    A[R3] ~> D;
    A[R4] ~> E;
    A[R5] ~> F;
    A[R6] ~> G;

        A.print_endline A."Done with all the computations";


main := broadcast_opt A.100;