B.factorial n := if B.(n <= 1) then B.1 else let B.fact := B.factorial B.(n - 1); in B.(fact * n);
C.factorial n := if C.(n <= 1) then C.1 else let C.fact := C.factorial C.(n - 1); in C.(fact * n);


broadcast_opt freq :=

    if A.(freq > 0) then
    A[L1] ~> B;
    B[L2] ~> C;

        let B.result  := B.factorial B.20; in 
        let A.reply_B := [B] B.result ~> A; in

        let C.result  := C.factorial C.20; in
        let A.reply_C := [C] C.result ~> A; in 

        broadcast_opt A.(freq - 1)


    else 
    A[R1] ~> B;
    B[R2] ~> C;

        A.print_endline A."Done with all the computations";

main := broadcast_opt A.10000000;