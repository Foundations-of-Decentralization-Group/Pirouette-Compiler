broadcast_opt freq :=

    if A.(freq > 0) then
        A[L1] ~> B;
        A[L2] ~> C;

            let B.x := B.10; in 
            let C.x := C.10; in broadcast_opt A.(freq - 1)
    else 
        A[R1] ~> B;
        A[R2] ~> C;
     
            let B.x := B.9; in
            let C.x := C.9; in  C.print_endline C."Terminate";

main := broadcast_opt A.1000000;