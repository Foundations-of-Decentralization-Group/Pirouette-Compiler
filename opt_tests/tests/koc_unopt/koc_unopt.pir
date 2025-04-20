broadcast freq :=

    if A.(freq > 0) then
        A[L1] ~> B;
        A[L2] ~> C;
        A[L3] ~> D;
        A[L4] ~> E;
        A[L5] ~> F;
        A[L5] ~> G;
    
            let C._ := C.10; in 
            let D._ := D.10; in
            let E._ := E.10; in
            let F._ := F.10; in
            let G._ := G.10; in broadcast A.(freq - 1)

    else 
        A[R1] ~> B;
        A[R2] ~> C;
        A[R3] ~> D;
        A[R4] ~> E;
        A[R5] ~> F;
        A[R6] ~> G;
     
            let B._ := B.9; in
            let C._ := C.9; in 
            let D._ := D.9; in
            let E._ := E.9; in
            let F._ := F.9; in
            let G._ := G.9; in G.0;

main :=
    let A.time := A.Unix___gettimeofday A.(); in
    let A._ := A.print_float A.time; in
    let A._ := A.print_string A." "; in
    let A._ := broadcast A.NUM_ITERS; in
    let A.time := A.Unix___gettimeofday A.(); in
    let A._ := A.print_float A.time; in
    A.print_endline A." ";