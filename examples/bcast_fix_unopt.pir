broad_cast_opt input_num :=

    if A.(input_num > 2) then
    A[L] ~> B;
    A[L] ~> C;

        {-- This is the part where B sends to D and E --}
        let B.x := B.10; in
        let B.cond_koc := [A] A.1 ~> B; in
        if B.(cond_koc = 1) then
        B[L] ~> D;
        B[L] ~> E;

            let D.x := D.10; in
            let E.x := E.10; in 
            ()
        
        else 
        B[R] ~> D;
        B[R] ~> E;
            
            let D.x := D.9; in
            let E.x := E.9; in
            ();
        
        {-- This is the part where C sends to F and G --}
        let C.x := C.10; in
        let C.cond_koc := [A] A.1 ~> C; in
        if C.(cond_koc = 1)
        C[L] ~> F;
        C[L] ~> G;

            let F.x := F.10; in
            let G.x := G.10; in
            ()

        else
        C[R] ~> F;
        C[R] ~> G;

            let F.x := F.9; in
            let G.x := G.9; in
            ();
    
    else 
    A[R] ~> B;
    A[R] ~> C;
    
    A.print_endline A."Computation not possible"
