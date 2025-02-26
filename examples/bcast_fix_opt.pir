broad_cast_opt input_num :=

        let A.result := if A.(input_num > 2) then A.1 else A.0; in 

        let B.cond_koc := [A] A.result ~> B; in

        let C.cond_koc := [A] A.result ~> C; in

        if B.(cond_koc = 1) then
        B[L] ~> D;
        B[L] ~> E;

            if C.(cond_koc = 1) then
            C[L] ~> F;
            C[L] ~> G;

            let D.x := D.10; in
            let E.x := E.10; in 
            let F.x := F.10; in
            let G.x := G.10; in
            ()

            else 
            C[R] ~> F;
            C[R] ~> G;

            let D.x := D.0; in
            let E.x := E.0; in
            let F.x := F.0; in
            let G.x := G.0; in
            ()
        
        else 
        B[R] ~> D;
        B[R] ~> E;

            if C.(cond_koc = 1) then
            C[R] ~> F;
            C[R] ~> G;

            let D.x := D.10; in
            let E.x := E.10; in 
            let F.x := F.10; in
            let G.x := G.10; in
            ()

            else 
            C[R] ~> F;
            C[R] ~> G;

            let D.x := D.11; in
            let E.x := E.11; in
            let F.x := F.11; in
            let G.x := G.11; in
            ();

main := broad_cast_opt A.4;
