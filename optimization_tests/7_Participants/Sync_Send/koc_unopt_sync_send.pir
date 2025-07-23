broadcapst_unopt freq :=

    if A.(freq > 0) then
    A[L] ~> B;
    A[L] ~> C;    
    A[L] ~> D;
    A[L] ~> E;
    A[L] ~> F;
    A[L] ~> G;

        let B.x := B.10; in
	let A.result_B := [B] B.x ~> A; in 
        let C.x := C.10; in
	let A.result_C := [C] C.x ~> A; in 	
        let D.x := D.10; in
	let A.result_D := [D] D.x ~> A; in 	
        let E.x := E.10; in
	let A.result_E := [E] E.x ~> A; in 	
        let F.x := F.10; in
	let A.result_F := [F] F.x ~> A; in 	
        let G.x := G.10; in
	let A.result_G := [G] G.x ~> A; in 	
	broadcast_unopt A.(freq - 1)

    else 
    A[R] ~> B;
    A[R] ~> C;
    A[R] ~> D;
    A[R] ~> E;
    A[R] ~> F;
    A[R] ~> G;

        let B.x := B.9; in
	let A.result_B := [B] B.x ~> A; in 
        let C.x := C.9; in
	let A.result_C := [C] C.x ~> A; in 	
        let D.x := D.9; in
	let A.result_D := [D] D.x ~> A; in 	
        let E.x := E.9; in
	let A.result_E := [E] E.x ~> A; in 	
        let F.x := F.9; in
	let A.result_F := [F] F.x ~> A; in 	
        let G.x := G.9; in
	let A.result_G := [G] G.x ~> A; 
        in A.print_endline A."Terminate - Unoptimized";

main := broadcast_unopt A.100000;