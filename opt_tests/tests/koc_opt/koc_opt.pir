broadcast freq :=

    if A.(freq > 0) then 
		A[L1] ~> B;
		A[L2] ~> C;
		B[L3] ~> D;
		B[L4] ~> E;
		C[L5] ~> F;
		C[L6] ~> G;

		 	let B._ := B.10; in 
			let C._ := C.10; in
			let D._ := D.10; in
			let E._ := E.10; in
			let F._ := F.10; in
			let G._ := G.10; in broadcast A.(freq - 1)

	else	 
		A[R1] ~> B;
		A[R2] ~> C;
		B[R3] ~> D;
		B[R4] ~> E;
		C[R5] ~> F;
		C[R6] ~> G;

			let B._ := B.11; in
			let C._ := C.11; in
			let D._ := D.11; in
			let E._ := E.11; in
			let F._ := F.11; in
			let G._ := G.11; in G.0;

main :=
    let A.time := A.Unix___gettimeofday A.(); in
    let A._ := A.print_float A.time; in
    let A._ := A.print_string A." "; in
    let A._ := broadcast A.NUM_ITERS; in
    let A.time := A.Unix___gettimeofday A.(); in
    let A._ := A.print_float A.time; in
    A.print_endline A." ";