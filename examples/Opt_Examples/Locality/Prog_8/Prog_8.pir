{-- If node A constantly requires a lot of data from node C, wouldn't it make sense to move all A's computations to B to exploit locality rather than having increased costs for message passing? This is to exploit locality --}

{-- Node A--------------------------Node B----------------------Node C --}

Computation_A : A.int -> A.int;

Computation_A val := let A.x := [C] C.x ~> A; in 
                   let A.y := [C] C.y ~> A; in 
                   let A.z := [C] C.z ~> A; in 
		   A.(x + y + z + val);

Computation_B : B.int -> B.int;

Computation_B val := B.(b + n + m);
