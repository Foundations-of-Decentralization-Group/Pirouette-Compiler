{-- If node A constantly requires a lot of data from node C, wouldn't it make sense to move all A's computations to B to exploit locality rather than having increased costs for message passing? This is to exploit locality --}

{-- Node A--------------------------Node B----------------------Node C --}

Computation_A : B.int -> B.int;

Computation_A val := let B.x := [C] C.x ~> B; in 
                   let B.y := [C] C.y ~> B; in 
                   let B.z := [C] C.z ~> B; in 
		   B.(x + y + z + val);

Computation_B : B.int -> B.int;

Computation_B val := B.(b + n + m);
