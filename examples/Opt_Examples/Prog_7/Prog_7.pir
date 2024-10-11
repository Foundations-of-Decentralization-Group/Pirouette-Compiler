{-- If node A requires a value from node C at a certain point within its execution; wouldn't it make sense for C to proactively send the value to B/(or even to A) which is much closer? A can then directly get the value from B; This could be construed as latency hiding/prefetching. --}

{-- Node A------------------Node B----------------Node C --}

Computation_A val := let A.val := A.(x + y + z); in A.(val + q);
                     A.rx_val := [C] C.x ~> A;
		     A.final_res := A.(val + rx_val); 
