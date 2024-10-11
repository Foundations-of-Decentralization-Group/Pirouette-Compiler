{-- For this program, A and B receive C.x separately but in the optimized program, A could receive C.x from B directly instead of getting it from A. --} 


{-- Node A------------------Node B----------------Node C --}


Computation_B val := let B.val := B.(x + y + z); in B.(val + q);
                     B.rx_val := [C] C.x ~> B;
		     B.final_res := B.(val + rx_val); 

Computation_A val := let A.val := A.(x + y + z); in A.(val + q);
                     A.rx_val := [C] C.x ~> A;
		     A.final_res := A.(val + rx_val); 
