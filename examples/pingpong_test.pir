pingpong counter init_value :=

         let A.x := A.init_value; in

         if A.(counter < 10)
	 
	    then A[L] ~> B;
	         A[L] ~> C;

		let B.x := [A] A.x ~> B; in
                  
	        if B.(x = 0)
		then B[L] ~> A;
		     B[L] ~> C; 

                   let B.y := B.(x + 1); in
		   let C.x := [B] B.y ~> C; in

                       if C.(x = 1)
		       {-- B has to be able to know when to terminate; this is based on C's choice of either restarting the whole process from A or stopping all computation --}
		       then C [L] ~> A;
                            C [R] ~> B;

                           let A.new_counter := A.(counter + 1); in
                           pingpong A.new_counter A.init_value	

                       else
                          C[R] ~> A;
                          C[L] ~> B;

                           C.print_endline C."Mismatch at C"

                else	
		B[R] ~> A;
                B[R] ~> C;

		   B.print_endline B."Failed at B"

          else
             A[R] ~> B;
             A[R] ~> C;

             A.print_endline A."All iterations are done";
	
 main := pingpong A.0 A.0;