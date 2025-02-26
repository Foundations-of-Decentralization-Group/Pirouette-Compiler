pingpong counter init_value :=

         let A.x := A.init_value; in

         if A.(counter < 10)
	 
	    then A[L] ~> B;
	         A[L] ~> C;
                 A[L] ~> D;
                 A[L] ~> E;
                 A[L] ~> F;
                 A[L] ~> G; 
                 A[L] ~> H;                
             
                
		let B.x := [A] A.x ~> B; in
                  
	        if B.(x = 0)
		then B[L] ~> A;
		     B[L] ~> C; 
                     B[L] ~> D;
                     B[L] ~> E;
                     B[L] ~> F;
                     B[L] ~> G;  
                     B[L] ~> H;               


                   let B.y := B.(x + 1); in
		         let C.x := [B] B.y ~> C; in

                       if C.(x = 1)
		       {-- B has to be able to know when to terminate; this is based on C's choice of either restarting the whole process from A or stopping all computation --}
		       then C[L] ~> A;
                            C[L] ~> B;
                            C[L] ~> D;
                            C[L] ~> E;
                            C[L] ~> F;
                            C[L] ~> G;
                            C[L] ~> H;                 

                            let C.y := C.(x + 1); in
                            let D.x := [C] C.y ~> D; in

                            if D.(x = 2)
                            then D[L] ~> A;
                                 D[L] ~> B;
                                 D[L] ~> C;
                                 D[L] ~> E;
                                 D[L] ~> F;
                                 D[L] ~> G;          
                                 D[L] ~> H;       


                                 let D.y := D.(x + 1); in 
                                 let E.x := [D] D.y ~> E; in

                                 if E.(x = 3)
                                 then E[L] ~> A;
                                      E[L] ~> B;
                                      E[L] ~> C;
                                      E[L] ~> D;
                                      E[L] ~> F;
                                      E[L] ~> G;  
                                      E[L] ~> H;               
                                
                                      let E.y := E.(x + 1); in
                                      let F.x := [E] E.y ~> F; in

                                      if F.(x = 4) 
                                      then F[L] ~> A;
                                           F[L] ~> B;
                                           F[L] ~> C;
                                           F[L] ~> D;
                                           F[L] ~> E;
                                           F[L] ~> G; 
                                           F[L] ~> H;                
                                        
                                           let F.y := F.(x + 1); in
                                           let G.x := [F] F.y ~> G; in

                                           if G.(x = 5)
                                           then G[L] ~> A;
                                                G[L] ~> B;
                                                G[L] ~> C;
                                                G[L] ~> D;
                                                G[L] ~> E;
                                                G[L] ~> F;
                                                G[L] ~> H;

                                                let G.y := G.(x + 1); in
                                                let H.x := [G] G.y ~> H; in

                                                if H.(x = 6) 
                                                then H[L] ~> A;
                                                     H[L] ~> B;
                                                     H[L] ~> C;
                                                     H[L] ~> D;
                                                     H[L] ~> E;
                                                     H[L] ~> F;
                                                     H[L] ~> G;

                                                     let A.new_counter := A.(counter + 1); in
                                                     pingpong A.new_counter A.init_value

                                                else 
                                                    
                                                     H[R] ~> A;
                                                     H[R] ~> B;
                                                     H[R] ~> C;
                                                     H[R] ~> D;
                                                     H[R] ~> E;
                                                     H[R] ~> F;
                                                     H[R] ~> G;

                                                     H.print_endline H."Mismatch at H"
                                                      

                                            else 
                                                G[R] ~> A;
                                                G[R] ~> B;
                                                G[R] ~> C;
                                                G[R] ~> D;
                                                G[R] ~> E;
                                                G[R] ~> F;
                                                G[R] ~> H;

                                                G.print_endline G."Mismatch at G"


                                       else 

                                           F[R] ~> A;
                                           F[R] ~> B;
                                           F[R] ~> C;
                                           F[R] ~> D;
                                           F[R] ~> E;
                                           F[R] ~> G;
                                           F[R] ~> H;


                                        F.print_endline F."Mismatch at F"	
                            
                                 else 

                                   E[R] ~> A;
                                   E[R] ~> B;
                                   E[R] ~> C;
                                   E[R] ~> D;
                                   E[R] ~> F;
                                   E[R] ~> G;
                                   E[R] ~> H;
                                   
                                    E.print_endline E."Mismatch at E"

                             else

                                 D[R] ~> A;
                                 D[R] ~> B;
                                 D[R] ~> C;
                                 D[R] ~> E;
                                 D[R] ~> F;
                                 D[R] ~> G;
                                 D[R] ~> H;


                                  D.print_endline D."Mismatch at D"

                       else

                          C[R] ~> A;
                          C[R] ~> B;
                          C[R] ~> D;
                          C[R] ~> E;
                          C[R] ~> F;
                          C[R] ~> G;
                          C[R] ~> H;

                           C.print_endline C."Mismatch at C"

                else	

		  B[R] ~> A;
                  B[R] ~> C;
                  B[R] ~> D;
                  B[R] ~> E;
                  B[R] ~> F;
                  B[R] ~> G;
                  B[R] ~> H;

		    B.print_endline B."Failed at B"

          else

             A[R] ~> B;
             A[R] ~> C;
             A[R] ~> D;
             A[R] ~> E;
             A[R] ~> F;
             A[R] ~> G;
             A[R] ~> H;

              A.print_endline A."All iterations are done";
	
 main := pingpong A.0 A.0;