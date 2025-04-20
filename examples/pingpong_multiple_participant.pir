{-- The aim of this program is for a participant to send a value to the next participant.
After the first participant, all the rest will increment the value by one before sending it.
--}

pingpong init_value freq :=

         let A.x := A.init_value; in
         if A.(freq < 100000)
	 
         {-- All the participants that are affected by this conditional 1 should be informed of the changes happening here; these will be marked by an X --}
	 
	    then A[L] ~> B;
                 A[L] ~> C;

         {-- X; B is affected by the conditional 1 --------------------> 1 --}
	 
		let B.x := [A] A.x ~> B; in

	        if B.(x = 0)

                {-- All the participants that are affected by this conditional 2 should be informed of the changes happening here; these will be marked by an X2 --}
		then B[L] ~> A;
                     B[L] ~> C;

                   let B.y := B.(x + 1); in

                   {-- X; C is affected by the conditional 1 -----------> 1 --}
		   {-- X2; C is paffected by the conditional 2 ----------> 2 --}
		   
                   let C.x := [B] B.y ~> C; in

                   {-- All the participants that are affected by this conditional 3 should be informed of the changes happening here; these will be marked by an X3 --}
		   if C.(x = 2) then
		   C[L] ~> A;
		   C[L] ~> B;

                       {-- X3; A is affected by the conditional 3 -----> 3 --}
			let A.new_freq := A.(freq + 1); in
		   	pingpong A.init_value A.new_freq
		   
		   else
		   C[R] ~> A;
                   C[R] ~> B;

			C.print_endline C."Failed at C"

                else	
		B[R] ~> A;
		B[R] ~> C;
		
		   B.print_endline B."Failed at B"

          else
             A[R] ~> B;
             A[R] ~> C;	     
             A.print_endline A."All iterations are done";
	
 main := pingpong A.1 A.100000;
