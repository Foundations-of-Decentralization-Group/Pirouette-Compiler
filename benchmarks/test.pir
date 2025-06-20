commstime E.iterations E.init_num :=

	    if E.(iterations > 0) then
            E[L] ~> F;

               let E.new_iterations := E.(iterations - 1); in
	       let E.new_init_num := E.(init_num + 1); in
	       let F.x := [E] E.new_init_num ~> F; in 
	       commstime E.new_iterations E.new_init_num

            else
            E[R] ~> F;
	    
               E.print_string E."Communications Time completed successfully - else";



main := commstime E.10 E.0;

