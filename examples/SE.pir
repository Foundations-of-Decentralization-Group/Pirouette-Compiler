B.x : B.int;
B.x := B.100;
B.y := B.120;

send_expression := 

    --let A.z := fun A.x -> A.print_endline A."This works";--

    let A.z := fun A.x -> prnt; in 
    let B.z := [A] A.z ~> B; in B.print_int B.z;

