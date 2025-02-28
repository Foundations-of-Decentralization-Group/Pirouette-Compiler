{-- In this case, A sends a bunch of messages to B with regard to knowledge of choice - this is the program for optimization --}


B.factorial n := if B.(n <= 1) then B.1 else B.factorial B.(n - 1);



broadcast_opt freq :=

    if A.(freq > 0) then
    A[L] ~> B;

        let B.result := B.factorial B.10; in 
        let A.reply_B := [B] B.result ~> A; in
        A.print_int A.reply_B

    else 
    A[R] ~> B;
        
        B.print_endline B."Terminate";



main := broadcast_opt A.10;