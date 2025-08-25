A.test_collatz inp :=

        if A.(inp = 1)
        then 
            A.0
        else 
            let A.res1 := A.(inp/2); in 
            let A.res2 := A.(res1 * 2); in
                if A.(res2 = inp) then
                    let A.part_res := A.test_collatz A.(res1); in A.(part_res + 1)
                else 
                    let A.res3 := A.(3 * inp + 1); in
                    let A.res4 := A.test_collatz A.res3; in
                    A.(1 + res4);

B.test_collatz inp :=

        if B.(inp = 1)
        then 
            B.0
        else 
            let B.res1 := B.(inp/2); in 
            let B.res2 := B.(res1 * 2); in
                if B.(res2 = inp) then
                    let B.part_res := B.test_collatz B.(res1); in B.(part_res + 1)
                else 
                    let B.res3 := B.(3 * inp + 1); in
                    let B.res4 := B.test_collatz B.res3; in
                    B.(1 + res4);
		   		   
C.test_collatz inp :=

        if C.(inp = 1)
        then 
            C.0
        else 
            let C.res1 := C.(inp/2); in 
            let C.res2 := C.(res1 * 2); in
                if C.(res2 = inp) then
                    let C.part_res := C.test_collatz C.(res1); in C.(part_res + 1)
                else 
                    let C.res3 := C.(3 * inp + 1); in
                    let C.res4 := C.test_collatz C.res3; in
                    C.(1 + res4);
		   
D.test_collatz inp :=

        if D.(inp = 1)
        then 
            D.0
        else 
            let D.res1 := D.(inp/2); in 
            let D.res2 := D.(res1 * 2); in
                if D.(res2 = inp) then
                    let D.part_res := D.test_collatz D.(res1); in D.(part_res + 1)
                else 
                    let D.res3 := D.(3 * inp + 1); in
                    let D.res4 := D.test_collatz D.res3; in
                    D.(1 + res4);

broadcast_opt freq :=

    if A.(freq > 0) then
    A[L1] ~> B;
    A[L2] ~> C;
    A[L3] ~> D;


        let B.result  := B.test_collatz B.931386509544713451; in 
        let A.reply_B := [B] B.result ~> A; in

        let C.result  := C.test_collatz C.931386509544713451; in
        let A.reply_C := [C] C.result ~> A; in 

        let D.result  := D.test_collatz D.931386509544713451; in
        let A.reply_D := [D] D.result ~> A; in 
        broadcast_opt A.(freq - 1)


    else 
    A[R1] ~> B;
    A[R2] ~> C;
    A[R3] ~> D;

        A.print_endline A."Done with all the computations";


main := broadcast_opt A.100;