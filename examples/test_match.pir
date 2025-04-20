
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


A.factorial n := if A.(n <= 1) then A.1 else let A.fact := A.factorial A.(n - 1); in A.(fact * n);

A.runner inp1 inp2 freq :=

        if A.(freq > 0) then
	
        let A.par_res1 := A.factorial inp1; in
	let A.par_res2 := A.test_collatz inp2; in
        let A.result_one := A.(par_res1 - par_res2); in 

        let A.inp3 := A.(inp1 - 1); in
	let A.inp4 := A.(inp2 - 1); in
        let A.par_res3 := A.factorial inp3; in
	let A.par_res4 := A.test_collatz inp4; in
	let A.result_two := A.(par_res3 / par_res4); in A.runner inp1 inp2 A.(freq - 1)

        else

        let A.result_three := A.3;
                    
main := let A.final_result := A.runner A.20 A.989345275647 A.100; in A.print_int A.final_result;
