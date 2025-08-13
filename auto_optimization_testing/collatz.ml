let rec collatz inp =
    if inp = 1 then
        0
     else 
        let res1 = inp / 2 in 
        let res2 = res1 * 2 in
            if res2 = inp then
                let part_res = collatz res1 in 
                    part_res + 1
            else 
                let res3 = 3 * inp + 1 in
                let res4 = collatz res3 in
                    1 + res4
