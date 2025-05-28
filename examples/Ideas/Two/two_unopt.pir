A.collatz_calc inp :=

if A.(inp > 1) then 
   let A.res1 := A.(inp / 2); in
   let A.res2 := A.(2 * res1); in
   if A.(inp = res2)
      then let A.result := A.(res1); in
      let final_res := A.collatz_calc A.result; in
      A.(final_res + 1)
   else
      let A.result := A.(3 * inp); in
      let A.mid_res := A.(result + 1); in
      let A.final_res := A.collatz_calc A.mid_res; in
      A.(final_res + 1)
else
   A.0;

B.collatz_calc inp :=

if B.(inp > 1) then 
   let B.res1 := B.(inp / 2); in
   let B.res2 := B.(2 * res1); in
   if B.(inp = res2)
      then let B.result := B.(res1); in
      let final_res := B.collatz_calc B.result; in
      B.(final_res + 1)
   else
      let B.result := B.(3 * inp); in
      let B.mid_res := B.(result + 1); in
      let B.final_res := B.collatz_calc B.mid_res; in
      B.(final_res + 1)
else
   B.0;

B.sum_three inp1 inp2 inp3 := B.(inp1 + inp2 + inp3);
B.sub_three inp2 inp2 inp3 := B.(inp1 - inp2 - inp3);
 
main :=

let A.result := A.collatz_calc A.17; in

    if A.(result > 5) then
       A[L] ~> B;
       
       let B.res_one := B.collatz_calc B.30; in
       let B.result := B.sum_three B.res_one B.res_one B.res_one; in B.()
       
    else
       A[R] ~> B;
       
       let B.res_one := B.collatz_calc B.40; in
       let B.result := B.sub_three B.res_one B.res_one B.res_one; in B.();
    
      
