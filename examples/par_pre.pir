par_pre := 

let A.x := A.1; in
let B.x := B.2; in
let C.x := C.3; in
let D.x := D.4; in

    let B.val_from_A := [A] A.x ~> B; in
    let B.final_result := B.(val_from_A + x); in

    let D.val_from_C := [C] C.x ~> D; in
    let D.intermediate_result := D.(val_from_C + x); in

    let C.val_from_B := [B] B.final_result ~> C; in
    let C.final_result := C.(val_from_B + x); in

    let D.val_from_B := [B] B.final_result ~> D; in
    let D.final_result := D.(val_from_B + intermediate_result); in
    D.print_int D.final_result;

main := par_pre;