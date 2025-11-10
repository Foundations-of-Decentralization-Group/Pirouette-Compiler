broadcast_unopt freq :=

    if A.(freq > 0) then
    A[L] ~> B;

        let B.x := B."10"; in
	let A.result_B := [B] B.x ~> A; in 
	broadcast_unopt A.(freq - 1)

    else 
    A[R] ~> B;

        let B.x := B."9"; in
	let A.result_B := [B] B.x ~> A; in 
        A.print_endline A."Terminate - Unoptimized";

main := broadcast_unopt A.3;