main := let B.x := B.11; in let A.x := [B] B.x ~> A; in
        let A.y := A.10; in let A.z := A.12; in
	let A.sum := A.(x + y + z); in A.();