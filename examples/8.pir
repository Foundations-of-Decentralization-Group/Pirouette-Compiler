{-- This is to make a loop using recursion --}

--y : R1.int;
--y := R1.5;

--Looper := Looper

{-- Factorial X := 
        let P8.x := X; in 
		if P8.(x = 0 || x = 1)
			then P8.1
		else 
			let P8.y := Factorial P8.(x - 1); in
	 		P8.(x * y);

Factorial P1.5; --}

Looper3 := 

        if R.(3>5) then
	let R.x := [Q] Q.3 ~> R; in
	Looper3
	else
	let R.x := [Q] Q.4 ~> R; in
	Looper3;

