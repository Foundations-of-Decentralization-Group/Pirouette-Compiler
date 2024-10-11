{-- This program keeps sending the values to R within a loop. In the optimized version the values could be stored within node R --} 

Looper3 : R.unit -> R.unit;
Looper3 := 

        if R.(a > 5) then
	let R.x := [Q] Q.3 ~> R; in
	let R.y := R.(x + 6); in 
	Looper3 ()
	else
	let R.x := [Q] Q.4 ~> R; in
	let R.y := R.(x + 7); in
	Looper3 ();

y := Looper3; 
