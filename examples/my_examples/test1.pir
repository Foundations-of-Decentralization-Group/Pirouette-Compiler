x : A.(int*int);
x := A.(3,5);

{- x := A.(5,3); -}
b := 
	match fst x with
		| A.5 -> A.print_endline A."xs first elem is a 5"
		| _ -> A.print_endline A."xs first elem isnt a 5"
;
