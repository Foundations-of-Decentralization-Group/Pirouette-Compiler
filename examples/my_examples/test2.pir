a := A.(3,5);

b := A.(5,5);

isf5 : A.(int*int) -> A.string;
isf5 x := 
	match fst x with
		| A.5 -> A."xs first elem is a 5"
		| _ -> A."xs first elem isnt a 5"
;

main :=
  let c := isf5 a; in
  let _ := A.print_endline c; in

  let d := isf5 b; in
  A.print_endline d;
