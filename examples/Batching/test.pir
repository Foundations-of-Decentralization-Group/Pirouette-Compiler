main :=
     let A.x := A.5; in
     let A.y := A.6; in
     let B.x := [A] A.x ~> B; in
     let B.y := [A] A.y ~> B; in
     B.();