send_serial :=

	    let A.x := A.5; in
	    
	    let B.x := [A] A.x ~> B; in
	    let C.x := [A] A.x ~> C; in
	    let D.x := [A] A.x ~> D; in
	    let E.x := [A] A.x ~> E; in
	    let F.x := [A] A.x ~> F; in
	    let G.x := [A] A.x ~> G; in

            let B.y := [A] A.x ~> B; in
	    let C.y := [A] A.x ~> C; in
	    let D.y := [A] A.x ~> D; in
	    let E.y := [A] A.x ~> E; in
	    let F.y := [A] A.x ~> F; in
	    let G.y := [A] A.x ~> G; in

            let B.z := [A] A.x ~> B; in
	    let C.z := [A] A.x ~> C; in
	    let D.z := [A] A.x ~> D; in
	    let E.z := [A] A.x ~> E; in
	    let F.z := [A] A.x ~> F; in
	    let G.z := [A] A.x ~> G; in

            let B.a := [A] A.x ~> B; in
	    let C.a := [A] A.x ~> C; in
	    let D.a := [A] A.x ~> D; in
	    let E.a := [A] A.x ~> E; in
	    let F.a := [A] A.x ~> F; in
	    let G.a := [A] A.x ~> G; in

            let B.b := [A] A.x ~> B; in
	    let C.b := [A] A.x ~> C; in
	    let D.b := [A] A.x ~> D; in
	    let E.b := [A] A.x ~> E; in
	    let F.b := [A] A.x ~> F; in
	    let G.b := [A] A.x ~> G; in G.();

main := send_serial;
