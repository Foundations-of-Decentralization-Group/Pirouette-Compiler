send_para :=

	    let A.x := A.5; in
	    let B.x := [A] A.x ~> B; in
	    let C.x := [A] A.x ~> C; in
	    let D.x := [B] B.x ~> D; in
	    let E.x := [B] B.x ~> E; in
	    let F.x := [C] C.x ~> F; in
	    let G.x := [C] C.x ~> G; in G.();

main := send_para;
