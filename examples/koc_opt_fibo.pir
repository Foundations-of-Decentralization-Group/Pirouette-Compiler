A.fib n :=
  if A.(n = 0) then A.0 else
    if A.(n = 1) then A.1 else
      let A.n1 := A.fib A.(n - 1); in
      let A.n2 := A.fib A.(n - 2); in
      A.(n1 + n2);
B.fib n :=
  if B.(n = 0) then B.0 else
    if B.(n = 1) then B.1 else
      let B.n1 := B.fib B.(n - 1); in
      let B.n2 := B.fib B.(n - 2); in
      B.(n1 + n2);
C.fib n :=
  if C.(n = 0) then C.0 else
    if C.(n = 1) then C.1 else
      let C.n1 := C.fib C.(n - 1); in
      let C.n2 := C.fib C.(n - 2); in
      C.(n1 + n2);
D.fib n :=
  if D.(n = 0) then D.0 else
    if D.(n = 1) then D.1 else
      let D.n1 := D.fib D.(n - 1); in
      let D.n2 := D.fib D.(n - 2); in
      D.(n1 + n2);
E.fib n :=
  if E.(n = 0) then E.0 else
    if E.(n = 1) then E.1 else
      let E.n1 := E.fib E.(n - 1); in
      let E.n2 := E.fib E.(n - 2); in
      E.(n1 + n2);
F.fib n :=
  if F.(n = 0) then F.0 else
    if F.(n = 1) then F.1 else
      let F.n1 := F.fib F.(n - 1); in
      let F.n2 := F.fib F.(n - 2); in
      F.(n1 + n2);
G.fib n :=
  if G.(n = 0) then G.0 else
    if G.(n = 1) then G.1 else
      let G.n1 := G.fib G.(n - 1); in
      let G.n2 := G.fib G.(n - 2); in
      G.(n1 + n2);

broadcast_opt freq :=

    if A.(freq > 0) then
    A[L1] ~> B;
    A[L2] ~> C;
    B[L3] ~> D;
    B[L4] ~> E;
    C[L5] ~> F;
    C[L6] ~> G;

        let B.result  := B.fib B.40; in 
        let A.reply_B := [B] B.result ~> A; in

        let C.result  := C.fib C.40; in
        let A.reply_C := [C] C.result ~> A; in 

        let D.result  := D.fib D.40; in
        let A.reply_D := [D] D.result ~> A; in 

        let E.result  := E.fib E.40; in 
        let A.reply_E := [E] E.result ~> A; in 

        let F.result  := F.fib F.40; in 
        let A.reply_F := [F] F.result ~> A; in 

        let G.result  := G.fib G.40; in 
        let A.reply_G := [G] G.result ~> A; in 

        broadcast_opt A.(freq - 1)


    else 
    A[R1] ~> B;
    A[R2] ~> C;
    B[R3] ~> D;
    B[R4] ~> E;
    C[R5] ~> F;
    C[R6] ~> G;

        A.print_endline A."Done with all the computations";


main := broadcast_opt A.150;