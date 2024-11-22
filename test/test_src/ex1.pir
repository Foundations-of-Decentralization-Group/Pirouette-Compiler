main := let R.x := [S] S.3 ~> R; in 
  if R.(x>5) 
  then R[L] ~> S;
       S.print_endline S."Hello"
  else R[R] ~> S;
       S.print_endline S."Bye"
;

{-
NetIR:
  S:
  2+3
  send 3 to R
  Allow R choice
  | L => print_endline "Hello"
  | R => print_endline "Bye"

  R:
  let x = receive from R in 
  if x > 5 
  then choose L for S
  else choose R for S
-}
