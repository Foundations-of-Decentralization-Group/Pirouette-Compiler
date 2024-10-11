y := P.let y : int := 3 in
match (x,z) with
|(true,_)->(1,"None");


{-
NetIR:
  S:
  2+3
  send 3 to R
  Allow R choice
  | L => "Hello"
  | R => "Bye"

  R:
  let x = receive from R in 
  if x > 5 
  then choose L for S
  else choose R for S
-}