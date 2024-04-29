_ := if R.(3+5 < 2-1)
then R[L] ~> S;
  	 R."Sent"
else R[R] ~> S;
  	 R."why"
;

{-
  NetIR:
  S: 
  Allow R choice
  | L => send (1,true) to R
  | R => send (0,false) to R

  R:
  if 3+5 > 2-1
  then choose L for S
    "Sent"
    receive from S
  else choose R for S
    "why"
    receive from S
-}