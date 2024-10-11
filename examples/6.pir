
{-- add := let R.x := [Q] Q.3 ~> R; in 
  if R.(x>5)
  then R[L] ~> Q; 
       Q.True
  else R[R] ~> Q;
       Q.False;
--}

{--
_ := let R.x := [S] S.3 ~> R; in 
  if R.(x>5) 
  then R[L] ~> S;
       S."Hello"
  else R[R] ~> S;
       S."Bye";
--}

{-- choreo_expr is x + 5 --}
{-- FUN is fun --}
{-- choreo_pattern is (_) --}

y : R.int; 
--y := R.6; 

--x : R.int;
--x := R.6; 

Double: R.int -> R.int;
Double x := R.x;

R.y := Double R.6;
