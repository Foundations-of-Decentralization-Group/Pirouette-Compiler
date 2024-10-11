{-- The goal of this program is to show keep a received value cached instead of requesting for it again and again from the other node; note - constant propagation might not work here because the values received could be set by some initial conditions in the other nodes. This is the working assumption for this example; This is an optimization where the programmer writes inefficient code? --}

y : L2.int;
w : L1.int;
v : L1.int;

L1.y := [L2] L2.y ~> L1;

Sum L1.x := L1.(x + y);
Diff L1.x := L1.(x - y);

w := Sum L1.5;
v := Diff L1.6;
