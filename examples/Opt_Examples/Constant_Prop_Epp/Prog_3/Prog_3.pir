{-- This program computes the sum of two numbers at location L1, stores the result at L1 and then sends over the result to a location L2; The optimized version could have constant propagation along with constant folding. This would eliminate the overhead of sending the values over to the other node --}

c : L1.int;
d : L2.int;

Sum L1.a L1.b := L1.(a + b);
c := Sum L1.5 L1.8;

L2.d := [L1] L1.c ~> L2;
