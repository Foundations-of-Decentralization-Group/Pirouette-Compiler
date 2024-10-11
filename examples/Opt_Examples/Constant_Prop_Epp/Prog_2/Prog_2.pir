{-- This program computes a sum at L1 and stores at a location in L2 --}
{-- Question - Would this mean sending over the result to L2 after computation in L1? --}


c : L2.int;
Sum L1.a L1.b := L1.(a + b);
c := Sum L1.5 L1.6;
