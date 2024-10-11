{-- This program sends a constant value from L1 to L2 --}
{-- The optimized version would have the value 3 in node L1's end point projection eliminating the need to send over the value --}

L1.x := [L2] L2.3 ~> L1;
