{-- This program keeps getting a certain value from L2 within a loop and doing a computation within L1; this could be potentially placed outside the loop ; we'd have to make sure that this valuedoes not change to be placed outside the loop; one time dynamic value assignment --}


y : L2.int;
q : L1.unit;

Looper : L1.unit -> L1.unit;

L1.x := [L2] L2.y ~> L1;

Looper := let L1.q := L1.(2 * x); in
	  Looper;
