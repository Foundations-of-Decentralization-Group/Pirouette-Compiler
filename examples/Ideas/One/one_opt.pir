{- The main ideas here is to partially evaluate computations as soon as possible before for any receives that maybe part of the computation; this may require splitting up the computations. If they are expressed as functions then possibly rearranging the function as well;applies to dynamic data -}

main :=
 let A.y := A.10; in let A.z := A.12; in
 let A.partial_sum := A.(y + z); in 
 let B.x := B.11; in let A.x := [B] B.x ~> A; in
 let A.sum := A.(x + partial_sum); in A.();