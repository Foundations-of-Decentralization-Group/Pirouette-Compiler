broadcast_unopt freq :=

        let B.x := B."10"; in
	let A.result_B1 := [B] B.x ~> A; in 
        let C.x := C."11"; in
	let A.result_C1 := [C] C.x ~> A; in
        let B.x := B."12"; in
	let A.result_B2 := [B] B.x ~> A; in 
        let C.x := C."13"; in
	let A.result_C2 := [C] C.x ~> A; in
        let B.x := B."14"; in
	let A.result_B3 := [B] B.x ~> A; in 
        let C.x := C."15"; in
	let A.result_C3 := [C] C.x ~> A; in
        let B.x := B."16"; in
	let A.result_B4 := [B] B.x ~> A; in 
        let C.x := C."17"; in
	let A.result_C4 := [C] C.x ~> A; in
        let B.x := B."18"; in
	let A.result_B5 := [B] B.x ~> A; in 
        let C.x := C."19"; in
	let A.result_C5 := [C] C.x ~> A; in
        let B.x := B."20"; in
	let A.result_B6 := [B] B.x ~> A; in 
        let C.x := C."21"; in
	let A.result_C6 := [C] C.x ~> A; in
        let B.x := B."22"; in
	let A.result_B7 := [B] B.x ~> A; in 
        let C.x := C."23"; in
	let A.result_C7 := [C] C.x ~> A; in
	let B.x := B."24"; in
	let A.result_B8 := [B] B.x ~> A; in 
        let C.x := C."25"; in
	let A.result_C8 := [C] C.x ~> A; in
        let B.x := B."26"; in
	let A.result_B9 := [B] B.x ~> A; in 
        let C.x := C."27"; in
	let A.result_C9 := [C] C.x ~> A; in
 	if A.(freq > 0) then
	A[L] ~> B;
        A[L] ~> C;    
        let B.x := B."10"; in
	let A.result_B := [B] B.x ~> A; in 
        let C.x := C."10"; in
	let A.result_C := [C] C.x ~> A; in 
        A.print_endline A."Terminate - Unoptimized"
	
	else 
        A[R] ~> B;
        A[R] ~> C;

        let B.x := B."9"; in
	let A.result_B := [B] B.x ~> A; in 
        let C.x := C."9"; in
	let A.result_C := [C] C.x ~> A; in
        A.print_endline A."Terminate - Unoptimized";

main := broadcast_unopt A.5;