foreign gettimeofday : unit -> unit := "Unix:gettimeofday";
foreign print_float : unit -> unit := "Stdlib:print_float";
foreign sub_float : unit -> unit -> unit := "Stdlib:(-.)";
foreign get_mod : unit -> unit -> unit := "Stdlib:mod";
foreign get_equal : unit -> unit -> unit := "Stdlib:(==)";
foreign print_int : unit -> unit := "Stdlib:print_int";

assent_A :=

     let A.x := A.10; in
     let Coordinator.msg_ask := Coordinator.2; in
     
     let A.msg_ask := [Coordinator] Coordinator.msg_ask ~> A; in
     let A.msg_ask_intermed_result := A.get_mod A.x A.msg_ask; in

     if A.(msg_ask_intermed_result = 0) then
     
       A[L] ~> Coordinator;
      
       let A.msg_ask_res := A.1; in
       A.msg_ask_res

     else

       A[R] ~> Coordinator;
     
       let A.msg_ask_res := A.0; in
       A.msg_ask_res;

assent_B :=

     let B.x := B.10; in
     let Coordinator.msg_ask := Coordinator.2; in
     
     let B.msg_ask := [Coordinator] Coordinator.msg_ask ~> B; in
     let B.msg_ask_intermed_result := B.get_mod B.x B.msg_ask; in

     if B.(msg_ask_intermed_result = 0) then
     
       B[L] ~> Coordinator;
      
       let B.msg_ask_res := B.1; in
       B.msg_ask_res

     else

       B[R] ~> Coordinator;
     
       let B.msg_ask_res := B.0; in
       B.msg_ask_res;

commit_A :=

       let A.commit_val := A.5; in
       A.commit_val;

commit_B :=

       let B.commit_val := B.5; in
       B.commit_val;

main :=

    let A.assent_reply_A := A.assent_A; in
    let B.assent_reply_B := B.assent_B; in

    let Coordinator.assent_reply_A := [A] A.assent_reply_A ~> Coordinator; in 
    let Coordinator.assent_reply_B := [B] B.assent_reply_B ~> Coordinator; in
    let Coordinator.assent_consensus := Coordinator.(assent_reply_A * assent_reply_B); in
    
    if Coordinator.(assent_consensus = 1) then
    
       Coordinator[COMMIT] ~> A;
       Coordinator[COMMIT] ~> B;

       let A.commit_val := A.commit_A; in
       let B.commit_val := B.commit_B; in 
       let Coordinator.commit_reply_A := [A] A.commit_val ~> Coordinator; in
       let Coordinator.commit_reply_B := [B] B.commit_val ~> Coordinator; in
       Coordinator.()
       
    else
    
       Coordinator[ABORT] ~> A;
       Coordinator[ABORT] ~> B;
       
       let A.commit_val := A.(); in
       let B.commit_val := B.(); in 
       let Coordinator.commit_reply_A := [A] A.commit_val ~> Coordinator; in
       let Coordinator.commit_reply_B := [B] B.commit_val ~> Coordinator; in      
       Coordinator.();