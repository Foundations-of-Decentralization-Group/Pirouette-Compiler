foreign gettimeofday : unit -> unit := "Unix:gettimeofday";
foreign print_float : unit -> unit := "Stdlib:print_float";
foreign sub_float : unit -> unit -> unit := "Stdlib:(-.)";
foreign get_mod : unit -> unit -> unit := "Stdlib:mod";

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

assent_C :=

     let C.x := C.10; in
     let Coordinator.msg_ask := Coordinator.2; in
     
     let C.msg_ask := [Coordinator] Coordinator.msg_ask ~> C; in
     let C.msg_ask_intermed_result := C.get_mod C.x C.msg_ask; in

     if C.(msg_ask_intermed_result = 0) then
     
       C[L] ~> Coordinator;
      
       let C.msg_ask_res := C.1; in
       C.msg_ask_res

     else

       C[R] ~> Coordinator;
     
       let C.msg_ask_res := C.0; in
       C.msg_ask_res;

assent_D :=

     let D.x := D.10; in
     let Coordinator.msg_ask := Coordinator.2; in
     
     let D.msg_ask := [Coordinator] Coordinator.msg_ask ~> D; in
     let D.msg_ask_intermed_result := D.get_mod D.x D.msg_ask; in

     if D.(msg_ask_intermed_result = 0) then
     
       D[L] ~> Coordinator;
      
       let D.msg_ask_res := D.1; in
       D.msg_ask_res

     else

       D[R] ~> Coordinator;
     
       let D.msg_ask_res := D.0; in
       D.msg_ask_res;

assent_E :=

     let E.x := E.10; in
     let Coordinator.msg_ask := Coordinator.2; in
     
     let E.msg_ask := [Coordinator] Coordinator.msg_ask ~> E; in
     let E.msg_ask_intermed_result := E.get_mod E.x E.msg_ask; in

     if E.(msg_ask_intermed_result = 0) then
     
       E[L] ~> Coordinator;
      
       let E.msg_ask_res := E.1; in
       E.msg_ask_res

     else

       E[R] ~> Coordinator;
     
       let E.msg_ask_res := E.0; in
       E.msg_ask_res;

assent_F :=

     let F.x := F.10; in
     let Coordinator.msg_ask := Coordinator.2; in
     
     let F.msg_ask := [Coordinator] Coordinator.msg_ask ~> F; in
     let F.msg_ask_intermed_result := F.get_mod F.x F.msg_ask; in

     if F.(msg_ask_intermed_result = 0) then
     
       F[L] ~> Coordinator;
      
       let F.msg_ask_res := F.1; in
       F.msg_ask_res

     else

       F[R] ~> Coordinator;
     
       let F.msg_ask_res := F.0; in
       F.msg_ask_res;

commit_A :=

       let A.commit_val := A.5; in
       A.commit_val;

commit_B :=

       let B.commit_val := B.5; in
       B.commit_val;

commit_C :=

       let C.commit_val := C.5; in
       C.commit_val;
       
commit_D :=

       let D.commit_val := D.5; in
       D.commit_val;

commit_E :=

       let E.commit_val := E.5; in
       E.commit_val;

commit_F :=

       let F.commit_val := F.5; in
       F.commit_val;

abort_A :=

       let A.abort_val := A.(); in
       A.abort_val;

abort_B :=

       let B.abort_val := B.(); in
       B.abort_val;

abort_C :=

       let C.abort_val := C.(); in
       C.abort_val;
       
abort_D :=

       let D.abort_val := D.(); in
       D.abort_val;

abort_E :=

       let E.abort_val := E.(); in
       E.abort_val;

abort_F :=

       let F.abort_val := F.(); in
       F.abort_val;

two_phase_commit :=

    let A.assent_reply_A := A.assent_A; in
    let B.assent_reply_B := B.assent_B; in
    let C.assent_reply_C := C.assent_C; in    
    let D.assent_reply_D := D.assent_D; in
    let E.assent_reply_E := E.assent_E; in
    let F.assent_reply_F := F.assent_F; in    
    
    let Coordinator.assent_reply_A := [A] A.assent_reply_A ~> Coordinator; in 
    let Coordinator.assent_reply_B := [B] B.assent_reply_B ~> Coordinator; in
    let Coordinator.assent_reply_C := [C] C.assent_reply_C ~> Coordinator; in
    let Coordinator.assent_reply_D := [D] D.assent_reply_D ~> Coordinator; in
    let Coordinator.assent_reply_E := [E] E.assent_reply_E ~> Coordinator; in
    let Coordinator.assent_reply_F := [F] F.assent_reply_F ~> Coordinator; in
    
    let Coordinator.assent_consensus := Coordinator.(assent_reply_A * assent_reply_B * assent_reply_C * assent_reply_D * assent_reply_E * assent_reply_F); in
    
    if Coordinator.(assent_consensus = 1) then
    
       Coordinator[COMMIT] ~> A;
       Coordinator[COMMIT] ~> B;
       Coordinator[COMMIT] ~> C;
       Coordinator[COMMIT] ~> D;
       Coordinator[COMMIT] ~> E;
       Coordinator[COMMIT] ~> F;

       let A.commit_val := A.commit_A; in
       let B.commit_val := B.commit_B; in
       let C.commit_val := C.commit_C; in
       let D.commit_val := D.commit_D; in
       let E.commit_val := E.commit_E; in
       let F.commit_val := F.commit_F; in
       
       let Coordinator.commit_reply_A := [A] A.commit_val ~> Coordinator; in
       let Coordinator.commit_reply_B := [B] B.commit_val ~> Coordinator; in
       let Coordinator.commit_reply_C := [C] C.commit_val ~> Coordinator; in
       let Coordinator.commit_reply_D := [D] D.commit_val ~> Coordinator; in
       let Coordinator.commit_reply_E := [E] E.commit_val ~> Coordinator; in       
       let Coordinator.commit_reply_F := [F] F.commit_val ~> Coordinator; in       
       Coordinator.()
       
    else
    
       Coordinator[ABORT] ~> A;
       Coordinator[ABORT] ~> B;
       Coordinator[ABORT] ~> C;
       Coordinator[ABORT] ~> D;
       Coordinator[ABORT] ~> E;
       Coordinator[ABORT] ~> F;
       
       let A.abort_val := A.abort_A; in
       let B.abort_val := B.abort_B; in
       let C.abort_val := C.abort_C; in
       let D.abort_val := D.abort_D; in
       let E.abort_val := E.abort_E; in
       let F.abort_val := F.abort_F; in
       
       let Coordinator.abort_reply_A := [A] A.abort_val ~> Coordinator; in
       let Coordinator.abort_reply_B := [B] B.abort_val ~> Coordinator; in
       let Coordinator.abort_reply_C := [C] C.abort_val ~> Coordinator; in
       let Coordinator.abort_reply_D := [D] D.abort_val ~> Coordinator; in
       let Coordinator.abort_reply_E := [E] E.abort_val ~> Coordinator; in
       let Coordinator.abort_reply_F := [F] F.abort_val ~> Coordinator; in                    
       Coordinator.();


main :=
    let Coordinator.start_time := Coordinator.gettimeofday Coordinator.(); in
    let Coordinator._ := two_phase_commit; in
    let Coordinator._ := two_phase_commit; in
    let Coordinator._ := two_phase_commit; in
    let Coordinator._ := two_phase_commit; in
    let Coordinator._ := two_phase_commit; in
    let Coordinator._ := two_phase_commit; in     
    let Coordinator.end_time := Coordinator.gettimeofday Coordinator.(); in
    let Coordinator.time_diff := Coordinator.sub_float Coordinator.end_time Coordinator.start_time; in
    Coordinator.print_float Coordinator.time_diff;