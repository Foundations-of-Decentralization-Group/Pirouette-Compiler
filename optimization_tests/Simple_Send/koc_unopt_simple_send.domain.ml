let (chan_A_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_A_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_A_D : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_A_E : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_A_F : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_A_G : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_D : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_E : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_F : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_G : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_D : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_E : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_F : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_G : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_D_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_D_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_D_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_D_E : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_D_F : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_D_G : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_E_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_E_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_E_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_E_D : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_E_F : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_E_G : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_F_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_F_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_F_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_F_D : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_F_E : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_F_G : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_G_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_G_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_G_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_G_D : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_G_E : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_G_F : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
;;let domain_A =
    Domain.spawn
      (fun _ ->
         let rec broadcast_opt freq =
           if freq > 0
           then
             (Domainslib.Chan.send chan_A_B "L";
              Domainslib.Chan.send chan_A_D "L";
              Domainslib.Chan.send chan_A_E "L";
              Domainslib.Chan.send chan_A_F "L";
              Domainslib.Chan.send chan_A_G "L";
              Domainslib.Chan.send chan_A_C "L";
              broadcast_opt (freq - 1))
           else
             (Domainslib.Chan.send chan_A_B "R";
              Domainslib.Chan.send chan_A_C "R";
              Domainslib.Chan.send chan_A_D "R";
              Domainslib.Chan.send chan_A_E "R";
              Domainslib.Chan.send chan_A_F "R";
              Domainslib.Chan.send chan_A_G "R";
              print_endline "Terminate - Unoptimized") in
         broadcast_opt 1000)
  and domain_B =
    Domain.spawn
      (fun _ ->
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_B with
           | "L" -> broadcast_opt ()
           | "R" -> let rec x = 9 in ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_C =
    Domain.spawn
      (fun _ ->
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_C with
           | "L" -> let rec x = 10 in broadcast_opt ()
           | "R" -> let rec x = 9 in ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_D =
    Domain.spawn
      (fun _ ->
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_D with
           | "L" -> let rec x = 10 in broadcast_opt ()
           | "R" -> let rec x = 9 in ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_E =
    Domain.spawn
      (fun _ ->
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_E with
           | "R" -> let rec x = 9 in ()
           | "L" -> let rec x = 10 in broadcast_opt ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_F =
    Domain.spawn
      (fun _ ->
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_F with
           | "L" -> let rec x = 10 in broadcast_opt ()
           | "R" -> let rec x = 9 in ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_G =
    Domain.spawn
      (fun _ ->
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_G with
           | "R" -> let rec x = 9 in ()
           | "L" -> let rec x = 10 in broadcast_opt ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ()) in
  Domain.join domain_A;
  Domain.join domain_B;
  Domain.join domain_C;
  Domain.join domain_D;
  Domain.join domain_E;
  Domain.join domain_F;
  Domain.join domain_G
