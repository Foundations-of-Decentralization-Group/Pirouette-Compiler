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
         let rec test_collatz inp =
           if inp = 1
           then 0
           else
             (let rec res1 = inp / 2 in
              let rec res2 = res1 * 2 in
              if res2 = inp
              then let rec part_res = test_collatz res1 in part_res + 1
              else
                (let rec res3 = (3 * inp) + 1 in
                 let rec res4 = test_collatz res3 in 1 + res4)) in
         let rec _unit_1 = () in
         let rec _unit_2 = () in
         let rec _unit_3 = () in
         let rec _unit_4 = () in
         let rec _unit_5 = () in
         let rec _unit_6 = () in
         let rec broadcast_opt freq =
           if freq > 0
           then
             (Domainslib.Chan.send chan_A_B "L1";
              Domainslib.Chan.send chan_A_C "L2";
              (let rec reply_B =
                 Marshal.from_string (Domainslib.Chan.recv chan_B_A) 0 in
               let rec reply_C =
                 Marshal.from_string (Domainslib.Chan.recv chan_C_A) 0 in
               let rec reply_D =
                 Marshal.from_string (Domainslib.Chan.recv chan_D_A) 0 in
               let rec reply_E =
                 Marshal.from_string (Domainslib.Chan.recv chan_E_A) 0 in
               let rec reply_F =
                 Marshal.from_string (Domainslib.Chan.recv chan_F_A) 0 in
               let rec reply_G =
                 Marshal.from_string (Domainslib.Chan.recv chan_G_A) 0 in
               broadcast_opt (freq - 1)))
           else
             (Domainslib.Chan.send chan_A_B "R1";
              Domainslib.Chan.send chan_A_C "R2";
              print_endline "Done with all the computations - Optimized") in
         broadcast_opt 1000)
  and domain_B =
    Domain.spawn
      (fun _ ->
         let rec _unit_7 = () in
         let rec test_collatz inp =
           if inp = 1
           then 0
           else
             (let rec res1 = inp / 2 in
              let rec res2 = res1 * 2 in
              if res2 = inp
              then let rec part_res = test_collatz res1 in part_res + 1
              else
                (let rec res3 = (3 * inp) + 1 in
                 let rec res4 = test_collatz res3 in 1 + res4)) in
         let rec _unit_8 = () in
         let rec _unit_9 = () in
         let rec _unit_10 = () in
         let rec _unit_11 = () in
         let rec _unit_12 = () in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_B with
           | "R1" ->
               (Domainslib.Chan.send chan_B_D "R3";
                Domainslib.Chan.send chan_B_E "R4";
                ())
           | "L1" ->
               (Domainslib.Chan.send chan_B_D "L3";
                Domainslib.Chan.send chan_B_E "L4";
                (let rec result = test_collatz 931386509544713451 in
                 let rec _unit_14 =
                   let val_13 = result in
                   Domainslib.Chan.send chan_B_A
                     (Marshal.to_string val_13 []) in
                 broadcast_opt ()))
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_C =
    Domain.spawn
      (fun _ ->
         let rec _unit_15 = () in
         let rec _unit_16 = () in
         let rec test_collatz inp =
           if inp = 1
           then 0
           else
             (let rec res1 = inp / 2 in
              let rec res2 = res1 * 2 in
              if res2 = inp
              then let rec part_res = test_collatz res1 in part_res + 1
              else
                (let rec res3 = (3 * inp) + 1 in
                 let rec res4 = test_collatz res3 in 1 + res4)) in
         let rec _unit_17 = () in
         let rec _unit_18 = () in
         let rec _unit_19 = () in
         let rec _unit_20 = () in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_C with
           | "R2" ->
               (Domainslib.Chan.send chan_C_F "R5";
                Domainslib.Chan.send chan_C_G "R6";
                ())
           | "L2" ->
               (Domainslib.Chan.send chan_C_F "L5";
                Domainslib.Chan.send chan_C_G "L6";
                (let rec result = test_collatz 931386509544713451 in
                 let rec _unit_22 =
                   let val_21 = result in
                   Domainslib.Chan.send chan_C_A
                     (Marshal.to_string val_21 []) in
                 broadcast_opt ()))
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_D =
    Domain.spawn
      (fun _ ->
         let rec _unit_23 = () in
         let rec _unit_24 = () in
         let rec _unit_25 = () in
         let rec test_collatz inp =
           if inp = 1
           then 0
           else
             (let rec res1 = inp / 2 in
              let rec res2 = res1 * 2 in
              if res2 = inp
              then let rec part_res = test_collatz res1 in part_res + 1
              else
                (let rec res3 = (3 * inp) + 1 in
                 let rec res4 = test_collatz res3 in 1 + res4)) in
         let rec _unit_26 = () in
         let rec _unit_27 = () in
         let rec _unit_28 = () in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_B_D with
           | "L3" ->
               let rec result = test_collatz 931386509544713451 in
               let rec _unit_30 =
                 let val_29 = result in
                 Domainslib.Chan.send chan_D_A (Marshal.to_string val_29 []) in
               broadcast_opt ()
           | "R3" -> ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_E =
    Domain.spawn
      (fun _ ->
         let rec _unit_31 = () in
         let rec _unit_32 = () in
         let rec _unit_33 = () in
         let rec _unit_34 = () in
         let rec test_collatz inp =
           if inp = 1
           then 0
           else
             (let rec res1 = inp / 2 in
              let rec res2 = res1 * 2 in
              if res2 = inp
              then let rec part_res = test_collatz res1 in part_res + 1
              else
                (let rec res3 = (3 * inp) + 1 in
                 let rec res4 = test_collatz res3 in 1 + res4)) in
         let rec _unit_35 = () in
         let rec _unit_36 = () in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_B_E with
           | "L4" ->
               let rec result = test_collatz 931386509544713451 in
               let rec _unit_38 =
                 let val_37 = result in
                 Domainslib.Chan.send chan_E_A (Marshal.to_string val_37 []) in
               broadcast_opt ()
           | "R4" -> ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_F =
    Domain.spawn
      (fun _ ->
         let rec _unit_39 = () in
         let rec _unit_40 = () in
         let rec _unit_41 = () in
         let rec _unit_42 = () in
         let rec _unit_43 = () in
         let rec test_collatz inp =
           if inp = 1
           then 0
           else
             (let rec res1 = inp / 2 in
              let rec res2 = res1 * 2 in
              if res2 = inp
              then let rec part_res = test_collatz res1 in part_res + 1
              else
                (let rec res3 = (3 * inp) + 1 in
                 let rec res4 = test_collatz res3 in 1 + res4)) in
         let rec _unit_44 = () in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_C_F with
           | "L5" ->
               let rec result = test_collatz 931386509544713451 in
               let rec _unit_46 =
                 let val_45 = result in
                 Domainslib.Chan.send chan_F_A (Marshal.to_string val_45 []) in
               broadcast_opt ()
           | "R5" -> ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ())
  and domain_G =
    Domain.spawn
      (fun _ ->
         let rec _unit_47 = () in
         let rec _unit_48 = () in
         let rec _unit_49 = () in
         let rec _unit_50 = () in
         let rec _unit_51 = () in
         let rec _unit_52 = () in
         let rec test_collatz inp =
           if inp = 1
           then 0
           else
             (let rec res1 = inp / 2 in
              let rec res2 = res1 * 2 in
              if res2 = inp
              then let rec part_res = test_collatz res1 in part_res + 1
              else
                (let rec res3 = (3 * inp) + 1 in
                 let rec res4 = test_collatz res3 in 1 + res4)) in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_C_G with
           | "R6" -> ()
           | "L6" ->
               let rec result = test_collatz 931386509544713451 in
               let rec _unit_54 =
                 let val_53 = result in
                 Domainslib.Chan.send chan_G_A (Marshal.to_string val_53 []) in
               broadcast_opt ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         broadcast_opt ()) in
  Domain.join domain_A;
  Domain.join domain_B;
  Domain.join domain_C;
  Domain.join domain_D;
  Domain.join domain_E;
  Domain.join domain_F;
  Domain.join domain_G
