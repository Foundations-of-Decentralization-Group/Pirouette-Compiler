let chan_A_B = Domainslib.Chan.make_bounded 0
let chan_B_C = Domainslib.Chan.make_bounded 0
let chan_B_D = Domainslib.Chan.make_bounded 0
let chan_B_E = Domainslib.Chan.make_bounded 0
let chan_C_A = Domainslib.Chan.make_bounded 0
let chan_E_A = Domainslib.Chan.make_bounded 0
let chan_E_B = Domainslib.Chan.make_bounded 0
let chan_E_C = Domainslib.Chan.make_bounded 0
let chan_E_D = Domainslib.Chan.make_bounded 0

;;let domain_A =
    Domain.spawn
      (fun _ ->
         let rec commstime _ _ =
           match Domainslib.Chan.recv chan_E_A with
           | "R" -> ()
           | "L" ->
               let rec successor =
                 Marshal.from_string (Domainslib.Chan.recv chan_C_A) 0 in
               let rec _unit_2 =
                 let val_1 = successor in
                 Domainslib.Chan.send chan_A_B (Marshal.to_string val_1 []) in
               (commstime ()) ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         (commstime ()) ())
  and domain_B =
    Domain.spawn
      (fun _ ->
         let rec commstime _ _ =
           match Domainslib.Chan.recv chan_E_B with
           | "L" ->
               let rec init_num =
                 Marshal.from_string (Domainslib.Chan.recv chan_E_B) 0 in
               let rec _unit_8 =
                 let val_7 = init_num in
                 Domainslib.Chan.send chan_B_C (Marshal.to_string val_7 []) in
               let rec _unit_6 =
                 let val_5 = init_num in
                 Domainslib.Chan.send chan_B_D (Marshal.to_string val_5 []) in
               let rec init_num =
                 Marshal.from_string (Domainslib.Chan.recv chan_A_B) 0 in
               let rec _unit_4 =
                 let val_3 = init_num in
                 Domainslib.Chan.send chan_B_E (Marshal.to_string val_3 []) in
               (commstime ()) ()
           | "R" -> ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         (commstime ()) ())
  and domain_C =
    Domain.spawn
      (fun _ ->
         let rec commstime _ _ =
           match Domainslib.Chan.recv chan_E_C with
           | "R" -> ()
           | "L" ->
               let rec delta =
                 Marshal.from_string (Domainslib.Chan.recv chan_B_C) 0 in
               let rec successor = delta + 1 in
               let rec _unit_10 =
                 let val_9 = successor in
                 Domainslib.Chan.send chan_C_A (Marshal.to_string val_9 []) in
               (commstime ()) ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         (commstime ()) ())
  and domain_D =
    Domain.spawn
      (fun _ ->
         let rec commstime _ _ =
           match Domainslib.Chan.recv chan_E_D with
           | "R" -> ()
           | "L" ->
               let rec delta =
                 Marshal.from_string (Domainslib.Chan.recv chan_B_D) 0 in
               (commstime ()) ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         (commstime ()) ())
  and domain_E =
    Domain.spawn
      (fun _ ->
         let rec commstime iterations init_num =
           if iterations > 0
           then
             (Domainslib.Chan.send chan_E_A "L";
              Domainslib.Chan.send chan_E_B "L";
              Domainslib.Chan.send chan_E_C "L";
              Domainslib.Chan.send chan_E_D "L";
              (let rec _unit_12 =
                 let val_11 = init_num in
                 Domainslib.Chan.send chan_E_B (Marshal.to_string val_11 []) in
               let rec new_iterations = iterations - 1 in
               let rec new_init_num =
                 Marshal.from_string (Domainslib.Chan.recv chan_B_E) 0 in
               let () = Printf.printf "This is the new value of init %d" new_init_num in 
               (commstime new_iterations) new_init_num))
           else
             (Domainslib.Chan.send chan_E_A "R";
              Domainslib.Chan.send chan_E_B "R";
              Domainslib.Chan.send chan_E_C "R";
              Domainslib.Chan.send chan_E_D "R";
              print_string "Communications Time completed successfully") in
         (commstime 10) 0) in
  Domain.join domain_A;
  Domain.join domain_B;
  Domain.join domain_C;
  Domain.join domain_D;
  Domain.join domain_E
