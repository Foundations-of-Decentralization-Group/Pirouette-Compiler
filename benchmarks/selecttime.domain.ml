let chan_A_B = Domainslib.Chan.make_bounded 0
let chan_A_C = Domainslib.Chan.make_bounded 0
let chan_A_D = Domainslib.Chan.make_bounded 0
let chan_A_E = Domainslib.Chan.make_bounded 0
let chan_B_A = Domainslib.Chan.make_bounded 0
let chan_B_C = Domainslib.Chan.make_bounded 0
let chan_B_D = Domainslib.Chan.make_bounded 0
let chan_B_E = Domainslib.Chan.make_bounded 0
let chan_C_A = Domainslib.Chan.make_bounded 0
let chan_C_B = Domainslib.Chan.make_bounded 0
let chan_C_D = Domainslib.Chan.make_bounded 0
let chan_C_E = Domainslib.Chan.make_bounded 0
let chan_D_A = Domainslib.Chan.make_bounded 0
let chan_D_B = Domainslib.Chan.make_bounded 0
let chan_D_C = Domainslib.Chan.make_bounded 0
let chan_D_E = Domainslib.Chan.make_bounded 0
let chan_E_A = Domainslib.Chan.make_bounded 0
let chan_E_B = Domainslib.Chan.make_bounded 0
let chan_E_C = Domainslib.Chan.make_bounded 0
let chan_E_D = Domainslib.Chan.make_bounded 0
;;let domain_A =
    Domain.spawn
      (fun _ ->
         let rec _unit_1 = () in
         let rec experiment iterations =
           match Domainslib.Chan.recv chan_E_A with
           | "R5" -> ()
           | "L5" ->
               (match Domainslib.Chan.recv chan_E_A with
                | "R1" ->
                    (match Domainslib.Chan.recv chan_E_A with
                     | "L2" -> experiment ()
                     | "R2" ->
                         (match Domainslib.Chan.recv chan_E_A with
                          | "L3" -> experiment ()
                          | "R3" ->
                              (match Domainslib.Chan.recv chan_E_A with
                               | "L4" -> experiment ()
                               | "R4" -> ()
                               | _ ->
                                   failwith "Runtime Error: Unmatched label")
                          | _ -> failwith "Runtime Error: Unmatched label")
                     | _ -> failwith "Runtime Error: Unmatched label")
                | "L1" ->
                    let rec result =
                      Marshal.from_string (Domainslib.Chan.recv chan_E_A) 0 in
                    experiment ()
                | _ -> failwith "Runtime Error: Unmatched label")
           | _ -> failwith "Runtime Error: Unmatched label" in
         experiment ())
  and domain_B =
    Domain.spawn
      (fun _ ->
         let rec _unit_2 = () in
         let rec experiment iterations =
           match Domainslib.Chan.recv chan_E_B with
           | "L5" ->
               (match Domainslib.Chan.recv chan_E_B with
                | "L1" -> experiment ()
                | "R1" ->
                    (match Domainslib.Chan.recv chan_E_B with
                     | "L2" ->
                         let rec result =
                           Marshal.from_string
                             (Domainslib.Chan.recv chan_E_B) 0 in
                         experiment ()
                     | "R2" ->
                         (match Domainslib.Chan.recv chan_E_B with
                          | "R3" ->
                              (match Domainslib.Chan.recv chan_E_B with
                               | "R4" -> ()
                               | "L4" -> experiment ()
                               | _ ->
                                   failwith "Runtime Error: Unmatched label")
                          | "L3" -> experiment ()
                          | _ -> failwith "Runtime Error: Unmatched label")
                     | _ -> failwith "Runtime Error: Unmatched label")
                | _ -> failwith "Runtime Error: Unmatched label")
           | "R5" -> ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         experiment ())
  and domain_C =
    Domain.spawn
      (fun _ ->
         let rec _unit_3 = () in
         let rec experiment iterations =
           match Domainslib.Chan.recv chan_E_C with
           | "L5" ->
               (match Domainslib.Chan.recv chan_E_C with
                | "L1" -> experiment ()
                | "R1" ->
                    (match Domainslib.Chan.recv chan_E_C with
                     | "R2" ->
                         (match Domainslib.Chan.recv chan_E_C with
                          | "L3" ->
                              let rec result =
                                Marshal.from_string
                                  (Domainslib.Chan.recv chan_E_C) 0 in
                              experiment ()
                          | "R3" ->
                              (match Domainslib.Chan.recv chan_E_C with
                               | "L4" -> experiment ()
                               | "R4" -> ()
                               | _ ->
                                   failwith "Runtime Error: Unmatched label")
                          | _ -> failwith "Runtime Error: Unmatched label")
                     | "L2" -> experiment ()
                     | _ -> failwith "Runtime Error: Unmatched label")
                | _ -> failwith "Runtime Error: Unmatched label")
           | "R5" -> ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         experiment ())
  and domain_D =
    Domain.spawn
      (fun _ ->
         let rec _unit_4 = () in
         let rec experiment iterations =
           match Domainslib.Chan.recv chan_E_D with
           | "L5" ->
               (match Domainslib.Chan.recv chan_E_D with
                | "R1" ->
                    (match Domainslib.Chan.recv chan_E_D with
                     | "L2" -> experiment ()
                     | "R2" ->
                         (match Domainslib.Chan.recv chan_E_D with
                          | "L3" -> experiment ()
                          | "R3" ->
                              (match Domainslib.Chan.recv chan_E_D with
                               | "R4" -> ()
                               | "L4" ->
                                   let rec result =
                                     Marshal.from_string
                                       (Domainslib.Chan.recv chan_E_D) 0 in
                                   experiment ()
                               | _ ->
                                   failwith "Runtime Error: Unmatched label")
                          | _ -> failwith "Runtime Error: Unmatched label")
                     | _ -> failwith "Runtime Error: Unmatched label")
                | "L1" -> experiment ()
                | _ -> failwith "Runtime Error: Unmatched label")
           | "R5" -> ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         experiment ())
  and domain_E =
    Domain.spawn
      (fun _ ->
         let rec get_length num_one = num_one in
         let rec experiment iterations =
           if iterations > 0
           then
             (Domainslib.Chan.send chan_E_A "L5";
              Domainslib.Chan.send chan_E_B "L5";
              Domainslib.Chan.send chan_E_C "L5";
              Domainslib.Chan.send chan_E_D "L5";
              (let rec rand_number = 100 in
               let rec modulo_result = get_length 4 in
               if modulo_result = 0
               then
                 (Domainslib.Chan.send chan_E_A "L1";
                  Domainslib.Chan.send chan_E_B "L1";
                  Domainslib.Chan.send chan_E_C "L1";
                  Domainslib.Chan.send chan_E_D "L1";
                  (let rec _unit_12 =
                     let val_11 = modulo_result in
                     Domainslib.Chan.send chan_E_A
                       (Marshal.to_string val_11 []) in
                   experiment (iterations - 1)))
               else
                 (Domainslib.Chan.send chan_E_A "R1";
                  Domainslib.Chan.send chan_E_B "R1";
                  Domainslib.Chan.send chan_E_C "R1";
                  Domainslib.Chan.send chan_E_D "R1";
                  if modulo_result = 1
                  then
                    (Domainslib.Chan.send chan_E_A "L2";
                     Domainslib.Chan.send chan_E_B "L2";
                     Domainslib.Chan.send chan_E_C "L2";
                     Domainslib.Chan.send chan_E_D "L2";
                     (let rec _unit_10 =
                        let val_9 = modulo_result in
                        Domainslib.Chan.send chan_E_B
                          (Marshal.to_string val_9 []) in
                      experiment (iterations - 1)))
                  else
                    (Domainslib.Chan.send chan_E_A "R2";
                     Domainslib.Chan.send chan_E_B "R2";
                     Domainslib.Chan.send chan_E_C "R2";
                     Domainslib.Chan.send chan_E_D "R2";
                     if modulo_result = 2
                     then
                       (Domainslib.Chan.send chan_E_A "L3";
                        Domainslib.Chan.send chan_E_B "L3";
                        Domainslib.Chan.send chan_E_C "L3";
                        Domainslib.Chan.send chan_E_D "L3";
                        (let rec _unit_8 =
                           let val_7 = modulo_result in
                           Domainslib.Chan.send chan_E_C
                             (Marshal.to_string val_7 []) in
                         experiment (iterations - 1)))
                     else
                       (Domainslib.Chan.send chan_E_A "R3";
                        Domainslib.Chan.send chan_E_B "R3";
                        Domainslib.Chan.send chan_E_C "R3";
                        Domainslib.Chan.send chan_E_D "R3";
                        if modulo_result = 3
                        then
                          (Domainslib.Chan.send chan_E_A "L4";
                           Domainslib.Chan.send chan_E_B "L4";
                           Domainslib.Chan.send chan_E_C "L4";
                           Domainslib.Chan.send chan_E_D "L4";
                           (let rec _unit_6 =
                              let val_5 = modulo_result in
                              Domainslib.Chan.send chan_E_D
                                (Marshal.to_string val_5 []) in
                            experiment (iterations - 1)))
                        else
                          (Domainslib.Chan.send chan_E_A "R4";
                           Domainslib.Chan.send chan_E_B "R4";
                           Domainslib.Chan.send chan_E_C "R4";
                           Domainslib.Chan.send chan_E_D "R4";
                           print_string "Invalid Choice\n"))))))
           else
             (Domainslib.Chan.send chan_E_A "R5";
              Domainslib.Chan.send chan_E_B "R5";
              Domainslib.Chan.send chan_E_C "R5";
              Domainslib.Chan.send chan_E_D "R5";
              print_string "Select Time completed successfully") in
         experiment 1) in
  Domain.join domain_A;
  Domain.join domain_B;
  Domain.join domain_C;
  Domain.join domain_D;
  Domain.join domain_E
