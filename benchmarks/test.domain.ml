let chan_E_F = Domainslib.Chan.make_bounded 0

;;let domain_E =
    Domain.spawn
      (fun _ ->
         let rec commstime iterations init_num =
           if iterations > 0
           then
             (Domainslib.Chan.send chan_E_F "L";
              (let rec new_iterations = iterations - 1 in
               let rec new_init_num = init_num + 1 in
               let rec _unit_2 =
                 let val_1 = new_init_num in
                 Domainslib.Chan.send chan_E_F (Marshal.to_string val_1 []) in
               (commstime new_iterations) new_init_num))
           else
             (Domainslib.Chan.send chan_E_F "R";
              print_string
                "Communications Time completed successfully - else") in
         (commstime 10) 0)
  and domain_F =
    Domain.spawn
      (fun _ ->
         let rec commstime _ _ =
           match Domainslib.Chan.recv chan_E_F with
           | "R" -> ()
           | "L" ->
               let rec x =
                 Marshal.from_string (Domainslib.Chan.recv chan_E_F) 0 in
               (commstime ()) ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         (commstime ()) ()) in
  Domain.join domain_E; Domain.join domain_F
