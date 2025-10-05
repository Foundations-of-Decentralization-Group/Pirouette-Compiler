let (chan_A_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_A_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_C : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_C_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
;;let domain_A =
    Domain.spawn
      (fun _ ->
         let rec gettimeofday arg = Unix.gettimeofday arg in
         let rec print_float arg = Stdlib.print_float arg in
         let rec sub_float arg = Stdlib.(-.) arg in
         let rec broadcast_opt freq =
           if freq > 0
           then
             (Domainslib.Chan.send chan_A_B "L";
              Domainslib.Chan.send chan_A_C "L";
              (let rec result_B =
                 Marshal.from_string (Domainslib.Chan.recv chan_B_A) 0 in
               let rec result_C =
                 Marshal.from_string (Domainslib.Chan.recv chan_C_A) 0 in
               broadcast_opt (freq - 1)))
           else
             (Domainslib.Chan.send chan_A_B "R";
              Domainslib.Chan.send chan_A_C "R";
              (let rec result_B =
                 Marshal.from_string (Domainslib.Chan.recv chan_B_A) 0 in
               let rec result_C =
                 Marshal.from_string (Domainslib.Chan.recv chan_C_A) 0 in
               ())) in
         let rec start_time = gettimeofday () in
         let rec _unit_1 = broadcast_opt 10 in
         let rec end_time = gettimeofday () in
         let rec time_diff = (sub_float end_time) start_time in
         print_float time_diff)
  and domain_B =
    Domain.spawn
      (fun _ ->
         let rec gettimeofday arg = Unix.gettimeofday arg in
         let rec print_float arg = Stdlib.print_float arg in
         let rec sub_float arg = Stdlib.(-.) arg in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_B with
           | "L" ->
               let rec x = "10" in
               let rec _unit_3 =
                 let val_2 = x in
                 Domainslib.Chan.send chan_B_A (Marshal.to_string val_2 []) in
               broadcast_opt ()
           | "R" ->
               let rec x = "9" in
               let rec _unit_5 =
                 let val_4 = x in
                 Domainslib.Chan.send chan_B_A (Marshal.to_string val_4 []) in
               ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         let rec _unit_6 = broadcast_opt () in ())
  and domain_C =
    Domain.spawn
      (fun _ ->
         let rec gettimeofday arg = Unix.gettimeofday arg in
         let rec print_float arg = Stdlib.print_float arg in
         let rec sub_float arg = Stdlib.(-.) arg in
         let rec broadcast_opt freq =
           match Domainslib.Chan.recv chan_A_C with
           | "L" ->
               let rec x = "10" in
               let rec _unit_8 =
                 let val_7 = x in
                 Domainslib.Chan.send chan_C_A (Marshal.to_string val_7 []) in
               broadcast_opt ()
           | "R" ->
               let rec x = "9" in
               let rec _unit_10 =
                 let val_9 = x in
                 Domainslib.Chan.send chan_C_A (Marshal.to_string val_9 []) in
               ()
           | _ -> failwith "Runtime Error: Unmatched label" in
         let rec _unit_11 = broadcast_opt () in ()) in
  Domain.join domain_A; Domain.join domain_B; Domain.join domain_C
