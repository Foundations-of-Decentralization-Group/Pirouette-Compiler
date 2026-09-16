let (chan_A_B : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
let (chan_B_A : string Domainslib.Chan.t) = Domainslib.Chan.make_bounded 0
;;let domain_A =
    Domain.spawn
      (fun _ ->
         let rec x = 5 in
         let rec y = 6 in
         let rec _unit_2 =
           let val_1 = x in
           Domainslib.Chan.send chan_A_B (Marshal.to_string val_1 []) in
         ())
  and domain_B =
    Domain.spawn
      (fun _ ->
         let rec x = Marshal.from_string (Domainslib.Chan.recv chan_A_B) 0 in
         let rec y = y in ()) in
  Domain.join domain_A; Domain.join domain_B
