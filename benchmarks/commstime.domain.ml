(* let chan_A_B = Domainslib.Chan.make_bounded 0 *)
(* let chan_A_C = Domainslib.Chan.make_bounded 0 *)
(* let chan_A_D = Domainslib.Chan.make_bounded 0 *)
(* let chan_A_E = Domainslib.Chan.make_bounded 0 *)
(* let chan_B_A = Domainslib.Chan.make_bounded 0 *)
(* let chan_B_C = Domainslib.Chan.make_bounded 0 *)
(* let chan_B_D = Domainslib.Chan.make_bounded 0 *)
(* let chan_B_E = Domainslib.Chan.make_bounded 0 *)
(* let chan_C_A = Domainslib.Chan.make_bounded 0 *)
(* let chan_C_B = Domainslib.Chan.make_bounded 0 *)
(* let chan_C_D = Domainslib.Chan.make_bounded 0 *)
(* let chan_C_E = Domainslib.Chan.make_bounded 0 *)
(* let chan_D_A = Domainslib.Chan.make_bounded 0 *)
(* let chan_D_B = Domainslib.Chan.make_bounded 0 *)
(* let chan_D_C = Domainslib.Chan.make_bounded 0 *)
(* let chan_D_E = Domainslib.Chan.make_bounded 0 *)
(* let chan_E_A = Domainslib.Chan.make_bounded 0 *)
(* let chan_E_B = Domainslib.Chan.make_bounded 0 *)
(* let chan_E_C = Domainslib.Chan.make_bounded 0 *)
(* let chan_E_D = Domainslib.Chan.make_bounded 0 *)

let chan_A_B = Domainslib.Chan.make_bounded 0
let chan_B_C = Domainslib.Chan.make_bounded 0
let chan_B_D = Domainslib.Chan.make_bounded 0
let chan_B_E = Domainslib.Chan.make_bounded 0
let chan_C_A = Domainslib.Chan.make_bounded 0
let chan_E_A = Domainslib.Chan.make_bounded 0
let chan_E_B = Domainslib.Chan.make_bounded 0
let chan_E_C = Domainslib.Chan.make_bounded 0
let chan_E_D = Domainslib.Chan.make_bounded 0;;

let domain_A =
  Domain.spawn (fun _ ->
    let rec _unit_3 =
      match Domainslib.Chan.recv chan_E_A with
      | "R" -> ()
      | "L" ->
        let rec successor = Marshal.from_string (Domainslib.Chan.recv chan_C_A) 0 in
        let rec _unit_2 =
          let val_1 = successor in
          Domainslib.Chan.send chan_A_B (Marshal.to_string val_1 [])
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    ())
and domain_B =
  Domain.spawn (fun _ ->
    let rec _unit_10 =
      match Domainslib.Chan.recv chan_E_B with
      | "L" ->
        let rec init_num = Marshal.from_string (Domainslib.Chan.recv chan_E_B) 0 in
        let rec _unit_9 =
          let val_8 = init_num in
          Domainslib.Chan.send chan_B_C (Marshal.to_string val_8 [])
        in
        let rec _unit_7 =
          let val_6 = init_num in
          Domainslib.Chan.send chan_B_D (Marshal.to_string val_6 [])
        in
        let rec init_num = Marshal.from_string (Domainslib.Chan.recv chan_A_B) 0 in
        let rec _unit_5 =
          let val_4 = init_num in
          Domainslib.Chan.send chan_B_E (Marshal.to_string val_4 [])
        in
        ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    ())
and domain_C =
  Domain.spawn (fun _ ->
    let rec _unit_13 =
      match Domainslib.Chan.recv chan_E_C with
      | "R" -> ()
      | "L" ->
        let rec delta = Marshal.from_string (Domainslib.Chan.recv chan_B_C) 0 in
        let rec successor = delta + 1 in
        let rec _unit_12 =
          let val_11 = successor in
          Domainslib.Chan.send chan_C_A (Marshal.to_string val_11 [])
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    ())
and domain_D =
  Domain.spawn (fun _ ->
    let rec _unit_14 =
      match Domainslib.Chan.recv chan_E_D with
      | "R" -> ()
      | "L" ->
        let rec delta = Marshal.from_string (Domainslib.Chan.recv chan_B_D) 0 in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    ())
and domain_E =
  Domain.spawn (fun _ ->
    let rec commstime iterations init_num =
      let () = print_string "In here zero" in
      if iterations > 0
      then (
        let () = print_string "In here one" in
        Domainslib.Chan.send chan_E_A "L";
        Domainslib.Chan.send chan_E_B "L";
        Domainslib.Chan.send chan_E_C "L";
        Domainslib.Chan.send chan_E_D "L";
        let rec _unit_16 =
          let val_15 = init_num in
          Domainslib.Chan.send chan_E_B (Marshal.to_string val_15 [])
        in
        let rec new_iterations = iterations - 1 in
        let () = print_string "In here two" in
        let rec new_init_num = Marshal.from_string (Domainslib.Chan.recv chan_B_E) 0 in
        (commstime new_iterations) new_init_num)
      else (
        Domainslib.Chan.send chan_E_A "R";
        Domainslib.Chan.send chan_E_B "R";
        Domainslib.Chan.send chan_E_C "R";
        Domainslib.Chan.send chan_E_D "R";
        print_string "Communications Time completed successfully")
    in
    (commstime 10) 0)
in
Domain.join domain_A;
Domain.join domain_B;
Domain.join domain_C;
Domain.join domain_D;
Domain.join domain_E
