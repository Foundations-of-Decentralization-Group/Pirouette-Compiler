let chan_R_S = Domainslib.Chan.make_bounded 0
let chan_S_R = Domainslib.Chan.make_bounded 0

let domain_R =
  Domain.spawn (fun _ ->
    let rec print arg = Printer.print_string_unit arg in
    let rec x =
      match Domainslib.Chan.recv chan_S_R with
      | Ok msg -> msg
      | Error msg ->
        Printf.printf "Receive error in %s: %s\n" "R" msg;
        failwith ("Receive error: " ^ msg)
    in
    if x > 5
    then (
      Domainslib.Chan.send chan_R_S "L";
      ())
    else (
      Domainslib.Chan.send chan_R_S "R";
      ()))
;;

let domain_S =
  Domain.spawn (fun _ ->
    let rec print arg = Printer.print_string_unit arg in
    let rec _unit_2 =
      let val_1 = 3 in
      Domainslib.Chan.send chan_S_R (Ok val_1)
    in
    match Domainslib.Chan.recv chan_R_S with
    | "L" -> print "Hello"
    | "R" -> print "Bye"
    | _ -> failwith "Error: Unmatched label")
;;
