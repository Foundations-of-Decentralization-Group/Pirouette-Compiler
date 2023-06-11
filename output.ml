open Utils

let () =
  let amt_due = 5 in
  let func_0 X_0 =
    let _ = send_message amt_due 8082 in
    let initpay d =
      let ___server_sock = open_port 8082 in
      let amt_due = receive_message ___server_sock in
      t
    in
    let ___disreg = initpay d in
    ()
  in
  let ___disreg = func_0 () in
  ()
