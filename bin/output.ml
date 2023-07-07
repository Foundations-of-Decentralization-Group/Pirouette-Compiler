module Evt = Event

let () =
  let _channel_p1_p2 = Evt.new_channel () in
  let _channel_p2_p1 = Evt.new_channel () in
  let p2 () =
    let _ = () in
    let y : int = 10 in
    let ___synclbl : bool = Evt.sync (Evt.receive _channel_p1_p2) in
    if ___synclbl then
      let _ = Evt.sync (Evt.send _channel_p2_p1 (y + 2)) in
      ()
    else
      let _ = Evt.sync (Evt.send _channel_p2_p1 (y - 2)) in
      ()
  in

  let p1 () =
    let x : int = 5 in
    let _ = () in
    if x > 2 then
      if x > 10 then
        let _ = Evt.sync (Evt.send _channel_p1_p2 true) in
        let _ =
          let z : int = Evt.sync (Evt.receive _channel_p2_p1) in
          print_endline (string_of_int z)
        in
        ()
      else
        let _ = Evt.sync (Evt.send _channel_p1_p2 false) in
        let _ =
          let z : int = Evt.sync (Evt.receive _channel_p2_p1) in
          print_endline (string_of_int z)
        in
        ()
    else if x > 10 then
      let _ = Evt.sync (Evt.send _channel_p1_p2 true) in
      let _ =
        let z : int = Evt.sync (Evt.receive _channel_p2_p1) in
        print_endline (string_of_int z)
      in
      ()
    else
      let _ = Evt.sync (Evt.send _channel_p1_p2 false) in
      let _ =
        let z : int = Evt.sync (Evt.receive _channel_p2_p1) in
        print_endline (string_of_int z)
      in
      ()
  in
  let _p1 = Thread.create p1 () in
  let _p2 = Thread.create p2 () in
  Thread.join _p1;
  Thread.join _p2
