module Evt = Event

let () =
  let _channel_person1_person2 = Evt.new_channel () in
  let _channel_person2_person1 = Evt.new_channel () in
  let person1 () =
    let amt = 5 in
    let _ = Evt.sync (Evt.send _channel_person1_person2 amt) in
    let ___synclbl = Evt.sync (Evt.receive _channel_person2_person1) in
    if ___synclbl then print_endline (string_of_int 1) else print_endline (string_of_int 0)
  in

  let person2 () =
    let _ = () in
    let d = Evt.sync (Evt.receive _channel_person1_person2) in
    if d < 10 then
      let _ = Evt.sync (Evt.send _channel_person2_person1 true) in
      let _ = () in
      ()
    else
      let _ = Evt.sync (Evt.send _channel_person2_person1 false) in
      let _ = () in
      ()
  in
  let person1 = Thread.create person1 () in
  let person2 = Thread.create person2 () in
  Thread.join person1;
  Thread.join person2
