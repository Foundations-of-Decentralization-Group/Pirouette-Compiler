module Evt = Event

let () =
  let _channel_person1_person2 = Evt.new_channel () in
  let _channel_person2_person1 = Evt.new_channel () in
  let person1 () =
    let _ = Evt.sync (Evt.send _channel_person1_person2 5) in
    ()
  in

  let person2 () =
    let d = Evt.sync (Evt.receive _channel_person1_person2) in
    d + 3
  in
  let person1 = Thread.create person1 () in
  let person2 = Thread.create person2 () in
  Thread.join person1;
  Thread.join person2
