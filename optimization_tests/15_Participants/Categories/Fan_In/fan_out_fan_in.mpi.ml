let loc_to_rank =
  function
  | "P1" -> 0
  | "P10" -> 1
  | "P11" -> 2
  | "P12" -> 3
  | "P13" -> 4
  | "P14" -> 5
  | "P15" -> 6
  | "P2" -> 7
  | "P3" -> 8
  | "P4" -> 9
  | "P5" -> 10
  | "P6" -> 11
  | "P7" -> 12
  | "P8" -> 13
  | "P9" -> 14
  | _ -> failwith "Runtime Error: Unknown location"
let _ = Mpi.barrier Mpi.comm_world
let _ =
  match Mpi.comm_rank Mpi.comm_world with
  | 0 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        if iter > 0
        then
          (Mpi.send "L" (loc_to_rank "P2") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P3") 0 Mpi.comm_world;
           (let rec reply_P2 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P3 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P4 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P5 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P6 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P7 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P8 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P9 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P10 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P11 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P12 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P13 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P14 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P15 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            loop (iter - 1)))
        else
          (Mpi.send "R" (loc_to_rank "P2") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P3") 0 Mpi.comm_world;
           ()) in
      let rec start_time = gettimeofday () in
      let rec _unit_1 = loop 10000 in
      let rec end_time = gettimeofday () in
      let rec time_diff = (sub_float end_time) start_time in
      print_float time_diff
  | 1 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 10 in
            let rec _unit_3 =
              let val_2 = result in
              Mpi.send (Marshal.to_string val_2 []) (loc_to_rank "P5") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_4 = loop () in ()
  | 2 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 11 in
            let rec _unit_6 =
              let val_5 = result in
              Mpi.send (Marshal.to_string val_5 []) (loc_to_rank "P5") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_7 = loop () in ()
  | 3 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 12 in
            let rec _unit_9 =
              let val_8 = result in
              Mpi.send (Marshal.to_string val_8 []) (loc_to_rank "P6") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_10 = loop () in ()
  | 4 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 13 in
            let rec _unit_12 =
              let val_11 = result in
              Mpi.send (Marshal.to_string val_11 []) (loc_to_rank "P6") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_13 = loop () in ()
  | 5 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 14 in
            let rec _unit_15 =
              let val_14 = result in
              Mpi.send (Marshal.to_string val_14 []) (loc_to_rank "P7") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_16 = loop () in ()
  | 6 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 15 in
            let rec _unit_18 =
              let val_17 = result in
              Mpi.send (Marshal.to_string val_17 []) (loc_to_rank "P7") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_19 = loop () in ()
  | 7 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            (Mpi.send "L" (loc_to_rank "P4") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "P5") 0 Mpi.comm_world;
             (let rec result = 2 in
              let rec _unit_33 =
                let val_32 = result in
                Mpi.send (Marshal.to_string val_32 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P4 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_31 =
                let val_30 = reply_P4 in
                Mpi.send (Marshal.to_string val_30 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P5 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_29 =
                let val_28 = reply_P5 in
                Mpi.send (Marshal.to_string val_28 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P8 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_27 =
                let val_26 = reply_P8 in
                Mpi.send (Marshal.to_string val_26 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P9 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_25 =
                let val_24 = reply_P9 in
                Mpi.send (Marshal.to_string val_24 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P10 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_23 =
                let val_22 = reply_P10 in
                Mpi.send (Marshal.to_string val_22 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P11 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_21 =
                let val_20 = reply_P11 in
                Mpi.send (Marshal.to_string val_20 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              loop ()))
        | "R" ->
            (Mpi.send "R" (loc_to_rank "P4") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "P5") 0 Mpi.comm_world;
             ())
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_34 = loop () in ()
  | 8 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            (Mpi.send "R" (loc_to_rank "P6") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "P7") 0 Mpi.comm_world;
             ())
        | "L" ->
            (Mpi.send "L" (loc_to_rank "P6") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "P7") 0 Mpi.comm_world;
             (let rec result = 3 in
              let rec _unit_48 =
                let val_47 = result in
                Mpi.send (Marshal.to_string val_47 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P6 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_46 =
                let val_45 = reply_P6 in
                Mpi.send (Marshal.to_string val_45 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P7 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_44 =
                let val_43 = reply_P7 in
                Mpi.send (Marshal.to_string val_43 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P12 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_42 =
                let val_41 = reply_P12 in
                Mpi.send (Marshal.to_string val_41 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P13 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_40 =
                let val_39 = reply_P13 in
                Mpi.send (Marshal.to_string val_39 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P14 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_38 =
                let val_37 = reply_P14 in
                Mpi.send (Marshal.to_string val_37 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              let rec reply_P15 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_36 =
                let val_35 = reply_P15 in
                Mpi.send (Marshal.to_string val_35 []) (loc_to_rank "P1") 0
                  Mpi.comm_world in
              loop ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_49 = loop () in ()
  | 9 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            (Mpi.send "L" (loc_to_rank "P8") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "P9") 0 Mpi.comm_world;
             (let rec result = 4 in
              let rec _unit_55 =
                let val_54 = result in
                Mpi.send (Marshal.to_string val_54 []) (loc_to_rank "P2") 0
                  Mpi.comm_world in
              let rec reply_P8 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P8") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_53 =
                let val_52 = reply_P8 in
                Mpi.send (Marshal.to_string val_52 []) (loc_to_rank "P2") 0
                  Mpi.comm_world in
              let rec reply_P9 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P9") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_51 =
                let val_50 = reply_P9 in
                Mpi.send (Marshal.to_string val_50 []) (loc_to_rank "P2") 0
                  Mpi.comm_world in
              loop ()))
        | "R" ->
            (Mpi.send "R" (loc_to_rank "P8") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "P9") 0 Mpi.comm_world;
             ())
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_56 = loop () in ()
  | 10 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            (Mpi.send "R" (loc_to_rank "P10") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "P11") 0 Mpi.comm_world;
             ())
        | "L" ->
            (Mpi.send "L" (loc_to_rank "P10") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "P11") 0 Mpi.comm_world;
             (let rec result = 5 in
              let rec _unit_62 =
                let val_61 = result in
                Mpi.send (Marshal.to_string val_61 []) (loc_to_rank "P2") 0
                  Mpi.comm_world in
              let rec reply_P10 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P10") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_60 =
                let val_59 = reply_P10 in
                Mpi.send (Marshal.to_string val_59 []) (loc_to_rank "P2") 0
                  Mpi.comm_world in
              let rec reply_P11 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P11") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_58 =
                let val_57 = reply_P11 in
                Mpi.send (Marshal.to_string val_57 []) (loc_to_rank "P2") 0
                  Mpi.comm_world in
              loop ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_63 = loop () in ()
  | 11 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            (Mpi.send "L" (loc_to_rank "P12") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "P13") 0 Mpi.comm_world;
             (let rec result = 6 in
              let rec _unit_69 =
                let val_68 = result in
                Mpi.send (Marshal.to_string val_68 []) (loc_to_rank "P3") 0
                  Mpi.comm_world in
              let rec reply_P12 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P12") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_67 =
                let val_66 = reply_P12 in
                Mpi.send (Marshal.to_string val_66 []) (loc_to_rank "P3") 0
                  Mpi.comm_world in
              let rec reply_P13 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P13") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_65 =
                let val_64 = reply_P13 in
                Mpi.send (Marshal.to_string val_64 []) (loc_to_rank "P3") 0
                  Mpi.comm_world in
              loop ()))
        | "R" ->
            (Mpi.send "R" (loc_to_rank "P12") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "P13") 0 Mpi.comm_world;
             ())
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_70 = loop () in ()
  | 12 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            (Mpi.send "R" (loc_to_rank "P14") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "P15") 0 Mpi.comm_world;
             ())
        | "L" ->
            (Mpi.send "L" (loc_to_rank "P14") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "P15") 0 Mpi.comm_world;
             (let rec result = 7 in
              let rec _unit_76 =
                let val_75 = result in
                Mpi.send (Marshal.to_string val_75 []) (loc_to_rank "P3") 0
                  Mpi.comm_world in
              let rec reply_P14 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P14") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_74 =
                let val_73 = reply_P14 in
                Mpi.send (Marshal.to_string val_73 []) (loc_to_rank "P3") 0
                  Mpi.comm_world in
              let rec reply_P15 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "P15") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_72 =
                let val_71 = reply_P15 in
                Mpi.send (Marshal.to_string val_71 []) (loc_to_rank "P3") 0
                  Mpi.comm_world in
              loop ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_77 = loop () in ()
  | 13 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 8 in
            let rec _unit_79 =
              let val_78 = result in
              Mpi.send (Marshal.to_string val_78 []) (loc_to_rank "P4") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_80 = loop () in ()
  | 14 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 9 in
            let rec _unit_82 =
              let val_81 = result in
              Mpi.send (Marshal.to_string val_81 []) (loc_to_rank "P4") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_83 = loop () in ()
  | _ -> failwith "Runtime Error: Unknown rank"
