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
           Mpi.send "L" (loc_to_rank "P4") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P5") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P6") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P7") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P8") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P9") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P10") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P11") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P12") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P13") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P14") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P15") 0 Mpi.comm_world;
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
           Mpi.send "R" (loc_to_rank "P4") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P5") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P6") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P7") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P8") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P9") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P10") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P11") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P12") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P13") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P14") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P15") 0 Mpi.comm_world;
           (let rec _unit_1 = () in print_endline "Terminate Unoptimized")) in
      let rec start_time = gettimeofday () in
      let rec _unit_2 = loop 1000 in
      let rec end_time = gettimeofday () in
      let rec time_diff = (sub_float end_time) start_time in
      print_float time_diff
  | 1 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 10 in
            let rec _unit_4 =
              let val_3 = result in
              Mpi.send (Marshal.to_string val_3 []) (loc_to_rank "P5") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_5 = loop () in ()
  | 2 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 11 in
            let rec _unit_7 =
              let val_6 = result in
              Mpi.send (Marshal.to_string val_6 []) (loc_to_rank "P5") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_8 = loop () in ()
  | 3 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 12 in
            let rec _unit_10 =
              let val_9 = result in
              Mpi.send (Marshal.to_string val_9 []) (loc_to_rank "P6") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_11 = loop () in ()
  | 4 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 13 in
            let rec _unit_13 =
              let val_12 = result in
              Mpi.send (Marshal.to_string val_12 []) (loc_to_rank "P6") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_14 = loop () in ()
  | 5 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 14 in
            let rec _unit_16 =
              let val_15 = result in
              Mpi.send (Marshal.to_string val_15 []) (loc_to_rank "P7") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_17 = loop () in ()
  | 6 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 15 in
            let rec _unit_19 =
              let val_18 = result in
              Mpi.send (Marshal.to_string val_18 []) (loc_to_rank "P7") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_20 = loop () in ()
  | 7 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 2 in
            let rec _unit_34 =
              let val_33 = result in
              Mpi.send (Marshal.to_string val_33 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P4 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_32 =
              let val_31 = reply_P4 in
              Mpi.send (Marshal.to_string val_31 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P5 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_30 =
              let val_29 = reply_P5 in
              Mpi.send (Marshal.to_string val_29 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P8 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_28 =
              let val_27 = reply_P8 in
              Mpi.send (Marshal.to_string val_27 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P9 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_26 =
              let val_25 = reply_P9 in
              Mpi.send (Marshal.to_string val_25 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P10 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_24 =
              let val_23 = reply_P10 in
              Mpi.send (Marshal.to_string val_23 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P11 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_22 =
              let val_21 = reply_P11 in
              Mpi.send (Marshal.to_string val_21 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_35 = loop () in ()
  | 8 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 3 in
            let rec _unit_49 =
              let val_48 = result in
              Mpi.send (Marshal.to_string val_48 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P6 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_47 =
              let val_46 = reply_P6 in
              Mpi.send (Marshal.to_string val_46 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P7 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_45 =
              let val_44 = reply_P7 in
              Mpi.send (Marshal.to_string val_44 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P12 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_43 =
              let val_42 = reply_P12 in
              Mpi.send (Marshal.to_string val_42 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P13 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_41 =
              let val_40 = reply_P13 in
              Mpi.send (Marshal.to_string val_40 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P14 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_39 =
              let val_38 = reply_P14 in
              Mpi.send (Marshal.to_string val_38 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            let rec reply_P15 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_37 =
              let val_36 = reply_P15 in
              Mpi.send (Marshal.to_string val_36 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_50 = loop () in ()
  | 9 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 4 in
            let rec _unit_56 =
              let val_55 = result in
              Mpi.send (Marshal.to_string val_55 []) (loc_to_rank "P2") 0
                Mpi.comm_world in
            let rec reply_P8 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P8") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_54 =
              let val_53 = reply_P8 in
              Mpi.send (Marshal.to_string val_53 []) (loc_to_rank "P2") 0
                Mpi.comm_world in
            let rec reply_P9 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P9") Mpi.any_tag Mpi.comm_world) 0 in
            let rec _unit_52 =
              let val_51 = reply_P9 in
              Mpi.send (Marshal.to_string val_51 []) (loc_to_rank "P2") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_57 = loop () in ()
  | 10 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 5 in
            let rec _unit_63 =
              let val_62 = result in
              Mpi.send (Marshal.to_string val_62 []) (loc_to_rank "P2") 0
                Mpi.comm_world in
            let rec reply_P10 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P10") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec _unit_61 =
              let val_60 = reply_P10 in
              Mpi.send (Marshal.to_string val_60 []) (loc_to_rank "P2") 0
                Mpi.comm_world in
            let rec reply_P11 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P11") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec _unit_59 =
              let val_58 = reply_P11 in
              Mpi.send (Marshal.to_string val_58 []) (loc_to_rank "P2") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_64 = loop () in ()
  | 11 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 6 in
            let rec _unit_70 =
              let val_69 = result in
              Mpi.send (Marshal.to_string val_69 []) (loc_to_rank "P3") 0
                Mpi.comm_world in
            let rec reply_P12 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P12") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec _unit_68 =
              let val_67 = reply_P12 in
              Mpi.send (Marshal.to_string val_67 []) (loc_to_rank "P3") 0
                Mpi.comm_world in
            let rec reply_P13 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P13") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec _unit_66 =
              let val_65 = reply_P13 in
              Mpi.send (Marshal.to_string val_65 []) (loc_to_rank "P3") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_71 = loop () in ()
  | 12 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 7 in
            let rec _unit_77 =
              let val_76 = result in
              Mpi.send (Marshal.to_string val_76 []) (loc_to_rank "P3") 0
                Mpi.comm_world in
            let rec reply_P14 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P14") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec _unit_75 =
              let val_74 = reply_P14 in
              Mpi.send (Marshal.to_string val_74 []) (loc_to_rank "P3") 0
                Mpi.comm_world in
            let rec reply_P15 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P15") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec _unit_73 =
              let val_72 = reply_P15 in
              Mpi.send (Marshal.to_string val_72 []) (loc_to_rank "P3") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_78 = loop () in ()
  | 13 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 8 in
            let rec _unit_80 =
              let val_79 = result in
              Mpi.send (Marshal.to_string val_79 []) (loc_to_rank "P4") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_81 = loop () in ()
  | 14 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 9 in
            let rec _unit_83 =
              let val_82 = result in
              Mpi.send (Marshal.to_string val_82 []) (loc_to_rank "P4") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_84 = loop () in ()
  | _ -> failwith "Runtime Error: Unknown rank"
