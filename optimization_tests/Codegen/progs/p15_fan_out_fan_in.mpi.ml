let loc_to_rank =
  function
  | "p1" -> 0
  | "p10" -> 1
  | "p11" -> 2
  | "p12" -> 3
  | "p13" -> 4
  | "p14" -> 5
  | "p15" -> 6
  | "p2" -> 7
  | "p3" -> 8
  | "p4" -> 9
  | "p5" -> 10
  | "p6" -> 11
  | "p7" -> 12
  | "p8" -> 13
  | "p9" -> 14
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
          (Mpi.send "L" (loc_to_rank "p2") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "p3") 0 Mpi.comm_world;
           (let rec res_2 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "p3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec res_1 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "p2") Mpi.any_tag Mpi.comm_world) 0 in
            loop (iter - 1)))
        else
          (Mpi.send "R" (loc_to_rank "p2") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "p3") 0 Mpi.comm_world;
           (let rec res_2 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "p3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec res_1 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "p2") Mpi.any_tag Mpi.comm_world) 0 in
            ())) in
      let rec start_time = gettimeofday () in
      let rec _unit_1 = loop 1000000 in
      let rec end_time = gettimeofday () in
      let rec time_diff = (sub_float end_time) start_time in
      print_float time_diff
  | 1 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p3") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            (Mpi.send "R" (loc_to_rank "p12") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "p13") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p13") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p12") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_3 =
                let val_2 = 1 in
                Mpi.send (Marshal.to_string val_2 []) (loc_to_rank "p3") 0
                  Mpi.comm_world in
              ()))
        | "L" ->
            (Mpi.send "L" (loc_to_rank "p12") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "p13") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p13") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p12") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_5 =
                let val_4 = 1 in
                Mpi.send (Marshal.to_string val_4 []) (loc_to_rank "p3") 0
                  Mpi.comm_world in
              loop ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_6 = loop () in ()
  | 2 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p3") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            (Mpi.send "R" (loc_to_rank "p14") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "p15") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p15") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p14") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_8 =
                let val_7 = 2 in
                Mpi.send (Marshal.to_string val_7 []) (loc_to_rank "p3") 0
                  Mpi.comm_world in
              ()))
        | "L" ->
            (Mpi.send "L" (loc_to_rank "p14") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "p15") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p15") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p14") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_10 =
                let val_9 = 2 in
                Mpi.send (Marshal.to_string val_9 []) (loc_to_rank "p3") 0
                  Mpi.comm_world in
              loop ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_11 = loop () in ()
  | 3 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p10") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec _unit_13 =
              let val_12 = 1 in
              Mpi.send (Marshal.to_string val_12 []) (loc_to_rank "p10") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec _unit_15 =
              let val_14 = 1 in
              Mpi.send (Marshal.to_string val_14 []) (loc_to_rank "p10") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_16 = loop () in ()
  | 4 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p10") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec _unit_18 =
              let val_17 = 2 in
              Mpi.send (Marshal.to_string val_17 []) (loc_to_rank "p10") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec _unit_20 =
              let val_19 = 2 in
              Mpi.send (Marshal.to_string val_19 []) (loc_to_rank "p10") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_21 = loop () in ()
  | 5 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p11") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec _unit_23 =
              let val_22 = 1 in
              Mpi.send (Marshal.to_string val_22 []) (loc_to_rank "p11") 0
                Mpi.comm_world in
            loop ()
        | "R" ->
            let rec _unit_25 =
              let val_24 = 1 in
              Mpi.send (Marshal.to_string val_24 []) (loc_to_rank "p11") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_26 = loop () in ()
  | 6 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p11") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec _unit_28 =
              let val_27 = 2 in
              Mpi.send (Marshal.to_string val_27 []) (loc_to_rank "p11") 0
                Mpi.comm_world in
            loop ()
        | "R" ->
            let rec _unit_30 =
              let val_29 = 2 in
              Mpi.send (Marshal.to_string val_29 []) (loc_to_rank "p11") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_31 = loop () in ()
  | 7 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            (Mpi.send "L" (loc_to_rank "p4") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "p5") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p5") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p4") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_33 =
                let val_32 = 1 in
                Mpi.send (Marshal.to_string val_32 []) (loc_to_rank "p1") 0
                  Mpi.comm_world in
              loop ()))
        | "R" ->
            (Mpi.send "R" (loc_to_rank "p4") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "p5") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p5") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p4") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_35 =
                let val_34 = 1 in
                Mpi.send (Marshal.to_string val_34 []) (loc_to_rank "p1") 0
                  Mpi.comm_world in
              ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_36 = loop () in ()
  | 8 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p1") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            (Mpi.send "R" (loc_to_rank "p10") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "p11") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p11") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p10") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_38 =
                let val_37 = 2 in
                Mpi.send (Marshal.to_string val_37 []) (loc_to_rank "p1") 0
                  Mpi.comm_world in
              ()))
        | "L" ->
            (Mpi.send "L" (loc_to_rank "p10") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "p11") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p11") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p10") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_40 =
                let val_39 = 2 in
                Mpi.send (Marshal.to_string val_39 []) (loc_to_rank "p1") 0
                  Mpi.comm_world in
              loop ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_41 = loop () in ()
  | 9 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p2") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            (Mpi.send "L" (loc_to_rank "p6") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "p7") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p7") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p6") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_43 =
                let val_42 = 1 in
                Mpi.send (Marshal.to_string val_42 []) (loc_to_rank "p2") 0
                  Mpi.comm_world in
              loop ()))
        | "R" ->
            (Mpi.send "R" (loc_to_rank "p6") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "p7") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p7") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p6") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_45 =
                let val_44 = 1 in
                Mpi.send (Marshal.to_string val_44 []) (loc_to_rank "p2") 0
                  Mpi.comm_world in
              ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_46 = loop () in ()
  | 10 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p2") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            (Mpi.send "R" (loc_to_rank "p8") 0 Mpi.comm_world;
             Mpi.send "R" (loc_to_rank "p9") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p9") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p8") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_48 =
                let val_47 = 2 in
                Mpi.send (Marshal.to_string val_47 []) (loc_to_rank "p2") 0
                  Mpi.comm_world in
              ()))
        | "L" ->
            (Mpi.send "L" (loc_to_rank "p8") 0 Mpi.comm_world;
             Mpi.send "L" (loc_to_rank "p9") 0 Mpi.comm_world;
             (let rec res_2 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p9") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec res_1 =
                Marshal.from_string
                  (Mpi.receive (loc_to_rank "p8") Mpi.any_tag Mpi.comm_world)
                  0 in
              let rec _unit_50 =
                let val_49 = 2 in
                Mpi.send (Marshal.to_string val_49 []) (loc_to_rank "p2") 0
                  Mpi.comm_world in
              loop ()))
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_51 = loop () in ()
  | 11 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p4") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec _unit_53 =
              let val_52 = 1 in
              Mpi.send (Marshal.to_string val_52 []) (loc_to_rank "p4") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec _unit_55 =
              let val_54 = 1 in
              Mpi.send (Marshal.to_string val_54 []) (loc_to_rank "p4") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_56 = loop () in ()
  | 12 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p4") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec _unit_58 =
              let val_57 = 2 in
              Mpi.send (Marshal.to_string val_57 []) (loc_to_rank "p4") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec _unit_60 =
              let val_59 = 2 in
              Mpi.send (Marshal.to_string val_59 []) (loc_to_rank "p4") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_61 = loop () in ()
  | 13 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p5") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec _unit_63 =
              let val_62 = 1 in
              Mpi.send (Marshal.to_string val_62 []) (loc_to_rank "p5") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec _unit_65 =
              let val_64 = 1 in
              Mpi.send (Marshal.to_string val_64 []) (loc_to_rank "p5") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_66 = loop () in ()
  | 14 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "p5") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec _unit_68 =
              let val_67 = 2 in
              Mpi.send (Marshal.to_string val_67 []) (loc_to_rank "p5") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec _unit_70 =
              let val_69 = 2 in
              Mpi.send (Marshal.to_string val_69 []) (loc_to_rank "p5") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_71 = loop () in ()
  | _ -> failwith "Runtime Error: Unknown rank"
