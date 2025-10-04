let loc_to_rank =
  function
  | "A" -> 0
  | "B" -> 1
  | "C" -> 2
  | "D" -> 3
  | "E" -> 4
  | "F" -> 5
  | "G" -> 6
  | "H" -> 7
  | "I" -> 8
  | "J" -> 9
  | "K" -> 10
  | "L" -> 11
  | "M" -> 12
  | "N" -> 13
  | "O" -> 14
  | _ -> failwith "Runtime Error: Unknown location"
let _ = Mpi.barrier Mpi.comm_world
let _ =
  match Mpi.comm_rank Mpi.comm_world with
  | 0 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        if freq > 0
        then
          (Mpi.send "L" (loc_to_rank "B") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "C") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "D") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "E") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "F") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "G") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "H") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "I") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "J") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "K") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "L") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "M") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "N") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "O") 0 Mpi.comm_world;
           (let rec result_B =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "B") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_C =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "C") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_D =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "D") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_E =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "E") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_F =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "F") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_G =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "G") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_H =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "H") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_I =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "I") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_J =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "J") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_K =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "K") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_L =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "L") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_M =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "M") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_N =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "N") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_O =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "O") Mpi.any_tag Mpi.comm_world) 0 in
            broadcast_unopt (freq - 1)))
        else
          (Mpi.send "R" (loc_to_rank "B") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "C") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "D") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "E") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "F") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "G") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "H") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "I") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "J") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "K") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "L") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "M") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "N") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "O") 0 Mpi.comm_world;
           (let rec result_B =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "B") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_C =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "C") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_D =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "D") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_E =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "E") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_F =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "F") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_G =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "G") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_H =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "H") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_I =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "I") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_J =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "J") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_K =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "K") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_L =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "L") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_M =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "M") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_N =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "N") Mpi.any_tag Mpi.comm_world) 0 in
            let rec result_O =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "O") Mpi.any_tag Mpi.comm_world) 0 in
            ())) in
      let rec start_time = gettimeofday () in
      let rec _unit_1 = broadcast_unopt 10000 in
      let rec end_time = gettimeofday () in
      let rec time_diff = (sub_float end_time) start_time in
      print_float time_diff
  | 1 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_3 =
              let val_2 = x in
              Mpi.send (Marshal.to_string val_2 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_5 =
              let val_4 = x in
              Mpi.send (Marshal.to_string val_4 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_6 = broadcast_unopt () in ()
  | 2 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec x = 9 in
            let rec _unit_8 =
              let val_7 = x in
              Mpi.send (Marshal.to_string val_7 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec x = 10 in
            let rec _unit_10 =
              let val_9 = x in
              Mpi.send (Marshal.to_string val_9 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_11 = broadcast_unopt () in ()
  | 3 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec x = 9 in
            let rec _unit_13 =
              let val_12 = x in
              Mpi.send (Marshal.to_string val_12 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec x = 10 in
            let rec _unit_15 =
              let val_14 = x in
              Mpi.send (Marshal.to_string val_14 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_16 = broadcast_unopt () in ()
  | 4 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_18 =
              let val_17 = x in
              Mpi.send (Marshal.to_string val_17 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_20 =
              let val_19 = x in
              Mpi.send (Marshal.to_string val_19 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_21 = broadcast_unopt () in ()
  | 5 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_23 =
              let val_22 = x in
              Mpi.send (Marshal.to_string val_22 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_25 =
              let val_24 = x in
              Mpi.send (Marshal.to_string val_24 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_26 = broadcast_unopt () in ()
  | 6 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_28 =
              let val_27 = x in
              Mpi.send (Marshal.to_string val_27 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_30 =
              let val_29 = x in
              Mpi.send (Marshal.to_string val_29 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_31 = broadcast_unopt () in ()
  | 7 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_33 =
              let val_32 = x in
              Mpi.send (Marshal.to_string val_32 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_35 =
              let val_34 = x in
              Mpi.send (Marshal.to_string val_34 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_36 = broadcast_unopt () in ()
  | 8 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_38 =
              let val_37 = x in
              Mpi.send (Marshal.to_string val_37 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_40 =
              let val_39 = x in
              Mpi.send (Marshal.to_string val_39 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_41 = broadcast_unopt () in ()
  | 9 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec x = 9 in
            let rec _unit_43 =
              let val_42 = x in
              Mpi.send (Marshal.to_string val_42 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec x = 10 in
            let rec _unit_45 =
              let val_44 = x in
              Mpi.send (Marshal.to_string val_44 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_46 = broadcast_unopt () in ()
  | 10 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec x = 9 in
            let rec _unit_48 =
              let val_47 = x in
              Mpi.send (Marshal.to_string val_47 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec x = 10 in
            let rec _unit_50 =
              let val_49 = x in
              Mpi.send (Marshal.to_string val_49 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_51 = broadcast_unopt () in ()
  | 11 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec x = 9 in
            let rec _unit_53 =
              let val_52 = x in
              Mpi.send (Marshal.to_string val_52 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec x = 10 in
            let rec _unit_55 =
              let val_54 = x in
              Mpi.send (Marshal.to_string val_54 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_56 = broadcast_unopt () in ()
  | 12 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_58 =
              let val_57 = x in
              Mpi.send (Marshal.to_string val_57 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_60 =
              let val_59 = x in
              Mpi.send (Marshal.to_string val_59 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_61 = broadcast_unopt () in ()
  | 13 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "R" ->
            let rec x = 9 in
            let rec _unit_63 =
              let val_62 = x in
              Mpi.send (Marshal.to_string val_62 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | "L" ->
            let rec x = 10 in
            let rec _unit_65 =
              let val_64 = x in
              Mpi.send (Marshal.to_string val_64 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_66 = broadcast_unopt () in ()
  | 14 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec broadcast_unopt freq =
        match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec x = 10 in
            let rec _unit_68 =
              let val_67 = x in
              Mpi.send (Marshal.to_string val_67 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            broadcast_unopt ()
        | "R" ->
            let rec x = 9 in
            let rec _unit_70 =
              let val_69 = x in
              Mpi.send (Marshal.to_string val_69 []) (loc_to_rank "A") 0
                Mpi.comm_world in
            ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_71 = broadcast_unopt () in ()
  | _ -> failwith "Runtime Error: Unknown rank"
