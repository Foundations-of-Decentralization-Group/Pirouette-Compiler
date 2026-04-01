let loc_to_rank = function
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
;;

let _ = Mpi.barrier Mpi.comm_world

let _ =
  match Mpi.comm_rank Mpi.comm_world with
  | 0 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let x1 = Marshal.to_string 10 [] in
    let x2 = Marshal.to_string 9 [] in
    let rec loop freq =
      if freq > 0
      then (
        Mpi.send "L" (loc_to_rank "B") 0 Mpi.comm_world;
        Mpi.send "L" (loc_to_rank "C") 0 Mpi.comm_world;
        let result_arr = Mpi.gather "x" (loc_to_rank "A") Mpi.comm_world in
        (* print_endline ("Result array size is" ^ Int.to_string (Array.length result_arr)); *)
        (* print_endline "Got the result array"; *)
        let val_to_print : string = Array.get result_arr 1 in
        (* print_endline *)
        (*   ("This is the required value: " *)
        (*    ^ Int.to_string (Marshal.from_string val_to_print 0)); *)
        loop (freq - 1))
      else (
        Mpi.send "R" (loc_to_rank "B") 0 Mpi.comm_world;
        Mpi.send "R" (loc_to_rank "C") 0 Mpi.comm_world;
        (* print_endline "In here"; *)
        let result_arr_y = Mpi.gather "y" (loc_to_rank "A") Mpi.comm_world in ()
        (* print_endline ("Result array size is" ^ Int.to_string (Array.length result_arr_y)); *)
        (* print_endline "Got the result array"; *)
        (* let val_to_print : string = Array.get result_arr_y 1 in *)
        (* print_endline *)
        (*   ("This is the required value: " *)
        (*    ^ Int.to_string (Marshal.from_string val_to_print 0)); *)
        (* print_endline "Terminate - Optimized"  *)
      )
    in
    let rec start_time = gettimeofday () in
    let rec _unit_1 = loop 1000 in
    let rec end_time = gettimeofday () in
    let rec time_diff = (sub_float end_time) start_time in
    print_float time_diff
  | 1 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        Mpi.send "L" (loc_to_rank "D") 0 Mpi.comm_world;
        Mpi.send "L" (loc_to_rank "E") 0 Mpi.comm_world;
        let rec x = 10 in
        let rec _unit_3 =
          let val_2 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_2 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        Mpi.send "R" (loc_to_rank "D") 0 Mpi.comm_world;
        Mpi.send "R" (loc_to_rank "E") 0 Mpi.comm_world;
        let rec y = 10 in
        let rec _unit_5 =
          let val_4 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world          
          (* Mpi.send (Marshal.to_string val_4 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_6 = loop () in
    ()
  | 2 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "A") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        Mpi.send "L" (loc_to_rank "F") 0 Mpi.comm_world;
        Mpi.send "L" (loc_to_rank "G") 0 Mpi.comm_world;
        let rec x = 10 in
        let rec _unit_8 =
          let val_7 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_7 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        Mpi.send "R" (loc_to_rank "F") 0 Mpi.comm_world;
        Mpi.send "R" (loc_to_rank "G") 0 Mpi.comm_world;
        let rec y = 10 in
        let rec _unit_10 =
          let val_9 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                    
          (* Mpi.send (Marshal.to_string val_9 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_11 = loop () in
    ()
  | 3 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "B") Mpi.any_tag Mpi.comm_world with
      | "R" ->
        Mpi.send "R" (loc_to_rank "H") 0 Mpi.comm_world;
        Mpi.send "R" (loc_to_rank "I") 0 Mpi.comm_world;
        let rec y = 10 in
        let rec _unit_13 =
          let val_12 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
          (* Mpi.send (Marshal.to_string val_12 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | "L" ->
        Mpi.send "L" (loc_to_rank "H") 0 Mpi.comm_world;
        Mpi.send "L" (loc_to_rank "I") 0 Mpi.comm_world;
        let rec x = 10 in
        let rec _unit_15 =
          let val_14 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_14 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_16 = loop () in
    ()
  | 4 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "B") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        Mpi.send "L" (loc_to_rank "J") 0 Mpi.comm_world;
        Mpi.send "L" (loc_to_rank "K") 0 Mpi.comm_world;
        let rec x = 10 in
        let rec _unit_18 =
          let val_17 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_17 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        Mpi.send "R" (loc_to_rank "J") 0 Mpi.comm_world;
        Mpi.send "R" (loc_to_rank "K") 0 Mpi.comm_world;
        let rec y = 10 in
        let rec _unit_20 =
          let val_19 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
          (* Mpi.send (Marshal.to_string val_19 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_21 = loop () in
    ()
  | 5 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "C") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        Mpi.send "L" (loc_to_rank "L") 0 Mpi.comm_world;
        Mpi.send "L" (loc_to_rank "M") 0 Mpi.comm_world;
        let rec x = 10 in
        let rec _unit_23 =
          let val_22 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_22 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        Mpi.send "R" (loc_to_rank "L") 0 Mpi.comm_world;
        Mpi.send "R" (loc_to_rank "M") 0 Mpi.comm_world;
        let rec y = 10 in
        let rec _unit_25 =
          let val_24 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
          (* Mpi.send (Marshal.to_string val_24 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_26 = loop () in
    ()
  | 6 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "C") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        Mpi.send "L" (loc_to_rank "N") 0 Mpi.comm_world;
        Mpi.send "L" (loc_to_rank "O") 0 Mpi.comm_world;
        let rec x = 10 in
        let rec _unit_28 =
          let val_27 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_27 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        Mpi.send "R" (loc_to_rank "N") 0 Mpi.comm_world;
        Mpi.send "R" (loc_to_rank "O") 0 Mpi.comm_world;
        let rec y = 10 in
        let rec _unit_30 =
          let val_29 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
          (* Mpi.send (Marshal.to_string val_29 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_31 = loop () in
    ()
  | 7 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "D") Mpi.any_tag Mpi.comm_world with
      | "R" ->
        let rec y = 10 in
        let rec _unit_33 =
          let val_32 = y in
          (* Mpi.send (Marshal.to_string val_32 []) (loc_to_rank "A") 0 Mpi.comm_world *)
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
        in
        ()
      | "L" ->
        let rec x = 10 in
        let rec _unit_35 =
          let val_34 = x in
          (* Mpi.send (Marshal.to_string val_34 []) (loc_to_rank "A") 0 Mpi.comm_world *)
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_36 = loop () in
    ()
  | 8 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "D") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec x = 10 in
        let rec _unit_38 =
          let val_37 = x in
          (* Mpi.send (Marshal.to_string val_37 []) (loc_to_rank "A") 0 Mpi.comm_world *)
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
        in
        loop ()
      | "R" ->
        let rec y = 10 in
        let rec _unit_40 =
          let val_39 = y in
          (* Mpi.send (Marshal.to_string val_39 []) (loc_to_rank "A") 0 Mpi.comm_world *)
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_41 = loop () in
    ()
  | 9 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "E") Mpi.any_tag Mpi.comm_world with
      | "R" ->
        let rec y = 10 in
        let rec _unit_43 =
          let val_42 = y in
          (* Mpi.send (Marshal.to_string val_42 []) (loc_to_rank "A") 0 Mpi.comm_world *)
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
        in
        ()
      | "L" ->
        let rec x = 10 in
        let rec _unit_45 =
          let val_44 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_44 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_46 = loop () in
    ()
  | 10 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "E") Mpi.any_tag Mpi.comm_world with
      | "R" ->
        let rec y = 10 in
        let rec _unit_48 =
          let val_47 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
          (* Mpi.send (Marshal.to_string val_47 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | "L" ->
        let rec x = 10 in
        let rec _unit_50 =
          let val_49 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_49 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_51 = loop () in
    ()
  | 11 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "F") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec x = 10 in
        let rec _unit_53 =
          let val_52 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_52 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        let rec y = 10 in
        let rec _unit_55 =
          let val_54 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
          (* Mpi.send (Marshal.to_string val_54 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_56 = loop () in
    ()
  | 12 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "F") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec x = 10 in
        let rec _unit_58 =
          let val_57 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_57 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        let rec y = 10 in
        let rec _unit_60 =
          let val_59 = y in
          (* Mpi.send (Marshal.to_string val_59 []) (loc_to_rank "A") 0 Mpi.comm_world *)
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_61 = loop () in
    ()
  | 13 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "G") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec x = 10 in
        let rec _unit_63 =
          let val_62 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_62 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        let rec y = 10 in
        let rec _unit_65 =
          let val_64 = y in
          (* Mpi.send (Marshal.to_string val_64 []) (loc_to_rank "A") 0 Mpi.comm_world *)
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_66 = loop () in
    ()
  | 14 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop freq =
      match Mpi.receive (loc_to_rank "G") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec x = 10 in
        let rec _unit_68 =
          let val_67 = x in
          Mpi.gather (Marshal.to_string x []) (loc_to_rank "A") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_67 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" ->
        let rec y = 10 in
        let rec _unit_70 =
          let val_69 = y in
          Mpi.gather (Marshal.to_string y []) (loc_to_rank "A") Mpi.comm_world                              
          (* Mpi.send (Marshal.to_string val_69 []) (loc_to_rank "A") 0 Mpi.comm_world *)
        in
        ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_71 = loop () in
    ()
  | _ -> failwith "Runtime Error: Unknown rank"
;;
