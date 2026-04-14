let loc_to_rank = function
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
;;

let _ = Mpi.barrier Mpi.comm_world

let _ =
  match Mpi.comm_rank Mpi.comm_world with
  | 0 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      if iter > 0
      then (
        Mpi.send "L" (loc_to_rank "P2") 0 Mpi.comm_world;
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
        let result_array = Mpi.gather "result" (loc_to_rank "P1") Mpi.comm_world in
        loop (iter - 1))
      else (
        Mpi.send "R" (loc_to_rank "P2") 0 Mpi.comm_world;
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
        let rec _unit_1 = () in
        print_endline "Terminate Unoptimized")
    in
    let rec start_time = gettimeofday () in
    let rec _unit_2 = loop 2 in
    let rec end_time = gettimeofday () in
    let rec time_diff = (sub_float end_time) start_time in
    print_float time_diff
  | 1 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec result = 10 in
        let rec _unit_4 =
          let val_3 = result in
          Mpi.gather (Marshal.to_string result []) (loc_to_rank "P5") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_3 []) (loc_to_rank "P5") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_5 = loop () in
    ()
  | 2 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "R" -> ()
      | "L" ->
        let rec result = 11 in
        let rec _unit_7 =
          let val_6 = result in
          Mpi.gather (Marshal.to_string result []) (loc_to_rank "P5") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_6 []) (loc_to_rank "P5") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_8 = loop () in
    ()
  | 3 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "R" -> ()
      | "L" ->
        let rec result = 12 in
        let rec _unit_10 =
          let val_9 = result in
          Mpi.gather (Marshal.to_string result []) (loc_to_rank "P6") Mpi.comm_world;
          print_endline "This send is done ; this is for processor P12"
          (* Mpi.send (Marshal.to_string val_9 []) (loc_to_rank "P6") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_11 = loop () in
    ()
  | 4 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec result = 13 in
        let rec _unit_13 =
          let val_12 = result in
          Mpi.gather (Marshal.to_string result []) (loc_to_rank "P6") Mpi.comm_world;
          print_endline "This send is done ; this is for P13"
          (* Mpi.send (Marshal.to_string val_12 []) (loc_to_rank "P6") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_14 = loop () in
    ()
  | 5 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "R" -> ()
      | "L" ->
        print_endline "Got the L from P1 ; this is P14";
        let rec result = 14 in
        let rec _unit_16 =
          let val_15 = result in
          print_endline "Before the gather send to P7";
          Mpi.gather (Marshal.to_string result []) (loc_to_rank "P7") Mpi.comm_world;
          print_endline "This send is done ; this is for P14"
          (* Mpi.send (Marshal.to_string val_15 []) (loc_to_rank "P7") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_17 = loop () in
    ()
  | 6 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec result = 15 in
        let rec _unit_19 =
          let val_18 = result in
          Mpi.gather (Marshal.to_string result []) (loc_to_rank "P7") Mpi.comm_world
          (* print_endline "This send is done ; this is for P15" *)
          (* Mpi.send (Marshal.to_string val_18 []) (loc_to_rank "P7") 0 Mpi.comm_world *)
        in
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_20 = loop () in
    ()
  | 7 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec initial_result = 2 in
        let result_array = Mpi.gather "result" (loc_to_rank "P2") Mpi.comm_world in
        let result_arr_one = Marshal.from_string (Array.get result_array 0) 0 in
        let result_arr_two = Marshal.from_string (Array.get result_array 1) 0 in
        let result =
          int_of_string result_arr_one + int_of_string result_arr_two + initial_result
        in
        Mpi.gather (Marshal.to_string result []) (loc_to_rank "P1") Mpi.comm_world;
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_35 = loop () in
    ()
  | 8 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "R" -> ()
      | "L" ->
        let rec initial_result = 3 in
        let result_arr = Mpi.gather "result" (loc_to_rank "P3") Mpi.comm_world in
        let result_arr_one = Marshal.from_string (Array.get result_arr 0) 0 in
        let result_arr_two = Marshal.from_string (Array.get result_arr 1) 0 in
        let result = result_arr_one + result_arr_two + initial_result in
        Mpi.gather (Marshal.to_string result []) (loc_to_rank "P1") Mpi.comm_world;
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_50 = loop () in
    ()
  | 9 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec initial_result = 4 in
        let result_arr = Mpi.gather "result" (loc_to_rank "P4") Mpi.comm_world in
        let result_arr_one = Marshal.from_string (Array.get result_arr 0) 0 in
        let result_arr_two = Marshal.from_string (Array.get result_arr 1) 0 in
        let result = result_arr_one + result_arr_two + initial_result in
        Mpi.gather (Marshal.to_string result []) (loc_to_rank "P2") Mpi.comm_world;
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_57 = loop () in
    ()
  | 10 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec initial_result = 5 in
        let result_arr = Mpi.gather "result" (loc_to_rank "P5") Mpi.comm_world in
        let result_arr_one = Marshal.from_string (Array.get result_arr 0) 0 in
        let result_arr_two = Marshal.from_string (Array.get result_arr 1) 0 in
        let result = result_arr_one + result_arr_two + initial_result in
        Mpi.gather (Marshal.to_string result []) (loc_to_rank "P2") Mpi.comm_world;
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_64 = loop () in
    ()
  | 11 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        let rec initial_result = 6 in
        let result_arr = Mpi.gather "result" (loc_to_rank "P6") Mpi.comm_world in
        let result_arr_one = Marshal.from_string (Array.get result_arr 0) 0 in
        let result_arr_two = Marshal.from_string (Array.get result_arr 1) 0 in
        let result = result_arr_one + result_arr_two + initial_result in
        Mpi.gather (Marshal.to_string result) (loc_to_rank "P3") Mpi.comm_world;
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_71 = loop () in
    ()
  | 12 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "L" ->
        print_endline "Got the L from P1 ; this is P7";
        let rec initial_result = 7 in
        let result_arr = Mpi.gather "result" (loc_to_rank "P7") Mpi.comm_world in
        print_endline "Got past the recv";
        let result_arr_one = Marshal.from_string (Array.get result_arr 0) 0 in
        let result_arr_two = Marshal.from_string (Array.get result_arr 1) 0 in
        let result = result_arr_one + result_arr_two + initial_result in
        Mpi.gather (Marshal.to_string result) (loc_to_rank "P3") Mpi.comm_world;
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_78 = loop () in
    ()
  | 13 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "R" -> ()
      | "L" ->
        let rec result = 8 in
        let rec _unit_80 =
          let val_79 = result in
          Mpi.gather (Marshal.to_string result) (loc_to_rank "P4") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_79 []) (loc_to_rank "P4") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_81 = loop () in
    ()
  | 14 ->
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
      | "R" -> ()
      | "L" ->
        let rec result = 9 in
        let rec _unit_83 =
          let val_82 = result in
          Mpi.gather (Marshal.to_string result) (loc_to_rank "P4") Mpi.comm_world
          (* Mpi.send (Marshal.to_string val_82 []) (loc_to_rank "P4") 0 Mpi.comm_world *)
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_84 = loop () in
    ()
  | _ -> failwith "Runtime Error: Unknown rank"
;;
