let loc_to_rank =
  function
  | "P1" -> 0
  | "P2" -> 1
  | "P3" -> 2
  | "P4" -> 3
  | "P5" -> 4
  | "P6" -> 5
  | "P7" -> 6
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
           (let rec _reduced =
              Mpi.reduce_int 0 Mpi.Int_sum 0 Mpi.comm_world in
            loop (iter - 1)))
        else
          (Mpi.send "R" (loc_to_rank "P2") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P3") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P4") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P5") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P6") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P7") 0 Mpi.comm_world;
           (let rec _unit_1 = () in print_endline "Terminate Optimized")) in
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
            let rec result = 2 in
            let rec _reduced =
              Mpi.reduce_int result Mpi.Int_sum 0 Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_9 = loop () in ()
  | 2 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 3 in
            let rec _reduced =
              Mpi.reduce_int result Mpi.Int_sum 0 Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_16 = loop () in ()
  | 3 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 4 in
            let rec _reduced =
              Mpi.reduce_int result Mpi.Int_sum 0 Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_19 = loop () in ()
  | 4 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = 5 in
            let rec _reduced =
              Mpi.reduce_int result Mpi.Int_sum 0 Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_22 = loop () in ()
  | 5 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 6 in
            let rec _reduced =
              Mpi.reduce_int result Mpi.Int_sum 0 Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_25 = loop () in ()
  | 6 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = 7 in
            let rec _reduced =
              Mpi.reduce_int result Mpi.Int_sum 0 Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_28 = loop () in ()
  | _ -> failwith "Runtime Error: Unknown rank"
