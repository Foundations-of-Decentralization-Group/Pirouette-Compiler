(* print_endline "In here, within the program"; *)
(* Printf.printf "This is the rank %d \n" (Mpi.comm_rank Mpi.comm_world); *)
(* print_endline "Done with the print for all the nodes"; *)
print_endline "Printing this out";
let loc_to_rank = function
  | "P1" -> 0
  | "P2" -> 1
  | "P3" -> 2
  | "P4" -> 3
  | "P5" -> 4
  | "P6" -> 5
  | "P7" -> 6
  | _ -> failwith "Runtime Error: Unknown location"
in
(* let () = print_endline "Reached first checkpoint" in *)
(* Printf.printf "This is the rank %d \n" (Mpi.comm_rank Mpi.comm_world); *)
(* Printf.printf "This is the rank %d \n" (Mpi.comm_rank Mpi.comm_world); *)
(* print_endline "This is to clean up the buffer"; *)
(* Verified until this point *)
let world = Mpi.comm_world in
(* let rank = Mpi.comm_rank world in *)
let size = Mpi.comm_size world in
let rank = Mpi.comm_rank world in
(* Printf.printf "This is the rank %d \n" rank; *)
(* let () = Printf.printf "rank %d: reached first checkpoint\n%!" rank in *)

(* let () = print_endline "Reached second checkpoint" in *)
(* let color_a = if rank = 0 || rank = 1 || rank = 2 then 1 else -1 in *)
let color_b = if rank = 1 || rank = 3 || rank = 4 then 2 else 0 in
let color_c = if rank = 2 || rank = 5 || rank = 6 then 3 else 0 in
(* let comm_a = if color_a = -1 then None else Some (Mpi.comm_split world color_a rank) in *)
(* let comm_b = if color_b = -1 then None else Some (Mpi.comm_split world color_b rank) in *)
(* let comm_c = if color_c = -1 then None else Some (Mpi.comm_split world color_c rank) in *)
let comm_b_raw = Mpi.comm_split world color_b rank in
let comm_c_raw = Mpi.comm_split world color_c rank in
let comm_b = if rank = 1 || rank = 3 || rank = 4 then Some comm_b_raw else None in
let comm_c = if rank = 2 || rank = 5 || rank = 6 then Some comm_c_raw else None in
(* let () = print_endline "Reached third checkpoint" in *)
(* let () = Printf.printf "rank %d: reached third checkpoint\n%!" rank in  *)
(* (if color_b = -1 *)
(* then *)
(*   Printf.printf "rank %d: not in B\n%!" rank *)

(* else *)
(*   Printf.printf "rank %d: in B\n%!" rank); *)

(* if color_c = -1 *)
(* then Printf.printf "rank %d: not in C\n%!" rank *)
(* else Printf.printf "rank %d: in C\n%!" rank; *)
let get_comm_exn = function
  | Some comm -> comm
  | None -> failwith "no communicator for this subtree"
in
(* let () = print_endline "Reached fourth checkpoint" in *)
(* let () = Printf.printf "rank %d: reached fourth checkpoint\n%!" rank in  *)
let _ = Mpi.barrier Mpi.comm_world in
let _ =
  match Mpi.comm_rank Mpi.comm_world with
  | 0 ->
    print_endline "Reached sixth checkpoint;this means the code for zero is executing";
    print_endline "This is to clean the buffer";
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      if iter > 0
      then (
        Mpi.send "L" (loc_to_rank "P2") 0 world;
        Mpi.send "L" (loc_to_rank "P3") 0 world;
        Mpi.send "L" (loc_to_rank "P4") 0 world;
        Mpi.send "L" (loc_to_rank "P5") 0 world;
        Mpi.send "L" (loc_to_rank "P6") 0 world;
        Mpi.send "L" (loc_to_rank "P7") 0 world;
        print_endline "Finished sending all the Ls";
        let s1 : string = Mpi.receive (loc_to_rank "P2") 0 world in
        print_endline "Finished a receive from P2";
        let s2 : string = Mpi.receive (loc_to_rank "P3") 0 world in
        print_endline "Finished a receive from P3";
        print_endline "Stopped here just before we get stuff from P2 and P3";
        let v1 : int = Marshal.from_string s1 0 in
        let v2 : int = Marshal.from_string s2 0 in
        (* (\* let comm_a = get_comm_exn comm_a in *\) *)
        (* (\* let rec result_array = Mpi.gather (Marshal.to_string 0 []) 0 comm_a in *\) *)
        (* let val_one = Marshal.from_string (Array.get result_array 0) 0 in *)
        (* let val_two = Marshal.from_string (Array.get result_array 1) 0 in *)
        (* let val_three = Marshal.from_string (Array.get result_array 2) 0 in *)
        let sum_result = v1 + v2 in
        print_int sum_result;
        loop (iter - 1))
      else (
        Mpi.send "R" (loc_to_rank "P2") 0 world;
        Mpi.send "R" (loc_to_rank "P3") 0 world;
        Mpi.send "R" (loc_to_rank "P4") 0 world;
        Mpi.send "R" (loc_to_rank "P5") 0 world;
        Mpi.send "R" (loc_to_rank "P6") 0 world;
        Mpi.send "R" (loc_to_rank "P7") 0 world;
        let rec _unit_1 = () in
        print_endline "Terminate Unoptimized")
    in
    let rec start_time = gettimeofday () in
    (* let () = print_endline "Reached just before the loop" in  *)
    let rec _unit_2 = loop 5 in
    let rec end_time = gettimeofday () in
    let rec time_diff = (sub_float end_time) start_time in
    print_float time_diff
  | 1 ->
    print_endline "This is for a flush";
    print_endline "Looks like 1 is firing";
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag world with
      | "L" ->
        (* print_endline "Got an L in 2"; *)
        let rec result = 2 in
        let comm_b = get_comm_exn comm_b in
        let rec result_array = Mpi.gather (Marshal.to_string result []) 0 comm_b in
        let val_one = Marshal.from_string (Array.get result_array 0) 0 in
        let val_two = Marshal.from_string (Array.get result_array 1) 0 in
        let val_three = Marshal.from_string (Array.get result_array 2) 0 in
        let sum_result = val_one + val_two + val_three in
        let _ = Mpi.send (Marshal.to_string sum_result []) (loc_to_rank "P1") 0 world in
        (* let comm_a = get_comm_exn comm_a in *)
        (* let _ = Mpi.gather (Marshal.to_string sum_result []) 0 comm_a in *)
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let () = print_endline "In here" in
    let rec _unit_16 = loop () in
    ()
  | 2 ->
    print_endline "Looks like 2 is firing";
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag world with
      | "L" ->
        print_endline "Got an L in 3";
        let rec result = 3 in
        let comm_c = get_comm_exn comm_c in
        let rec result_array = Mpi.gather (Marshal.to_string result []) 0 comm_c in
        let val_one = Marshal.from_string (Array.get result_array 0) 0 in
        let val_two = Marshal.from_string (Array.get result_array 1) 0 in
        let val_three = Marshal.from_string (Array.get result_array 2) 0 in
        let sum_result = val_one + val_two + val_three in
        let _ = Mpi.send (Marshal.to_string sum_result []) (loc_to_rank "P1") 0 world in
        (* let comm_a = get_comm_exn comm_a in *)
        (* let _ = Mpi.gather (Marshal.to_string sum_result []) 0 comm_a in *)
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_16 = loop () in
    ()
  | 3 ->
    print_endline "Looks like 3 is firing";
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag world with
      | "L" ->
        print_endline "Got an L in 4";
        let rec result = 4 in
        let rec _unit_18 =
          let comm_b = get_comm_exn comm_b in
          let val_17 = result in
          Mpi.gather (Marshal.to_string val_17 []) 0 comm_b
        in
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_19 = loop () in
    ()
  | 4 ->
    print_endline "Looks like 4 is firing";
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag world with
      | "R" -> ()
      | "L" ->
        Printf.printf "We are in here\n%!";
        print_endline "Got an L in 5";
        let rec result = 5 in
        let rec _unit_21 =
          let val_20 = result in
          let comm_b = get_comm_exn comm_b in
          Mpi.gather (Marshal.to_string val_20 []) 0 comm_b
        in
        loop ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_22 = loop () in
    ()
  | 5 ->
    print_endline "Looks like 5 is firing";
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag world with
      | "L" ->
        print_endline "Got an L in 6";
        let rec result = 6 in
        let rec _unit_24 =
          let val_23 = result in
          let comm_c = get_comm_exn comm_c in
          Mpi.gather (Marshal.to_string val_23 []) 0 comm_c
        in
        print_endline "One loop over for 6";
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_25 = loop () in
    ()
  | 6 ->
    print_endline "Looks like 6 is firing";
    let rec gettimeofday arg = Unix.gettimeofday arg in
    let rec print_float arg = Stdlib.print_float arg in
    let rec sub_float arg = Stdlib.( -. ) arg in
    let rec loop iter =
      match Mpi.receive (loc_to_rank "P1") Mpi.any_tag world with
      | "L" ->
        print_endline "Got an L in 7";
        let rec result = 7 in
        let val_26 = result in
        let comm_c = get_comm_exn comm_c in
        let _ = Mpi.gather (Marshal.to_string val_26 []) 0 comm_c in
        print_endline "One loop over for 7";
        loop ()
      | "R" -> ()
      | _ -> failwith "Runtime Error: Unmatched label"
    in
    let rec _unit_28 = loop () in
    ()
  | _ -> failwith "Runtime Error: Unknown rank"
in
()
