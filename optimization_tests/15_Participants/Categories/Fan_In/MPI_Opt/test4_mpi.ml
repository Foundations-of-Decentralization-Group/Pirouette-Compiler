let () =
  let world = Mpi.comm_world in
  let rank  = Mpi.comm_rank world in

  let color_b = if rank = 1 || rank = 3 || rank = 4 then 2 else 0 in

  (* Every rank calls comm_split, even non-members *)
  let comm_b_raw = Mpi.comm_split world color_b rank in
  let comm_b = if color_b = -1 then None else Some comm_b_raw in

  Printf.printf "rank %d: comm_split done, in_b=%b\n%!" rank (color_b <> -1)
