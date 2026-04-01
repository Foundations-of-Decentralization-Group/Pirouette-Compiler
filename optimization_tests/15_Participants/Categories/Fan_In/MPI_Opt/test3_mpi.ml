let () =
  let world = Mpi.comm_world in
  let rank  = Mpi.comm_rank world in
  let color_b = if rank = 1 || rank = 3 || rank = 4 then 2 else -1 in
  let comm_b  =
    if color_b = -1 then None
    else Some (Mpi.comm_split world color_b rank)
  in
  Printf.printf "rank %d: comm_split done, in_b=%b\n%!" rank (color_b <> -1);
