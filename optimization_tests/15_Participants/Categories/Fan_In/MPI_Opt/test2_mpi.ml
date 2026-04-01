let () =
  let rank = Mpi.comm_rank Mpi.comm_world in
  Printf.printf "rank %d: before barrier\n%!" rank;
  Mpi.barrier Mpi.comm_world;
  Printf.printf "rank %d: after barrier\n%!" rank;
