let () =
  let rank = Mpi.comm_rank Mpi.comm_world in
  Printf.printf "rank %d: hello\n%!" rank;
