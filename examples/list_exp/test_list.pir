foreign printf : unit -> unit := "Printf.printf";

A.print_number arg :=

   A.(match arg with
   | [] -> ()
   | [1,2,3,4,5] -> let _ := printf "Done" in ());

main :=
   let A.x := A.[1,2,3,5]; in
   A.print_number A.x;