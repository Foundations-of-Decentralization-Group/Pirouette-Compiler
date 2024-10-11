(*let print_stat info_message = Printf.printf "%s \n%!" info_message in
  print_stat "this is being executed"*)

let print_add x y = x + y
let print_sub x y = x - y
let v1 = print_add 1 3
let v2 = print_sub 5 4
let () = Printf.printf "%n %n \n" v1 v2
