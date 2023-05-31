open Lexing
open Expr

let loop lexer =
  flush stdout;
  match Parser.prog Lexer.read lexer with
    | Some t ->
        Printf.printf "%s\n\n" (show_expr t)
    | None -> Printf.printf "Couldnt resolve !\n\n"

let read_file file_name =
  let in_channel = open_in file_name in
  let rec read_lines acc =
    try
      let line = input_line in_channel in
      read_lines (line :: acc)
    with End_of_file ->
      close_in in_channel;
      List.rev acc
  in
  read_lines []

let () =
  let file_name = "./input.txt" in
  let lines = read_file file_name in
  let content = String.concat "\n" lines in
  let lexer = from_string content in
  (* print_endline content; *)
  loop lexer