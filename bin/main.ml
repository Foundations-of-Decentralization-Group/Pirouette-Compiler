open Lexing
open Pirouette.Expr
open Basictypes
open Controlang.Ctrl
open Backend_intf
open Ocaml_interthread
open Ocaml_backend

exception InvalidProgramException of string
exception EndPointProjectionFailedException of string


let result_list : astType list ref = ref []

let base_dir = "bin/"
let output_file_name = "output"

type language =
  | OCaml
[@@deriving show]

let read_config_file (filename : string) : string list =
  let config_list = ref [] in
  let in_channel = open_in filename in
  try
    while true do
      let line = input_line in_channel in
      let key = String.trim line in
      config_list := key :: !config_list
    done;
    !config_list
  with
  | End_of_file ->
      close_in in_channel;
      List.rev !config_list
  | exn ->
      close_in_noerr in_channel;
      raise exn

let operation lang ctrl_ast process_impl = 
  let confMap = read_config_file "config.conf" in
  match lang, process_impl with
      | OCaml, InterThread -> 
        let module Ocaml_InterThread = Ocaml_backend(Ocaml_interthread) in
        let output_file_name = base_dir ^ "ocaml/" ^ output_file_name ^ Ocaml_InterThread.ext  in
        Ocaml_InterThread.main ctrl_ast confMap output_file_name
      | _ -> raise (InvalidProgramException "Invalid Program construct encountered")

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
  match Parser.prog Lexer.read lexer with
  | Some t ->
    (match type_check t ImmutableMap.empty ChoreoMap.empty with
      | GTcast(_, Some ast) ->
        let entities : LocationSet.t = get_entitities ast in
        LocationSet.iter (fun entity -> 
          let res = expr_to_ctrl ast entity in
          let Location str_entity = entity in
          let r = match res with
            | Some res -> res
            | None -> raise (EndPointProjectionFailedException "EPP Failed")
          in
          result_list := Ast{code = r; prop = str_entity} :: !result_list
        ) entities;
        operation OCaml result_list InterThread
      | _ -> raise (TypeCheckingFailedException "Typechecking Failed"))
  | None -> Printf.printf "Couldnt resolve !\n\n"

