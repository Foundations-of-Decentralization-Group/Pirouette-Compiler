open Lexing
open Pirouette.Expr
open Basictypes
open Controlang.Ctrl
open Backend_intf
open Ocaml_interthread
open Ocaml_backend
open Core
open Stdlib


exception InvalidProgramException of string
exception EndPointProjectionFailedException of string


let result_list : astType list ref = ref []

let base_dir = "bin/"
let output_file_name = "output"

type language =
  | OCaml

let getLang (lang_name: string): language = 
  match lang_name with
    | "ocaml" -> OCaml
    | _ -> raise (InvalidProgramException "Invalid Program construct encountered")

let getCommMedium (comm_medium: string) = 
  match comm_medium with
    | "inter-thread" -> InterThread
    | _ -> raise (InvalidProgramException "Invalid Program construct encountered")

let read_config_file (filename : string) : string list =
  let config_list = ref [] in
  let in_channel = Core.In_channel.create filename in
  try
    while true do
      match Core.In_channel.input_line in_channel with
      | Some line ->
          let key = String.trim line in
          config_list := key :: !config_list
      | None -> raise End_of_file
    done;
    !config_list
  with
  | End_of_file ->
      Core.In_channel.close in_channel;
      List.rev !config_list

let operation lang ctrl_ast process_impl = 
  let confMap = read_config_file "config.conf" in
  match lang, process_impl with
      | OCaml, InterThread -> 
        let module Ocaml_InterThread = Ocaml_backend(Ocaml_interthread) in
        let output_file_name = base_dir ^ "ocaml/" ^ output_file_name ^ Ocaml_InterThread.ext  in
        Ocaml_InterThread.main ctrl_ast confMap output_file_name
      | _ -> raise (InvalidProgramException "Invalid Program construct encountered")

let read_file file_name =
  let in_channel = In_channel.create file_name in
  let rec read_lines acc =
    match In_channel.input_line in_channel with
    | Some line -> read_lines (line :: acc)
    | None ->
        In_channel.close in_channel;
        List.rev acc
  in
  read_lines []

  let () =
  Command_unix.run ~version:"1.0" ~build_info:"RWO" 
    (Command.basic
       ~summary:"Process and manipulate data."
       (let open Command.Let_syntax in
        let%map_open language = anon ("language" %: string)
        and comm_type = anon ("comm_type" %: string)
        and _file_name = anon ("file_name" %: string)
        and _config_file = anon ("config_file" %: string)
        and _output_file = anon ("output_file" %: string) in
        fun () ->
          let lines = read_file _file_name in
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
                operation (getLang language) result_list (getCommMedium comm_type)
              | _ -> raise (TypeCheckingFailedException "Typechecking Failed"))
          | None -> Printf.printf "Couldnt resolve !\n\n"
       )
    )

