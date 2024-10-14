open Lwt.Infix
open Ast_utils

type location_config = {
  location: string
  ; http_address: string
}

type config = {
  locations: location_config list;
}

let parse_location_config yaml =
  match yaml with
  | `O [ ("location", `String loc); ("http_address", `String addr) ] ->
    Some { location = loc; http_address = addr }
  | _ -> None
;;

let parse_config yaml =
  match yaml with
  | `O [ ("locations", `A locs) ] ->
    let parsed_locs = List.filter_map parse_location_config locs in
    Some { locations = parsed_locs }
  | _ -> None
;;

let load_config filename =
  Lwt_io.with_file ~mode:Input filename (fun ic ->
    Lwt_io.read ic
    >>= fun contents ->
    let yaml = Yaml.of_string_exn contents in
    Lwt.return (parse_config yaml))
;;

let check_locations config choreo_ast =
  let extracted_locs = extract_locs choreo_ast in
  let config_locs = List.map (fun loc_config -> loc_config.location) config.locations in
  let undefined_locs =
    List.filter (fun loc -> not (List.mem loc config_locs)) extracted_locs
  in
  if List.length undefined_locs > 0
  then
    Error
      (Printf.sprintf "Undefined locations found: %s" (String.concat ", " undefined_locs))
  else
    Ok
      (List.filter
         (fun loc_config -> List.mem loc_config.location extracted_locs)
         config.locations)
;;

let load_pir_code filename = Lwt_io.with_file ~mode:Input filename Lwt_io.read

let () =
  let config_filename = "lib/config/example.yaml" in
  let pir_filename = "./examples/1.pir" in
  Lwt_main.run
    (Lwt.both (load_config config_filename) (load_pir_code pir_filename)
     >>= function
     | Some config, pir_code ->
       let choreo_ast = Parsing.parse_program (Lexing.from_string pir_code) in
       (match check_locations config choreo_ast with
        | Error msg -> Lwt_io.printf "Error: %s\n" msg
        | Ok defined_locations ->
          let net_ir_code =
            List.map
              (fun loc_config ->
                ( loc_config.location
                , loc_config.http_address
                , Irgen.epp choreo_ast loc_config.location ))
              defined_locations
          in
          Lwt_io.printf
            "Defined locations:\n%s\n"
            (String.concat
               "\n"
               (List.map
                  (fun loc -> Printf.sprintf "%s (%s)" loc.location loc.http_address)
                  defined_locations))
          >>= fun () ->
          Lwt_io.printf
            "Generated NetIR code:\n%s\n"
            (String.concat
               "\n"
               (List.map
                  (fun (loc, addr, net_ir) ->
                    Printf.sprintf
                      "Location: %s\nAddress: %s\nNetIR:\n%s\n"
                      loc
                      addr
                      (stringify_pprint_net_ast net_ir))
                  net_ir_code)))
     | None, _ -> Lwt_io.printf "Failed to parse config file\n")
;;
