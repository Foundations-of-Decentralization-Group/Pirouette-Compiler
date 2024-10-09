open Lwt.Infix

type location_config = {
  location: string;
  http_address: string;
}

type config = {
  locations: location_config list;
}

let parse_location_config yaml =
  match yaml with
  | `O [("location", `String loc); ("http_address", `String addr)] ->
    Some { location = loc; http_address = addr }
  | _ -> None

let parse_config yaml =
  match yaml with
  | `O [("locations", `A locs)] ->
    let parsed_locs = List.filter_map parse_location_config locs in
    Some { locations = parsed_locs }
  | _ -> None

let load_config filename =
  Lwt_io.with_file ~mode:Input filename (fun ic ->
    Lwt_io.read ic >>= fun contents ->
    let yaml = Yaml.of_string_exn contents in
    Lwt.return (parse_config yaml)
  )

let check_locations config code =
  let defined_locations = List.filter (fun loc ->
    String.contains code loc.location.[0]
  ) config.locations in
  let undefined_locations = List.filter (fun loc ->
    not (String.contains code loc.location.[0])
  ) config.locations in
  (defined_locations, undefined_locations)

let load_pir_code filename =
  Lwt_io.with_file ~mode:Input filename Lwt_io.read

let () =
  let config_filename = "lib/config/example.yaml" in
  let pir_filename = "./examples/1.pir" in
  Lwt_main.run (
    Lwt.both (load_config config_filename) (load_pir_code pir_filename) >>= function
    | (Some config, pir_code) ->
      let defined, undefined = check_locations config pir_code in
      Lwt_io.printf "Defined locations:\n%s\n"
        (String.concat "\n" (List.map (fun loc -> 
          Printf.sprintf "%s (%s)" loc.location loc.http_address) defined)) >>= fun () ->
      Lwt_io.printf "Undefined locations:\n%s\n"
        (String.concat "\n" (List.map (fun loc -> 
          Printf.sprintf "%s (%s)" loc.location loc.http_address) undefined))
    | (None, _) ->
      Lwt_io.printf "Failed to parse config file\n"
  )