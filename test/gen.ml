let shm_flags = "-thread -linkpkg -package threads,domainslib"

let gen_rules flags base =
  Printf.printf
    {|
      (rule
        (alias runtest)
        (target %s.ml)
        (deps
          %%{bin:pirc}
          ../test_src/%s.pir)
        (action
          (run %%{bin:pirc} ../test_src/%s.pir)))

      (rule
        (alias runtest)
        (target %s.exe)
        (deps %s.ml)
        (action
          (run ocamlfind ocamlopt -o %s.exe %s %s.ml)))

      (rule 
        (alias runtest)
        (target %s.res)
        (deps
          %s.exe
          (glob_files %s.ans))
        (action
          (progn
            (with-stdout-to %s.res
              (run ./%s.exe))
            (diff ../test_src/%s.ans %s.res))))
    |}
    (* rule 1 *)
    base
    base
    base
    (* rule 2 *)
    base
    base
    base
    flags
    base
    (* rule 3 *)
    base
    base
    base
    base
    base
    base
    base
;;

let () =
  Sys.readdir "../test_src"
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".pir")
  |> List.iter (gen_rules shm_flags)
;;
