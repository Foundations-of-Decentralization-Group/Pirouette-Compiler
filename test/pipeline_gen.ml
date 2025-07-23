let gen_rules basename =
  Printf.printf
    {|
(rule
 (target %s.domain.exe)
 (action
  (run ../../pirproj.sh -c %%{bin:pirc} -b domain -o . ../pipeline_src/%s.pir)))

(rule
 (with-stdout-to
  %s.domain.res
  (run ./%s.domain.exe)))

(rule
 (with-stdout-to
  %s.ans_sorted
  (system "cat ../pipeline_src/%s.ans | sort -s -k 1,1")))

(rule
 (deps %s.domain.res)
 (action
  (with-stdout-to
   %s.domain.res_sorted
   (system "cat %s.domain.res | sort -s -k 1,1"))))

(rule
 (alias pipeline-%s)
 (action
  (diff ./%s.ans_sorted %s.domain.res_sorted)))

(alias
 (name test-pipeline)
 (deps
  (alias pipeline-%s)))
|}
    basename basename
    basename basename
    basename basename basename
    basename basename
    basename basename basename
    basename
[@@ocamlformat "disable"]

let () =
  Sys.readdir "../pipeline_src"
  |> Array.to_list
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".pir")
  |> List.iter gen_rules;
  Printf.printf
    {|
(alias
 (name runtest)
 (deps
  (alias test-pipeline)))
|}
;;
