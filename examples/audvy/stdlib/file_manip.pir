main := 
let _ := oc_write_string_to_file A."test.txt" pirouette_stdlib_version; in
let ic := (ic_open_in_stdlib A."test.txt"); in
let A.line_from_file := ic_input_line_stdlib ic; in
let _ := A.print_endline_stdlib A.line_from_file; in
ic_close_in_stdlib ic;