main := 
let _ := oc_write_string_to_file A."test.txt" pirouette_stdlib_version; in
let _ := ic_print_line_from_file A."test.txt"; in
let _ := oc_write_byte_to_file A."test.txt" A.124; in
ic_print_byte A."test.txt";
