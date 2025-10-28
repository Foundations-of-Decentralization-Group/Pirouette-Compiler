tester := fun str -> A.(str = "myTest");
ic := (ic_open_in_stdlib A."typcheck.txt");
line_from_file := ic_input_line_stdlib ic;
_ := tester line_from_file;