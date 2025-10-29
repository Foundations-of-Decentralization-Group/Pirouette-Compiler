tester := fun str -> A.(str = "myTest");
ic := (ic_open_in A."typcheck.txt");
line_from_file := ic_input_line ic;
_ := tester line_from_file;