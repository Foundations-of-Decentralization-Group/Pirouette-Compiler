_ := print_endline (string_cat A."Current working directory: " (sys_get_cwd A.()));
_ := print_endline (string_cat A."Current os type: " (sys_get_os_type A.()));
_ := print_endline (string_cat A."Current word size: " (string_of_int (sys_get_wordsize A.())));
_ := print_endline (string_cat A."Current value of PIR environment variable: " (sys_get_env A."PIR"));
_ := print_endline (string_cat A."Int status of running echo command: " (string_of_int (sys_run_command A."echo this_was_echoed_from_pirouette")));
_ := print_endline (string_cat A."The CWD a direcotry? " (string_of_bool (sys_is_directory (sys_get_cwd A.()))));