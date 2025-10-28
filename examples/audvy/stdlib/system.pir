_ := print_endline_stdlib (string_cat_stdlib A."Current working directory: " (sys_get_cwd_stdlib A.()));
_ := print_endline_stdlib (string_cat_stdlib A."Current os type: " (sys_get_os_type_stdlib A.()));
_ := print_endline_stdlib (string_cat_stdlib A."Current word size: " (string_of_int_stdlib (sys_get_wordsize_stdlib A.())));
_ := print_endline_stdlib (string_cat_stdlib A."Current value of PIR_STDLIB environment variable: " (sys_get_env_stdlib A."PIR_STDLIB"));
_ := print_endline_stdlib (string_cat_stdlib A."Int status of running echo command: " (string_of_int_stdlib (sys_run_command_stdlib A."echo this_was_echoed_from_pirouette")));
_ := print_endline_stdlib (string_cat_stdlib A."The CWD a direcotry? " (string_of_bool_stdlib (sys_is_directory_stdlib (sys_get_cwd_stdlib A.()))));