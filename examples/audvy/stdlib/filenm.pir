_ := print_endline_stdlib (string_cat_stdlib A."Current directory name: " (filenm_get_current_dir_name_stdlib A.()));
_ := print_endline_stdlib (string_cat_stdlib A."Parent directory name: " (filenm_get_parent_dir_name_stdlib A.()));
_ := print_endline_stdlib (string_cat_stdlib A."System directory seperator: " (filenm_get_dir_sep_stdlib A.()));
_ := print_endline_stdlib (string_cat_stdlib A."CWD: " (sys_get_cwd_stdlib A.()));
_ := print_endline_stdlib (string_cat_stdlib A."Is CWD a relative path?: " (string_of_bool_stdlib (filenm_is_relative_filepath_stdlib (sys_get_cwd_stdlib A.()))));
_ := print_endline_stdlib (string_cat_stdlib A."Basename from CWD: " (filenm_get_basename_from_path_stdlib (sys_get_cwd_stdlib A.())));
_ := print_endline_stdlib (string_cat_stdlib A."Dirname from CWD: " (filenm_get_dirname_from_path_stdlib (sys_get_cwd_stdlib A.())));
_ := print_endline_stdlib (string_cat_stdlib A."Extension from this file: " (filenm_get_file_extension_stdlib (string_cat_stdlib (string_cat_stdlib (sys_get_cwd_stdlib A.()) (filenm_get_dir_sep_stdlib A.())) A."filenm.pir")));