{- Print to stdout functions -}
foreign print_string_stdlib : unit -> unit := "print_string";
foreign print_char_stdlib : unit -> unit := "print_char";
foreign print_endline_stdlib : unit -> unit := "print_endline";
foreign print_float_stdlib : unit -> unit := "print_float";
foreign print_int_stdlib : unit -> unit := "print_int";
foreign print_newline_stdlib : unit -> unit := "print_newline";
foreign printf_stdlib : unit -> unit -> unit := "Printf.printf";


{- Print to stderr functions -}
foreign prerr_string_stdlib : unit -> unit := "prerr_string";
foreign prerr_char_stdlib : unit -> unit := "prerr_char";
foreign prerr_endline_stdlib : unit -> unit := "prerr_endline";
foreign prerr_float_stdlib : unit -> unit := "prerr_float";
foreign prerr_int_stdlib : unit -> unit := "prerr_int";
foreign prerr_newline_stdlib : unit -> unit := "prerr_newline";
foreign eprintf_stdlib : unit -> unit -> unit := "Printf.eprintf";



{- In-channel functions -}
foreign ic_open_in_stdlib : unit -> unit := "open_in";
foreign ic_open_in_bin_stdlib : unit -> unit := "open_in_bin";
foreign ic_with_open_bin_stdlib : unit -> (unit -> unit) -> unit := "In_channel.with_open_bin";
foreign ic_with_open_text_stdlib : unit -> (unit -> unit) -> unit := "In_channel.with_open_text";
foreign ic_get_stdin_stdlib : unit -> unit := "((fun () -> (stdin)))";
foreign ic_really_input_stdlib : unit -> unit -> unit -> unit -> unit := "really_input";
foreign ic_really_input_string_stdlib : unit -> unit -> unit := "really_input_string";
foreign ic_seek_in_stdlib : unit -> unit -> unit := "seek_in";
foreign ic_pos_in_stdlib : unit -> unit := "pos_in";
foreign ic_input_line_stdlib : unit -> unit := "input_line";
foreign ic_input_char_stdlib : unit -> unit := "input_char";
foreign ic_input_byte_stdlib : unit -> unit := "input_byte";
foreign ic_input_stdlib : unit -> unit := "input";
foreign ic_input_all_stdlib : unit -> unit := "In_channel.input_all";
foreign ic_input_lines_stdlib : unit -> unit := "In_channel.input_lines";
foreign ic_in_channel_length_stdlib : unit -> unit := "in_channel_length";
foreign ic_close_in_stdlib : unit -> unit := "close_in";
foreign ic_close_in_noerr_stdlib : unit -> unit := "close_in_noerr";
foreign ic_set_binary_mode_stdlib : unit -> unit -> unit := "set_binary_mode_in";
foreign ic_is_binary_mode_stdlib : unit -> unit -> unit := "In_channel.is_binary_mode";
foreign ic_is_atty_stdlib : unit -> unit -> unit := "In_channel.isatty";
foreign ic_fold_lines : (unit -> unit -> unit) -> unit -> unit -> unit := "In_channel.fold_lines";



{- Out-channel functions -}
foreign oc_open_out_stdlib : unit -> unit := "open_out";
foreign oc_open_out_bin_stdlib : unit -> unit := "open_out_bin";
foreign oc_get_stdout_stdlib : unit -> unit := "((fun () -> (stdout)))";
foreign oc_get_stderr_stdlib : unit -> unit := "((fun () -> (stderr)))";
foreign oc_seek_out_stdlib : unit -> unit -> unit := "seek_out";
foreign oc_output_string_stdlib : unit -> unit -> unit := "output_string";
foreign oc_output_bytes_stdlib : unit -> unit -> unit := "output_bytes";
foreign oc_output_byte_stdlib : unit -> unit -> unit := "output_byte";
foreign oc_output_binary_int_stdlib : unit -> unit -> unit := "output_binary_int";
foreign oc_output_stdlib : unit -> unit -> unit -> unit -> unit := "output";
foreign oc_output_substring_stdlib : unit -> unit -> unit -> unit -> unit := "output_substring";
foreign oc_output_value_stdlib : unit -> unit -> unit -> unit -> unit := "output_value";
foreign oc_print_to_file_stdlib : unit -> unit -> unit := "Printf.fprintf";
foreign oc_fprintf_stdlib : unit -> unit -> unit := "Printf.fprintf";
foreign oc_ifprintf_stdlib : unit -> unit -> unit := "Printf.ifprintf";
foreign oc_kfprintf_stdlib : (unit -> unit) -> unit -> unit -> unit := "Printf.kfprintf";
foreign oc_ikfprintf_stdlib : (unit -> unit) -> unit -> unit -> unit := "Printf.ikfprintf";
foreign oc_out_channel_length_stdlib : unit -> unit := "out_channel_length";
foreign oc_flush_stdlib : unit -> unit := "flush";
foreign oc_flush_all_stdlib : unit -> unit := "flush_all";
foreign oc_close_out_stdlib : unit -> unit := "close_out";
foreign oc_close_out_noerr_stdlib : unit -> unit := "close_out_noerr";
foreign oc_set_binary_mode_out_stdlib : unit -> unit -> unit := "set_binary_mode_out";
foreign oc_is_binary_mode_stdlib : unit -> unit := "Out_channel.is_binary_mode";
foreign oc_is_buffered_stdlib : unit -> unit := "Out_channel.is_buffered";
foreign oc_is_atty_stdlib : unit -> unit := "Out_channel.isatty";
oc_write_string_to_file := fun file str -> 
    (let oc := (oc_open_out_stdlib file); in
    let _ := oc_print_to_file_stdlib oc A."%s\n" str; in
    oc_close_out_stdlib oc);


{- File manipulation functions -}
foreign filenm_get_current_dir_name_stdlib : unit -> unit := "((fun () -> (Filename.current_dir_name)))";
foreign filenm_get_parent_dir_name_stdlib : unit -> unit := "((fun () -> (Filename.parent_dir_name)))";
foreign filenm_get_dir_sep_stdlib : unit -> unit := "((fun () -> (Filename.dir_sep)))";
foreign filenm_concat_path_stdlib : unit -> unit -> unit := "Filename.concat";
foreign filenm_is_relative_filepath_stdlib : unit -> unit := "Filename.is_relative";
foreign filenm_is_implicit_filepath_stdlib : unit -> unit := "Filename.is_implicit";
foreign filenm_check_suffix_stdlib : unit -> unit -> unit := "Filename.check_suffix";
foreign filenm_chop_suffix_stdlib : unit -> unit -> unit := "Filename.chop_suffix";
foreign filenm_get_file_extension_stdlib : unit -> unit := "Filename.extension";
foreign filenm_remove_file_extension_noerr_stdlib : unit -> unit := "Filename.remove_extension";
foreign filenm_remove_file_extension_stdlib : unit -> unit := "Filename.chop_extension";
foreign filenm_get_basename_from_path_stdlib : unit -> unit := "Filename.basename";
foreign filenm_get_dirname_from_path_stdlib : unit -> unit := "Filename.dirname";
foreign filenm_get_null_file_stdlib : unit -> unit := "((fun () -> (Filename.null)))";
foreign filenm_create_temp_file_stdlib : unit -> unit -> unit := "Filename.temp_file";
foreign filenm_create_and_open_temp_file_stdlib : unit -> unit -> unit := "Filename.open_temp_file";
foreign filenm_get_temp_dir_stdlib : unit -> unit -> unit := "Filename.temp_dir";
foreign filenm_get_temp_dir_name_stdlib : unit -> unit -> unit := "Filename.get_temp_dir_name";
foreign filenm_set_temp_dir_name_stdlib : unit -> unit := "Filename.set_temp_dir_name";
foreign filenm_quote_filename_stdlib : unit -> unit := "Filename.set_temp_dir_name";
foreign filenm_quote_commands_stdlib : unit -> unit -> unit := "Filename.quote_command";



{- System call functions -}
foreign sys_get_argv_stdlib : unit -> unit := "((fun () -> (Sys.argv)))";
foreign sys_get_argv_stdlib : unit -> unit := "((fun () -> (Sys.executable_name)))";
foreign sys_is_file_stdlib : unit -> unit := "Sys.file_exists";
foreign sys_is_directory_stdlib : unit -> unit := "Sys.is_directory";
foreign sys_is_regular_file_stdlib : unit -> unit := "Sys.is_regular_file";
foreign sys_remove_file_stdlib : unit -> unit := "Sys.remove";
foreign sys_rename_file_stdlib : unit -> unit -> unit := "Sys.rename";
foreign sys_move_file_stdlib : unit -> unit -> unit := "Sys.rename";
foreign sys_get_env_stdlib : unit -> unit -> unit := "Sys.getenv";
foreign sys_run_command_stdlib : unit -> unit := "Sys.command";
foreign sys_get_current_execution_time_stdlib : unit -> unit := "Sys.time";
foreign sys_change_dir_stdlib : unit -> unit := "Sys.chdir";
foreign sys_ch_dir_stdlib : unit -> unit := "Sys.chdir";
foreign sys_cd_stdlib : unit -> unit := "Sys.chdir";
foreign sys_make_dir_stdlib : unit -> unit -> unit := "Sys.mkdir";
foreign sys_mkdir_stdlib : unit -> unit -> unit := "Sys.mkdir";
foreign sys_rmdir_stdlib : unit -> unit := "Sys.rmdir";
foreign sys_remove_empty_dir_stdlib : unit -> unit := "Sys.rmdir";
foreign sys_get_cwd_stdlib : unit -> unit := "Sys.getcwd";
foreign sys_readdir_stdlib : unit -> unit := "Sys.readdir";
foreign sys_ls_stdlib : unit -> unit := "Sys.readdir";
foreign sys_get_os_type_stdlib : unit -> unit := "((fun () -> (Sys.os_type)))";
foreign sys_get_wordsize_stdlib : unit -> unit := "((fun () -> (Sys.word_size)))";
foreign sys_get_ocaml_int_size_stdlib : unit -> unit := "((fun () -> (Sys.int_size)))";
foreign sys_get_ocaml_max_string_length_stdlib : unit -> unit := "((fun () -> (Sys.max_string_length)))";
foreign sys_get_ocaml_max_array_length_stdlib : unit -> unit := "((fun () -> (Sys.max_array_length)))";
foreign sys_get_ocaml_max_floatarray_length_stdlib : unit -> unit := "((fun () -> (Sys.max_floatarray_length)))";
foreign sys_check_big_endian_stdlib : unit -> unit := "((fun () -> (Sys.big_endian)))";



{- Buffer handling functions -}
foreign buffer_create_stdlib : unit -> unit := "Buffer.create";
foreign buffer_copy_to_string_stdlib : unit -> unit := "Buffer.contents";
foreign buffer_copy_to_bytes_stdlib : unit -> unit := "Buffer.to_bytes";
foreign buffer_copy_portion_stdlib : unit -> unit -> unit -> unit := "Buffer.sub";
foreign buffer_blit_stdlib : unit -> unit -> unit -> unit -> unit -> unit := "Buffer.blit";
foreign buffer_get_length_stdlib : unit -> unit := "Buffer.length";
foreign buffer_clear_stdlib : unit -> unit := "Buffer.clear";
foreign buffer_reset_stdlib : unit -> unit := "Buffer.reset";
foreign buffer_output_to_outchannel_stdlib : unit -> unit -> unit := "Buffer.output_buffer";
foreign buffer_truncate_stdlib : unit -> unit -> unit := "Buffer.truncate";
foreign buffer_add_string_stdlib : unit -> unit -> unit := "Buffer.add_string";
foreign buffer_add_bytes_stdlib : unit -> unit -> unit := "Buffer.add_bytes";
foreign buffer_add_substring_stdlib : unit -> unit -> unit -> unit -> unit := "Buffer.add_substring";
foreign buffer_add_subbytes_stdlib : unit -> unit -> unit -> unit -> unit := "Buffer.add_subbytes";
foreign buffer_add_substitute_stdlib : unit -> (unit -> unit) -> unit -> unit := "Buffer.add_substitute";
foreign buffer_add_buffer_stdlib : unit -> unit -> unit := "Buffer.add_buffer";
foreign buffer_add_channel_stdlib : unit -> unit -> unit -> unit := "Buffer.add_channel";
foreign buffer_add_uint8_stdlib : unit -> unit -> unit := "Buffer.add_uint8";
foreign buffer_add_int8_stdlib : unit -> unit -> unit := "Buffer.add_int8";
foreign buffer_add_uint16_stdlib : unit -> unit -> unit := "Buffer.add_uint16_ne";
foreign buffer_add_uint16_be_stdlib : unit -> unit -> unit := "Buffer.add_uint16_be";
foreign buffer_add_uint16_le_stdlib : unit -> unit -> unit := "Buffer.add_uint16_le";
foreign buffer_add_int16_stdlib : unit -> unit -> unit := "Buffer.add_int16_ne";
foreign buffer_add_int16_be_stdlib : unit -> unit -> unit := "Buffer.add_int16_be";
foreign buffer_add_int16_le_stdlib : unit -> unit -> unit := "Buffer.add_int16_le";
foreign buffer_bprintf_stdlib : unit -> unit -> unit := "Printf.bprintf";
foreign buffer_ibprintf_stdlib : unit -> unit -> unit := "Printf.ibprintf";
foreign buffer_kbprintf_stdlib : (unit -> unit) -> unit -> unit -> unit := "Printf.kbprintf";
foreign buffer_ikbprintf_stdlib : (unit -> unit) -> unit -> unit -> unit := "Printf.ikbprintf";



{- Error handling functions -}
foreign err_failwith_stdlib : unit -> unit := "failwith";
foreign err_create_exception_invalid_arg_stdlib : unit -> unit := "Invalid_argument";
foreign err_create_exception_failure_stdlib : unit -> unit := "Failure";
foreign err_create_exception_division_by_zero_stdlib : unit -> unit := "((fun () -> (Division_by_zero)))";
foreign err_raise_stdlib : unit -> unit := "raise";
foreign err_exc_to_string_stdlib : unit -> A.string := "Printexc.to_string";
foreign err_exc_to_string_default_stdlib : unit -> A.string := "Printexc.to_string_default";
foreign err_print_stdlib  : (unit -> unit) -> unit -> unit := "Printexc.print";
foreign err_print_backtrace_stdlib  : unit -> unit := "Printexc.print_backtrace";
foreign err_get_backtrace : unit -> A.string := "Printexc.get_backtrace";
foreign err_record_backtrace_stdlib  : A.bool -> unit := "Printexc.record_backtrace";
foreign err_get_backtrace_status_stdlib  : unit -> A.bool := "Printexc.backtrace_status";
foreign err_get_raw_backtrace_stdlib  : unit -> unit := "Printexc.get_raw_backtrace";
foreign err_print_raw_backtrace_stdlib  : unit -> unit -> unit := "Printexc.print_raw_backtrace";
foreign err_raw_backtrace_to_string_stdlib  : unit -> A.string := "Printexc.raw_backtrace_to_string";
foreign err_raise_with_backtrace_stdlib  : unit -> unit -> unit := "Printexc.raise_with_backtrace";
foreign err_get_callstack_stdlib  : unit -> unit := "Printexc.get_callstack";
foreign err_default_uncaught_exception_handler_stdlib  : unit -> unit -> unit := "Printexc.default_uncaught_exception_handler";
foreign err_set_uncaught_exception_handler_stdlib : unit -> unit -> unit := "Printexc.set_uncaught_exception_handler";
foreign err_backtrace_slots  : unit -> unit := "Printexc.backtrace_slots";
foreign err_backtrace_slots_of_raw_entry : unit -> unit := "Printexc.backtrace_slots_of_raw_entry";
foreign err_raw_backtrace_length : unit -> unit := "Printexc.raw_backtrace_length";
foreign err_get_raw_backtrace_slot : unit -> A.int -> unit := "Printexc.get_raw_backtrace_slot";
foreign err_convert_raw_backtrace_slot : unit -> unit := "Printexc.convert_raw_backtrace_slot";
foreign err_get_raw_backtrace_next_slot : unit -> unit := "Printexc.get_raw_backtrace_next_slot";
foreign err_exn_slot_id : unit -> A.int := "Printexc.exn_slot_id";
foreign err_exn_slot_name : unit -> A.string := "Printexc.exn_slot_name";



{- Hashing functions -}
foreign digest_compare_stdlib : unit -> unit -> unit := "Digest.compare";
foreign digest_is_equal_stdlib : unit -> unit -> unit := "Digest.equal";
foreign digest_string_stdlib : unit -> unit := "Digest.string";
foreign digest_bytes_stdlib : unit -> unit := "Digest.bytes";
foreign digest_substring_stdlib : unit -> unit -> unit -> unit := "Digest.substring";
foreign digest_subbytes_stdlib : unit -> unit -> unit -> unit := "Digest.subbytes";
foreign digest_file_stdlib : unit -> unit := "Digest.file";
foreign diesgt_write_to_output_channel_stdlib : unit -> unit -> unit := "Digest.output";
foreign digest_read_from_input_channel_stdlib : unit -> unit := "Digest.input";
foreign digest_channel_portion_stdlib : unit -> unit -> unit := "Digest.channel";
foreign digest_convert_to_hex_string_stdlib : unit -> unit := "Digest.to_hex";
foreign digest_convert_hex_string_to_digest_stdlib : unit -> unit := "Digest.of_hex";



{- Psuedo-Random Number Generator Functions -}
foreign rand_init_stdlib : A.int -> unit := "Random.init";
foreign rand_full_init_stdlib : unit -> unit := "Random.full_init";
foreign rand_self_init_stdlib : unit -> unit := "Random.self_init";
foreign rand_bits_stdlib : unit -> A.int := "Random.bits";
foreign rand_int_stdlib : A.int -> A.int := "Random.int";
foreign rand_full_int_stdlib : A.int -> A.int := "Random.full_int";
foreign rand_bool_stdlib : unit -> A.bool := "Random.bool";
foreign rand_get_state_stdlib : unit -> unit := "Random.get_state";
foreign rand_set_state_stdlib : unit -> unit := "Random.set_state";
foreign rand_split_state_stdlib: unit -> unit := "Random.split";


{- String manipulation -}
foreign sprintf_stdlib : unit -> unit -> unit := "Printf.sprintf";
foreign ksprintf_stdlib : (unit -> unit) -> unit -> unit -> unit := "Printf.ksprintf";
foreign string_cat_stdlib : A.string -> A.string -> unit := "String.cat";
foreign cat_stdlib : A.string -> A.string -> unit := "String.cat";


{- Program termination -}
foreign exit_stdlib : unit -> unit := "exit";
foreign exit_hook_stdlib : (unit -> unit) -> unit := "at_exit";


{- Standard library metavalues -}
pirouette_stdlib_version : A.string;
pirouette_stdlib_version := A."0.0.2";

pirouette_stdlib_info := fun _ -> (A.print_endline (string_cat_stdlib (string_cat_stdlib A."====================================\nPIROUETTE STANDARD LIBRARY INFO\n\nLibrary version: " pirouette_stdlib_version) A."\nLast modified: 10/21/2025\n\n====================================\n"));