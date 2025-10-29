{- Print to stdout functions -}
foreign print_string : A.string -> unit := "print_string";
foreign print_char : unit -> unit := "print_char";
foreign print_endline : A.string -> unit := "print_endline";
foreign print_float : unit -> unit := "print_float";
foreign print_int : A.int -> unit := "print_int";
foreign print_newline : unit -> unit := "print_newline";
foreign printf : A.string -> A.string -> unit := "Printf.printf";


{- Print to stderr functions -}
foreign prerr_string : A.string -> unit := "prerr_string";
foreign prerr_char : unit-> unit := "prerr_char";
foreign prerr_endline : A.string -> unit := "prerr_endline";
foreign prerr_float : unit -> unit := "prerr_float";
foreign prerr_int : A.int -> unit := "prerr_int";
foreign prerr_newline : unit -> unit := "prerr_newline";
foreign eprintf : A.string -> A.string -> unit := "Printf.eprintf";



{- String manipulation -}
foreign sprintf : unit -> unit -> unit := "Printf.sprintf";
foreign ksprintf : (unit -> unit) -> unit -> unit -> unit := "Printf.ksprintf";
foreign string_cat : A.string -> A.string -> unit := "String.cat";
foreign cat : A.string -> A.string -> unit := "String.cat";
foreign string_of_int : A.int -> A.string := "string_of_int";
foreign string_of_bool : A.bool -> A.string := "string_of_bool";
foreign of_bytes : unit -> A.string := "String.of_bytes";
foreign to_bytes : A.string -> unit := "String.to_bytes";
foreign length : A.string -> A.int := "String.length";


{- In-channel functions -}
foreign ic_open_in : A.string -> unit := "open_in";
foreign ic_open_in_bin : unit -> unit := "open_in_bin";
foreign ic_with_open_bin : unit -> (unit -> unit) -> unit := "In_channel.with_open_bin";
foreign ic_with_open_text : unit -> (unit -> unit) -> unit := "In_channel.with_open_text";
foreign ic_get_stdin : unit -> unit := "((fun () -> (stdin)))";
foreign ic_really_input : unit -> unit -> unit -> unit -> unit := "really_input";
foreign ic_really_input_string : unit -> unit -> unit := "really_input_string";
foreign ic_seek_in : unit -> unit -> unit := "seek_in";
foreign ic_pos_in : unit -> unit := "pos_in";
foreign ic_input_line : unit -> unit := "input_line";
foreign ic_input_char : unit -> unit := "input_char";
foreign ic_input_byte : unit -> unit := "input_byte";
foreign ic_input : unit -> unit := "input";
foreign ic_input_all : unit -> A.string := "In_channel.input_all";
foreign ic_input_lines : unit -> unit := "In_channel.input_lines";
foreign ic_in_channel_length : unit -> unit := "in_channel_length";
foreign ic_close_in : unit -> unit := "close_in";
foreign ic_close_in_noerr : unit -> unit := "close_in_noerr";
foreign ic_set_binary_mode : unit -> unit -> unit := "set_binary_mode_in";
foreign ic_is_binary_mode : unit -> unit -> unit := "In_channel.is_binary_mode";
foreign ic_is_atty : unit -> unit -> unit := "In_channel.isatty";
foreign ic_fold_lines : (unit -> unit -> unit) -> unit -> unit -> unit := "In_channel.fold_lines";


{- Out-channel functions -}
foreign oc_open_out : unit -> unit := "open_out";
foreign oc_open_out_bin : unit -> unit := "open_out_bin";
foreign oc_get_stdout : unit -> unit := "((fun () -> (stdout)))";
foreign oc_get_stderr : unit -> unit := "((fun () -> (stderr)))";
foreign oc_seek_out : unit -> unit -> unit := "seek_out";
foreign oc_output_string : unit -> A.string -> unit := "output_string";
foreign oc_output_bytes : unit -> unit -> unit := "output_bytes";
foreign oc_output_byte : unit -> A.int -> unit := "output_byte";
foreign oc_output_binary_int : unit -> unit -> unit := "output_binary_int";
foreign oc_output : unit -> unit -> unit -> unit -> unit := "output";
foreign oc_output_substring : unit -> unit -> unit -> unit -> unit := "output_substring";
foreign oc_output_value : unit -> unit -> unit -> unit -> unit := "output_value";
foreign oc_fprintf : unit -> unit -> unit := "Printf.fprintf";
foreign oc_ifprintf : unit -> unit -> unit := "Printf.ifprintf";
foreign oc_kfprintf : (unit -> unit) -> unit -> unit -> unit := "Printf.kfprintf";
foreign oc_ikfprintf : (unit -> unit) -> unit -> unit -> unit := "Printf.ikfprintf";
foreign oc_out_channel_length : unit -> unit := "out_channel_length";
foreign oc_flush : unit -> unit := "flush";
foreign oc_flush_all : unit -> unit := "flush_all";
foreign oc_close_out : unit -> unit := "close_out";
foreign oc_close_out_noerr : unit -> unit := "close_out_noerr";
foreign oc_set_binary_mode_out : unit -> unit -> unit := "set_binary_mode_out";
foreign oc_is_binary_mode : unit -> unit := "Out_channel.is_binary_mode";
foreign oc_is_buffered : unit -> unit := "Out_channel.is_buffered";
foreign oc_is_atty : unit -> unit := "Out_channel.isatty";


{- File manipulation functions -}
foreign filenm_get_current_dir_name : unit -> A.string := "((fun () -> (Filename.current_dir_name)))";
foreign filenm_get_parent_dir_name : unit -> unit := "((fun () -> (Filename.parent_dir_name)))";
foreign filenm_get_dir_sep : unit -> unit := "((fun () -> (Filename.dir_sep)))";
foreign filenm_concat_path : unit -> unit -> unit := "Filename.concat";
foreign filenm_is_relative_filepath : A.string -> A.bool := "Filename.is_relative";
foreign filenm_is_implicit_filepath : A.string -> A.bool := "Filename.is_implicit";
foreign filenm_check_suffix : unit -> unit -> unit := "Filename.check_suffix";
foreign filenm_chop_suffix : unit -> unit -> unit := "Filename.chop_suffix";
foreign filenm_get_file_extension : A.string -> A.string := "Filename.extension";
foreign filenm_remove_file_extension_noerr : unit -> unit := "Filename.remove_extension";
foreign filenm_remove_file_extension : unit -> unit := "Filename.chop_extension";
foreign filenm_get_basename_from_path : A.string -> A.string := "Filename.basename";
foreign filenm_get_dirname_from_path : A.string -> A.string := "Filename.dirname";
foreign filenm_get_null_file : unit -> unit := "((fun () -> (Filename.null)))";
foreign filenm_create_temp_file : unit -> unit -> unit := "Filename.temp_file";
foreign filenm_create_and_open_temp_file : unit -> unit -> unit := "Filename.open_temp_file";
foreign filenm_get_temp_dir : unit -> unit -> unit := "Filename.temp_dir";
foreign filenm_get_temp_dir_name : unit -> unit -> unit := "Filename.get_temp_dir_name";
foreign filenm_set_temp_dir_name : unit -> unit := "Filename.set_temp_dir_name";
foreign filenm_quote_filename : unit -> unit := "Filename.set_temp_dir_name";
foreign filenm_quote_commands : unit -> unit -> unit := "Filename.quote_command";



{- System call functions -}
foreign sys_get_argv : unit -> unit := "((fun () -> (Sys.argv)))";
foreign sys_get_argv : unit -> unit := "((fun () -> (Sys.executable_name)))";
foreign sys_is_file : unit -> unit := "Sys.file_exists";
foreign sys_is_directory : A.string -> A.bool := "Sys.is_directory";
foreign sys_is_regular_file : unit -> unit := "Sys.is_regular_file";
foreign sys_remove_file : unit -> unit := "Sys.remove";
foreign sys_rename_file : unit -> unit -> unit := "Sys.rename";
foreign sys_move_file : unit -> unit -> unit := "Sys.rename";
foreign sys_get_env : A.string -> A.string := "Sys.getenv";
foreign sys_run_command : A.string -> unit := "Sys.command";
foreign sys_get_current_execution_time : unit -> unit := "Sys.time";
foreign sys_change_dir : unit -> unit := "Sys.chdir";
foreign sys_ch_dir : unit -> unit := "Sys.chdir";
foreign sys_cd : unit -> unit := "Sys.chdir";
foreign sys_make_dir : unit -> unit -> unit := "Sys.mkdir";
foreign sys_mkdir : unit -> unit -> unit := "Sys.mkdir";
foreign sys_rmdir : unit -> unit := "Sys.rmdir";
foreign sys_remove_empty_dir : unit -> unit := "Sys.rmdir";
foreign sys_get_cwd : unit -> A.string := "Sys.getcwd";
foreign sys_readdir : unit -> unit := "Sys.readdir";
foreign sys_ls : unit -> unit := "Sys.readdir";
foreign sys_get_os_type : unit -> A.string := "((fun () -> (Sys.os_type)))";
foreign sys_get_wordsize : unit -> unit := "((fun () -> (Sys.word_size)))";
foreign sys_get_ocaml_int_size : unit -> unit := "((fun () -> (Sys.int_size)))";
foreign sys_get_ocaml_max_string_length : unit -> unit := "((fun () -> (Sys.max_string_length)))";
foreign sys_get_ocaml_max_array_length : unit -> unit := "((fun () -> (Sys.max_array_length)))";
foreign sys_get_ocaml_max_floatarray_length : unit -> unit := "((fun () -> (Sys.max_floatarray_length)))";
foreign sys_check_big_endian : unit -> unit := "((fun () -> (Sys.big_endian)))";



{- Buffer handling functions -}
foreign buffer_create : A.int -> unit := "Buffer.create";
foreign buffer_copy_to_string : unit -> A.string := "Buffer.contents";
foreign buffer_copy_to_bytes : unit -> unit := "Buffer.to_bytes";
foreign buffer_copy_portion : unit -> unit -> unit -> unit := "Buffer.sub";
foreign buffer_blit : unit -> unit -> unit -> unit -> unit -> unit := "Buffer.blit";
foreign buffer_get_length : unit -> unit := "Buffer.length";
foreign buffer_clear : unit -> unit := "Buffer.clear";
foreign buffer_reset : unit -> unit := "Buffer.reset";
foreign buffer_output_to_outchannel : unit -> unit -> unit := "Buffer.output_buffer";
foreign buffer_truncate : unit -> unit -> unit := "Buffer.truncate";
foreign buffer_add_string : unit -> unit -> unit := "Buffer.add_string";
foreign buffer_add_bytes : unit -> unit -> unit := "Buffer.add_bytes";
foreign buffer_add_substring : unit -> unit -> unit -> unit -> unit := "Buffer.add_substring";
foreign buffer_add_subbytes : unit -> unit -> unit -> unit -> unit := "Buffer.add_subbytes";
foreign buffer_add_substitute : unit -> (unit -> unit) -> unit -> unit := "Buffer.add_substitute";
foreign buffer_add_buffer : unit -> unit -> unit := "Buffer.add_buffer";
foreign buffer_add_channel : unit -> unit -> unit -> unit := "Buffer.add_channel";
foreign buffer_add_uint8 : unit -> unit -> unit := "Buffer.add_uint8";
foreign buffer_add_int8 : unit -> unit -> unit := "Buffer.add_int8";
foreign buffer_add_uint16 : unit -> unit -> unit := "Buffer.add_uint16_ne";
foreign buffer_add_uint16_be : unit -> unit -> unit := "Buffer.add_uint16_be";
foreign buffer_add_uint16_le : unit -> unit -> unit := "Buffer.add_uint16_le";
foreign buffer_add_int16 : unit -> unit -> unit := "Buffer.add_int16_ne";
foreign buffer_add_int16_be : unit -> unit -> unit := "Buffer.add_int16_be";
foreign buffer_add_int16_le : unit -> unit -> unit := "Buffer.add_int16_le";
foreign buffer_bprintf : unit -> unit -> unit := "Printf.bprintf";
foreign buffer_ibprintf : unit -> unit -> unit := "Printf.ibprintf";
foreign buffer_kbprintf : (unit -> unit) -> unit -> unit -> unit := "Printf.kbprintf";
foreign buffer_ikbprintf : (unit -> unit) -> unit -> unit -> unit := "Printf.ikbprintf";



{- Error handling functions -}
foreign err_failwith : unit -> unit := "failwith";
foreign err_create_exception_invalid_arg : unit -> unit := "Invalid_argument";
foreign err_create_exception_failure : unit -> unit := "Failure";
foreign err_create_exception_division_by_zero : unit -> unit := "((fun () -> (Division_by_zero)))";
foreign err_raise : unit -> unit := "raise";
foreign err_exc_to_string : unit -> A.string := "Printexc.to_string";
foreign err_exc_to_string_default : unit -> A.string := "Printexc.to_string_default";
foreign err_print  : (unit -> unit) -> unit -> unit := "Printexc.print";
foreign err_print_backtrace  : unit -> unit := "Printexc.print_backtrace";
foreign err_get_backtrace : unit -> A.string := "Printexc.get_backtrace";
foreign err_record_backtrace  : A.bool -> unit := "Printexc.record_backtrace";
foreign err_get_backtrace_status  : unit -> A.bool := "Printexc.backtrace_status";
foreign err_get_raw_backtrace  : unit -> unit := "Printexc.get_raw_backtrace";
foreign err_print_raw_backtrace  : unit -> unit -> unit := "Printexc.print_raw_backtrace";
foreign err_raw_backtrace_to_string  : unit -> A.string := "Printexc.raw_backtrace_to_string";
foreign err_raise_with_backtrace  : unit -> unit -> unit := "Printexc.raise_with_backtrace";
foreign err_get_callstack  : unit -> unit := "Printexc.get_callstack";
foreign err_default_uncaught_exception_handler  : unit -> unit -> unit := "Printexc.default_uncaught_exception_handler";
foreign err_set_uncaught_exception_handler : unit -> unit -> unit := "Printexc.set_uncaught_exception_handler";
foreign err_backtrace_slots  : unit -> unit := "Printexc.backtrace_slots";
foreign err_backtrace_slots_of_raw_entry : unit -> unit := "Printexc.backtrace_slots_of_raw_entry";
foreign err_raw_backtrace_length : unit -> unit := "Printexc.raw_backtrace_length";
foreign err_get_raw_backtrace_slot : unit -> A.int -> unit := "Printexc.get_raw_backtrace_slot";
foreign err_convert_raw_backtrace_slot : unit -> unit := "Printexc.convert_raw_backtrace_slot";
foreign err_get_raw_backtrace_next_slot : unit -> unit := "Printexc.get_raw_backtrace_next_slot";
foreign err_exn_slot_id : unit -> A.int := "Printexc.exn_slot_id";
foreign err_exn_slot_name : unit -> A.string := "Printexc.exn_slot_name";



{- Hashing functions -}
foreign digest_compare : unit -> unit -> unit := "Digest.compare";
foreign digest_is_equal : unit -> unit -> unit := "Digest.equal";
foreign digest_string : unit -> unit := "Digest.string";
foreign digest_bytes : unit -> unit := "Digest.bytes";
foreign digest_substring : unit -> unit -> unit -> unit := "Digest.substring";
foreign digest_subbytes : unit -> unit -> unit -> unit := "Digest.subbytes";
foreign digest_file : unit -> unit := "Digest.file";
foreign diesgt_write_to_output_channel : unit -> unit -> unit := "Digest.output";
foreign digest_read_from_input_channel : unit -> unit := "Digest.input";
foreign digest_channel_portion : unit -> unit -> unit := "Digest.channel";
foreign digest_convert_to_hex_string : unit -> unit := "Digest.to_hex";
foreign digest_convert_hex_string_to_digest : unit -> unit := "Digest.of_hex";



{- Psuedo-Random Number Generator Functions -}
foreign rand_init : A.int -> unit := "Random.init";
foreign rand_full_init : unit -> unit := "Random.full_init";
foreign rand_self_init : unit -> unit := "Random.self_init";
foreign rand_bits : unit -> A.int := "Random.bits";
foreign rand_int : A.int -> A.int := "Random.int";
foreign rand_full_int : A.int -> A.int := "Random.full_int";
foreign rand_bool : unit -> A.bool := "Random.bool";
foreign rand_get_state : unit -> unit := "Random.get_state";
foreign rand_set_state : unit -> unit := "Random.set_state";
foreign rand_split_state: unit -> unit := "Random.split";


{- Program termination -}
foreign exit : A.int -> unit := "exit";
foreign exit_hook : (unit -> unit) -> unit := "at_exit";


{- Standard library metavalues -}
stdlib_version : A.string;
stdlib_version := A."0.0.2";

display_stdlib_info := fun _ -> (A.print_endline (string_cat (string_cat A."====================================\nPIROUETTE STANDARD LIBRARY INFO\n\nLibrary version: " stdlib_version) A."\nLast modified: 10/28/2025\n\n====================================\n"));