foreign print_string_stdlib : unit -> unit := "print_string";
foreign print_char_stdlib : unit -> unit := "print_char";
foreign print_endline_stdlib : unit -> unit := "print_endline";
foreign print_float_stdlib : unit -> unit := "print_float";
foreign print_int_stdlib : unit -> unit := "print_int";
foreign print_newline_stdlib : unit -> unit := "print_newline";
foreign inchannel_open_text_stdlib : unit -> unit := "In_channel.with_open_text";
foreign outchannel_open_text_stdlib : unit -> unit := "Out_channel.with_open_text";


pirouette_stdlib_version : A.string;
pirouette_stdlib_version := A."0.0.1";