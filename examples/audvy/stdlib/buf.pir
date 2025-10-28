myBuf1 := buffer_create_stdlib A.30;
_ := buffer_add_string_stdlib myBuf1 A."This is inside a...";
myBuf2 := buffer_create_stdlib A.30;
_ := buffer_add_string_stdlib myBuf2 A." Buffer!";
_ := buffer_add_buffer_stdlib myBuf1 myBuf2;
_ := print_endline_stdlib (buffer_copy_to_string_stdlib myBuf1);