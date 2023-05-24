{
    open Parser
    open Lexing
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let string = [^ '(' ')' '\\' '.' '#' ' ' '\t' '\n' '\t']+

rule read = 
    parse
        | whitespace {read lexbuf }
        | "\\" {Lambda}
        | "." {Dot}
        | "(" {LParen}
        | ")" {RParen}
        | string {Identifier (lexeme lexbuf)}
        | newline {END}
        | eof {EOF}
        | "#" {skip_line lexbuf}
and skip_line = 
    parse
        | newline {new_line lexbuf; read lexbuf}
        | eof {EOF}
        | _ {skip_line lexbuf}
