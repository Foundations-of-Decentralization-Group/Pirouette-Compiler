{
    open Parser
    open Lexing

    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n" | ";"
let string = [^ '(' ')' '\\' '.' '#' ' ' '\t' '\n' '\t' '*' '-' '/' '+' '[' ']']+
let digit = ['0'-'9']
let int = '-'? digit+

rule read = 
    parse
        | whitespace {read lexbuf }
        | "[" {LSqParen}
        | "]" {RSqParen}
        | "<" {LessThan}
        | ">" {GreaterThan}
        | "=" {Equal}
        | "if" {If}
        | "then" {Then}
        | "else" {Else}
        | "@>" {Comm_S}
        | "fun" {Function}
        | ":=" {Assignment}
        | "/" {Divide}
        | "*" {Multiply}
        | "+" {Add}
        | "-" {Subtract}
        | "let" {Let}
        | "in" {In}
        | "." {Dot}
        | "(" {LParen}
        | ")" {RParen}
        | int { Val (int_of_string (lexeme lexbuf)) }
        | string {Identifier (lexeme lexbuf)}
        | newline {END}
        | eof {EOF}
        | "#" {skip_line lexbuf}
        | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
and skip_line = 
    parse
        | newline {new_line lexbuf; read lexbuf}
        | eof {EOF}
        | _ {skip_line lexbuf}
