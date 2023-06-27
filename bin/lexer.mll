{
    open Parser
    open Lexing

    exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n" 
let string = [^ 'L' 'R' '(' ')' '\\' '.' '#' ' ' '\t' '\n' '\t' '*' '-' '/' '+' '[' ']' ';' '<' '>' '=']+
let digit = ['0'-'9']
let choreographic_vars = ['A'-'Z']
let int = '-'? digit+
let conditional = '>' | '<' | '='
let sync_label = 'L' | 'R'
let bool = "true" | "false"
let type = "bool" | "int"  | "string"

rule read = 
    parse
        | whitespace {read lexbuf }
        | int { INT (int_of_string (lexeme lexbuf)) }
        | "[" {LSqParen}
        | "]" {RSqParen}
        | conditional {Condition (lexeme lexbuf)}
        | "+" {Plus}
        | "-" {Minus}
        | "*" {Product}
        | "/" {Division}
        | sync_label {SyncLbl (lexeme lexbuf)}
        | choreographic_vars {ChoreoVars (lexeme lexbuf)}
        | "if" {If}
        | "then" {Then}
        | "else" {Else}
        | "~>" {Comm_S}
        | "fun" {Fun}
        | ":=" {Assignment}
        | "let" {Let}
        | "in" {In}
        | "." {Dot}
        | "(" {LParen}
        | ")" {RParen}
        | ";" {Terminate}
        | ":" {Colon}
        | ('"' (([^'>''"']|'>'[^'>''"'])* as st) '"') { STRING st }
        | bool {BOOL (lexeme lexbuf)}
        | type {Type (lexeme lexbuf)}
        | string {Identifier (lexeme lexbuf)}
        | newline { read lexbuf }
        | eof {EOF}
        | "#" {skip_line lexbuf}
        | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
and skip_line = 
    parse
        | newline {new_line lexbuf; read lexbuf}
        | eof {EOF}
        | _ {skip_line lexbuf}
