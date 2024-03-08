{
  open Lexing
  open Parser

  exception SyntaxError of string

  let advance_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let escape = '\\' ['"' '\\' '/' 'b' 'f' 'n' 'r' 't']

let integer = '-'? digit+
let identifier = (alpha | '_' ) (alpha | digit | '_')*

rule token = parse
  | whitespace         { token lexbuf }
  | "--"               { read_single_line_comment lexbuf }
  | "{-"               { read_multi_line_comment lexbuf }
  | identifier as s    { ID (s) }
  | integer as s       { INT (int_of_string s) }
  | '"'                { STRING (read_string (Buffer.create 16) lexbuf) }
  | "unit"             { UNIT_T }
  | "int"              { INT_T }
  | "string"           { STRING_T }
  | "bool"             { BOOL_T }
  | "fun"              { FUN }
  | "type"             { TYPE }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIV }
  | 'x'                { CROSS }
  | "&&"               { AND }
  | "||"               { OR }
  | "="                { EQ }
  | "!="               { NEQ }
  | "<"                { LT }
  | "<="               { LEQ }
  | ">"                { GT }
  | ">="               { GEQ }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | '['                { LBRACKET }
  | ']'                { RBRACKET }
  | '.'                { DOT }
  | ','                { COMMA }
  | ':'                { COLON }
  | ';'                { SEMICOLON }
  | '|'                { VERTICAL }
  | '_'                { UNDERSCORE }
  | ":="               { ASSIGN }
  | "->"               { ARROW }
  | "~>"               { TILDE_ARROW }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "match"            { MATCH }
  | "with"             { WITH }
  | "let"              { LET }
  | "in"               { IN }
  | "fst"              { FST }
  | "snd"              { SND }
  | "left"             { LEFT }
  | "right"            { RIGHT }
  (* | "send"             { SEND }
  | "to"               { TO }
  | "receive"          { RECEIVE }
  | "from"             { FROM }
  | "choose"           { CHOOSE }
  | "for"              { FOR }
  | "allow"            { ALLOW }
  | "choice"           { CHOICE }
  | "ret"              { RET } *)
  | eof                { EOF }
  | _                  { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and read_string buf = parse
  | '"'       { Buffer.contents buf }
  | '\\' ('/' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' as esc) {
      let c = match esc with
        | '/'  -> '/'
        | '\\' -> '\\'
        | 'b'  -> '\b'
        | 'f'  -> '\012'
        | 'n'  -> '\n'
        | 'r'  -> '\r'
        | 't'  -> '\t'
        | _    -> assert false
      in Buffer.add_char buf c;
      read_string buf lexbuf
    }
  | [^ '"' '\\']+ as text {
      Buffer.add_string buf text;
      read_string buf lexbuf
    }
  | eof { raise (SyntaxError "String is not terminated") }
  | _   { raise (SyntaxError "Illegal string character") }

and read_single_line_comment = parse
  | newline { advance_line lexbuf; token lexbuf }
  | eof     { EOF }
  | _       { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
  | "-}"    { token lexbuf }
  | newline { advance_line lexbuf; read_multi_line_comment lexbuf }
  | eof     { raise (SyntaxError "Comment is not terminated") }
  | _       { read_multi_line_comment lexbuf }
