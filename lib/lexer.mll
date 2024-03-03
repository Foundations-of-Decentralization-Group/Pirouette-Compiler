{
  open Lexing
  open Parser

  let advance_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

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
  | "send"             { SEND }
  | "to"               { TO }
  | "receive"          { RECEIVE }
  | "from"             { FROM }
  | "choose"           { CHOOSE }
  | "for"              { FOR }
  | "allow"            { ALLOW }
  | "choice"           { CHOICE }
  | "ret"              { RET }
  | eof                { EOF }
  | _                  { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and read_string buffer = parse
  | '"' { Buffer.contents buffer }  (* End of string *)
  | '\\' ['/' '\\' 'b' 'f' 'n' 'r' 't' ] as escaped_char {
      let c = match escaped_char with
        | '/'  -> '/'
        | '\\' -> '\\'
        | 'b'  -> '\b'
        | 'f'  -> '\012'
        | 'n'  -> '\n'
        | 'r'  -> '\r'
        | 't'  -> '\t'
        | _    -> raise (SyntaxError ("Illegal escape sequence: \\" ^ (String.make 1 any)))
      in Buffer.add_char buffer c;
      read_string buffer lexbuf
    }
  | [^ '"' '\\']+ as str { (* Non-special characters *)
      Buffer.add_string buffer str;
      read_string buffer lexbuf
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
