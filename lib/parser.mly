%{
  open Ast
%}

%token <string> ID
%token <int> INT
%token <string> STRING
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE CROSS
%token AND OR
%token EQUAL LEQUAL GEQUAL NEQUAL LESS GREATER
%token LPAREN RPAREN LBRACKET RBRACKET
%token DOT COMMA COLON SEMICOLON
%token ARROW TILDE_ARROW
%token IF THEN ELSE
%token MATCH WITH
%token LET IN
%token FST SND LEFT RIGHT
%token SEND TO
%token RECV FROM
%token CHOOSE FOR
%token ALLOW CHOICE
%token RET
%token EOF

/* Types */

%type <Ast.program> program

%start program

%%

// program:
//   | atom* EOF { Program $1 }
//   ;

// atom:
//   | i=INT_CONSTANT { Int i }
//   | f=FLOAT_CONSTANT { Float f }
//   | s=WORD { Word s }
//   ;

program:
decl_block EOF { Program }