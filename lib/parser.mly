%{
  open Ast
%}

%token <string> ID
%token <int> INT
%token <string> STRING
%token UNIT_T INT_T STRING_T BOOL_T
%token TRUE FALSE
%token FUN TYPE
%token PLUS MINUS TIMES DIV CROSS
%token AND OR
%token EQ NEQ LT LEQ GT GEQ
%token LPAREN RPAREN LBRACKET RBRACKET
%token DOT COMMA COLON SEMICOLON VERTICAL UNDERSCORE
%token ASSIGN
%token ARROW TILDE_ARROW
%token IF THEN ELSE
%token MATCH WITH
%token LET IN
%token FST SND LEFT RIGHT
// %token SEND TO
// %token RECEIVE FROM
// %token CHOOSE FOR
// %token ALLOW CHOICE
// %token RET
%token EOF

%type <Ast.program> program
%type <Ast.decl_block> decl_block
%type <Ast.statement> statement
%type <Ast.choreo_expr> choreo_expr
%type <Ast.local_expr> local_expr
%type <Ast.pattern> pattern
%type <Ast.local_pattern> local_pattern
%type <Ast.choreo_type> choreo_type
%type <Ast.local_type> local_type
%type <Ast.bin_op> bin_op
%type <Ast.value> value
%type <Ast.loc_id> loc_id
%type <Ast.var_id> var_id
%type <Ast.fun_id> fun_id
%type <Ast.type_id> type_id
%type <Ast.sync_label> sync_label


%start program

%%

program:
  | decl_block EOF { Prog($1) }

decl_block:
  | list(statement) { $1 }

statement:
  | fun_id COLON choreo_type ARROW choreo_type    { FunDecl($1, $3, $5) }
  | var_id COLON choreo_type                      { VarDecl($1, $3) }
  | loc_id DOT var_id COLON loc_id DOT local_type { LocVarDecl($1, $3, $5, $7) }
  | TYPE type_id ASSIGN choreo_type               { TypeDecl($2, $4) }
  | var_id ASSIGN choreo_expr                     { VarAssign($1, $3) }
  | fun_id list(pattern) ASSIGN choreo_expr       { FunAssign($1, $2, $4) }
  | loc_id DOT var_id ASSIGN choreo_expr          { LocVarAssign($1, $3, $5) }

choreo_expr:
  | LPAREN RPAREN                                                                { Unit }
  | var_id                                                                       { Var($1) }
  | loc_id DOT local_expr                                                        { LocExpr($1, $3) }
  | loc_id DOT local_expr TILDE_ARROW loc_id DOT var_id SEMICOLON choreo_expr    { LocSend($1, $3, $5, $7, $9) }
  | choreo_expr TILDE_ARROW loc_id                                               { Send($1, $3) }
  | IF choreo_expr THEN choreo_expr ELSE choreo_expr                             { If($2, $4, $6) }
  | loc_id LBRACKET sync_label RBRACKET TILDE_ARROW loc_id SEMICOLON choreo_expr { Sync($1, $3, $6, $8) }
  | LET decl_block IN choreo_expr                                                { Let($2, $4) }
  | FUN fun_id ARROW choreo_expr                                                 { FunDef($2, $4) }
  | choreo_expr choreo_expr                                                      { FunApp($1, $2) }
  | LPAREN choreo_expr COMMA choreo_expr RPAREN                                  { Pair($2, $4) }
  | FST choreo_expr                                                              { Fst($2) }
  | SND choreo_expr                                                              { Snd($2) }
  | LEFT choreo_expr                                                             { Left($2) }
  | RIGHT choreo_expr                                                            { Right($2) }
  | MATCH choreo_expr WITH separated_list(VERTICAL, case)                  { Match($2, $4) }

local_expr:
  | LPAREN RPAREN                                                                  { Unit }
  | value                                                                          { Val($1) }                                                                    
  | var_id                                                                         { Var($1) }
  | local_expr bin_op local_expr                                                   { BinOp($1, $2, $3) }
  | LET var_id ASSIGN local_expr IN local_expr                                     { Let($2, $4, $6) }
  | LPAREN local_expr COMMA local_expr RPAREN                                      { Pair($2, $4) }
  | FST local_expr                                                                 { Fst($2) }
  | SND local_expr                                                                 { Snd($2) }
  | LEFT local_expr                                                                { Left($2) }
  | RIGHT local_expr                                                               { Right($2) }
  | MATCH local_expr WITH separated_list(VERTICAL, local_case)             { Match($2, $4) }

pattern:
  | UNDERSCORE                          { Default }
  | var_id                              { Var($1) }
  | LPAREN pattern COMMA pattern RPAREN { Pair($2, $4) }
  | loc_id DOT local_pattern            { LocPatt($1, $3) }
  | LEFT pattern                        { Left($2) }
  | RIGHT pattern                       { Right($2) }
  
local_pattern:
  | UNDERSCORE                                      { Default }
  | value                                           { Val($1) }
  | var_id                                          { Var($1) }
  | LPAREN local_pattern COMMA local_pattern RPAREN { Pair($2, $4) }
  | LEFT local_pattern                              { Left($2) }
  | RIGHT local_pattern                             { Right($2) }

choreo_type:
  | UNIT_T                        { TUnit }
  | loc_id DOT local_type         { TLoc($1, $3) }
  | choreo_type ARROW choreo_type { TSend($1, $3) }
  | choreo_type CROSS choreo_type { TProd($1, $3) }
  | choreo_type PLUS choreo_type  { TSum($1, $3) }

local_type:
  | UNIT_T                      { TUnit }
  | INT_T                       { TInt }
  | STRING_T                    { TString }
  | BOOL_T                      { TBool }
  | local_type CROSS local_type { TProd($1, $3) }
  | local_type PLUS local_type  { TSum($1, $3) }
  
value:
  | INT    { Int($1) }
  | STRING { String($1) }
  | TRUE   { Bool(true) }
  | FALSE  { Bool(false) }
  
loc_id:
  | ID { LocId($1) }

var_id:
  | ID { VarId($1) }

fun_id:
  | ID { FunId($1) }

type_id:
  | ID { TypeId($1) }

sync_label:
  | ID { LabelId($1) }

%inline case:
  | pattern ARROW choreo_expr { ($1, $3) }

%inline local_case:
  | local_pattern ARROW local_expr { ($1, $3) }

%inline bin_op:
  | PLUS  { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV   { Div }
  | AND   { And }
  | OR    { Or }
  | EQ    { Eq }
  | NEQ   { Neq }
  | LT    { Lt }
  | LEQ   { Leq }
  | GT    { Gt }
  | GEQ   { Geq }