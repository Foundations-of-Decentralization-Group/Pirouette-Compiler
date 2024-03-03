%{
  open Ast
%}

%token <string> ID
%token <int> INT
%token <string> STRING
%token UNIT_T INT_T STRING_T BOOL_T FUN TYPE
%token TRUE FALSE
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
%type <Ast.declaration> declaration
%type <Ast.assignment> assignment
%type <Ast.decl_or_assign> decl_or_assign
%type <Ast.choreo_seq> choreo_seq
%type <Ast.choreo_expr> choreo_expr
%type <Ast.local_expr> local_expr
%type <Ast.pattern> pattern
%type <Ast.local_pattern> local_pattern
%type <Ast.choreo_type> choreo_type
%type <Ast.local_type> local_type
%type <Ast.bin_op> bin_op
%type <Ast.value> value
%type <Ast.loc_name> loc_name
%type <Ast.var_name> var_name
%type <Ast.fn_name> fn_name
%type <Ast.type_name> type_name
%type <Ast.sync_label> sync_label


%start program

%%

program:
  | decl_block EOF { Prog($1) }

// (* The constructed pushdown automaton uses less stack space with left-recursive definitions
//  * Use prepend and reverse (if the order needs to be kept) to avoid quadratic left-recursion
//  *) 
// decl_block: rev_decl_block { List.rev $1 };
// rev_decl_block:
//   | (* empty *)                   { [] }
//   | rev_decl_block decl_or_assign { $2 :: $1 }
//   ;

decl_block:
  | list(decl_or_assign) { $1 }

decl_or_assign:
  | declaration { Decl($1) }
  | assignment  { Assign($1) }

declaration:
  | fn_name COLON choreo_type ARROW choreo_type         { DFun($1, $3, $5) }
  | var_name COLON choreo_type                          { DVar($1, $3) }
  | loc_name DOT var_name COLON loc_name DOT local_type { DLocVar($1, $3, $7) }
  | TYPE type_name ASSIGN choreo_type                   { DType($2, $4) }

assignment:
  | var_name ASSIGN choreo_seq              { AVar($1, $3) }
  | fn_name list(pattern) ASSIGN choreo_seq { AFun($1, $2, $4) }
  | loc_name DOT var_name ASSIGN choreo_seq { ALocVar($1, $3, $5) }

choreo_seq:
  | list(choreo_expr) { $1 }

choreo_expr:
  | LPAREN RPAREN                                                                   { Nop }
  | var_name                                                                        { Var($1) }
  | loc_name DOT local_expr                                                         { LocExpr($1, $3) }
  | loc_name DOT local_expr TILDE_ARROW loc_name DOT var_name SEMICOLON choreo_seq  { LocExprSend($1, $3, $5, $7, $9) }
  | choreo_seq TILDE_ARROW loc_name                                                 { ExprSend($1, $3) }
  | loc_name LBRACKET sync_label RBRACKET TILDE_ARROW loc_name SEMICOLON choreo_seq { LabelSend($1, $3, $6, $8) }
  | FUN fn_name ARROW choreo_seq                                                    { FunDef($2, $4) }
  | IF choreo_seq THEN choreo_seq ELSE choreo_seq                                   { If($2, $4, $6) }
  | MATCH choreo_seq WITH separated_list(VERTICAL, pattern_arrow_choreo_seq)        { Match($2, $4) }
  | LET decl_block IN choreo_seq                                                    { Let($2, $4) }
  | LPAREN choreo_seq COMMA choreo_seq RPAREN                                       { Pair($2, $4) }
  | FST choreo_seq                                                                  { Fst($2) }
  | SND choreo_seq                                                                  { Snd($2) }
  | LEFT choreo_seq                                                                 { Left($2) }
  | RIGHT choreo_seq                                                                { Right($2) }

local_expr:
  | LPAREN RPAREN                                                                  { Nop }
  | value                                                                          { Val($1) }                                                                    
  | var_name                                                                       { Var($1) }
  | local_expr bin_op local_expr                                                   { BinOp($1, $2, $3) }
  | MATCH local_expr WITH separated_list(VERTICAL, local_pattern_arrow_local_expr) { Match($2, $4) }
  | LET var_name ASSIGN local_expr IN local_expr                                   { Let($2, $4, $6) }
  | LPAREN local_expr COMMA local_expr RPAREN                                      { Pair($2, $4) }
  | FST local_expr                                                                 { Fst($2) }
  | SND local_expr                                                                 { Snd($2) }
  | LEFT local_expr                                                                { Left($2) }
  | RIGHT local_expr                                                               { Right($2) }

pattern:
  | UNDERSCORE                          { Default }
  | var_name                            { Var($1) }
  | LPAREN pattern COMMA pattern RPAREN { Pair($2, $4) }
  | loc_name DOT local_pattern          { LocPatt($1, $3) }
  | LEFT pattern                        { Left($2) }
  | RIGHT pattern                       { Right($2) }
  
local_pattern:
  | UNDERSCORE                                      { Default }
  | value                                           { Val($1) }
  | var_name                                        { Var($1) }
  | LPAREN local_pattern COMMA local_pattern RPAREN { Pair($2, $4) }
  | LEFT local_pattern                              { Left($2) }
  | RIGHT local_pattern                             { Right($2) }

choreo_type:
  | loc_name DOT local_type       { TLoc($1, $3) }
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
  
loc_name:
  | ID { LocId($1) }

var_name:
  | ID { VarId($1) }

fn_name:
  | ID { FunId($1) }

type_name:
  | ID { TypeId($1) }

sync_label:
  | ID { LabelId($1) }

%inline pattern_arrow_choreo_seq:
  | pattern ARROW choreo_seq { ($1, $3) }

%inline local_pattern_arrow_local_expr:
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