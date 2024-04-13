%{
  open Ast.Local
  open Ast.Choreo
%}

%token <string> ID
%token <int>    INT
%token <string> STRING
%token TRUE FALSE
%token UNIT_T INT_T STRING_T BOOL_T
%token FUN TYPE
%token PLUS MINUS TIMES DIV
%token AND OR
%token EQ NEQ LT LEQ GT GEQ
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA DOT COLON SEMICOLON
%token VERTICAL UNDERSCORE
%token COLONEQ ARROW TILDE_ARROW
%token LET IN
%token IF THEN ELSE
%token FST SND LEFT RIGHT
%token MATCH WITH
%token EOF

%type <Ast.Choreo.program> program
%type <Ast.Choreo.stmt_block> stmt_block
%type <Ast.Choreo.stmt> stmt
%type <Ast.Choreo.expr> choreo_expr
%type <Ast.Choreo.pattern> choreo_pattern
%type <Ast.Choreo.typ> choreo_type
%type <Ast.Local.expr> local_expr
%type <Ast.Local.pattern> local_pattern
%type <Ast.Local.typ> local_type
%type <Ast.Local.bin_op> bin_op
%type <Ast.Local.value> value
%type <Ast.Local.loc_id> loc_id
%type <Ast.Local.var_id> var_id
%type <Ast.Local.typ_id> typ_id
%type <Ast.Local.sync_label> sync_label

%nonassoc IN
%nonassoc VERTICAL
%nonassoc FST SND LEFT RIGHT
%right OR
%right AND
%left EQ NEQ LT LEQ GT GEQ
%right ARROW
%left PLUS MINUS
%left TIMES DIV
%left DOT


%start program

%%

program:
  | stmt_block EOF { Prog $1 }

stmt_block:
  | list(stmt) { $1 }
  | LPAREN list(stmt) RPAREN { $2 }

/* TODO: Removing the need for semicolons */
stmt:
  | choreo_pattern COLON choreo_type SEMICOLON        { Decl ($1, $3)}
  | choreo_pattern COLONEQ choreo_expr SEMICOLON      { Assign ($1, $3) }
  | TYPE typ_id COLONEQ choreo_type SEMICOLON?        { TypeDecl ($2, $4) }

/* Associativity increases from expr to expr3, with each precedence level falling through to the next. */
choreo_expr:
  | IF choreo_expr THEN choreo_expr ELSE choreo_expr                             { If ($2, $4, $6) }
  | LET stmt_block IN choreo_expr                                                { Let ($2, $4) }
  | FUN choreo_pattern ARROW choreo_expr                                         { FunDef ($2, $4) }
  | FST choreo_expr                                                              { Fst $2 }
  | SND choreo_expr                                                              { Snd $2 }
  | LEFT choreo_expr                                                             { Left $2 }
  | RIGHT choreo_expr                                                            { Right $2 }
  | LPAREN choreo_expr COMMA choreo_expr RPAREN                                  { Pair ($2, $4) }
  | MATCH choreo_expr WITH nonempty_list(choreo_case)                            { Match ($2, $4) }
  | loc_id LBRACKET sync_label RBRACKET TILDE_ARROW loc_id SEMICOLON choreo_expr { Sync ($1, $3, $6, $8) }
  | choreo_expr1 TILDE_ARROW loc_id DOT local_pattern SEMICOLON choreo_expr      { Let ([Assign (LocPatt ($3, $5), Send ($1, $3))], $7) }
  | choreo_expr1                                                                 { $1 }

choreo_expr1:
  | choreo_expr1 TILDE_ARROW loc_id                                              { Send ($1, $3) }
  | choreo_expr2                                                                 { $1 }

choreo_expr2:
  | choreo_expr2 choreo_expr3                                                    { FunApp ($1, $2) }
  | choreo_expr3                                                                 { $1 }

choreo_expr3:
  | LPAREN RPAREN                                                                { Unit }
  | var_id                                                                       { Var $1 }
  | loc_id DOT local_expr                                                        { LocExpr ($1, $3) }
  | LPAREN choreo_expr RPAREN                                                    { $2 }

local_expr:
  | LPAREN RPAREN                                   { Unit }
  | value                                           { Val $1 }                                                                    
  | var_id                                          { Var $1 }
  | local_expr bin_op local_expr                    { BinOp ($1, $2, $3) }
  | LET var_id EQ local_expr IN local_expr          { Let ($2, $4, $6) }
  | LPAREN local_expr COMMA local_expr RPAREN       { Pair ($2, $4) }
  | FST local_expr                                  { Fst $2 }
  | SND local_expr                                  { Snd $2 }
  | LEFT local_expr                                 { Left $2 }
  | RIGHT local_expr                                { Right $2 }
  | MATCH local_expr WITH nonempty_list(local_case) { Match ($2, $4) }
  | LPAREN local_expr RPAREN                        { $2 }

choreo_pattern:
  | UNDERSCORE                                        { Default }
  | var_id                                            { Var $1 }
  | loc_id DOT local_pattern                          { LocPatt ($1, $3) }
  | LPAREN choreo_pattern COMMA choreo_pattern RPAREN { Pair ($2, $4) }
  | LEFT choreo_pattern                               { Left $2 }
  | RIGHT choreo_pattern                              { Right $2 }
  | LPAREN choreo_pattern RPAREN                      { $2 }
  
local_pattern:
  | UNDERSCORE                                      { Default }
  | value                                           { Val $1 }
  | var_id                                          { Var $1 }
  | LPAREN local_pattern COMMA local_pattern RPAREN { Pair ($2, $4) }
  | LEFT local_pattern                              { Left $2 }
  | RIGHT local_pattern                             { Right $2 }
  | LPAREN local_pattern RPAREN                     { $2 }

choreo_type:
  | UNIT_T                        { TUnit }
  | loc_id DOT local_type         { TLoc ($1, $3) }
  | choreo_type ARROW choreo_type { TMap ($1, $3) }
  | choreo_type TIMES choreo_type { TProd ($1, $3) }
  | choreo_type PLUS choreo_type  { TSum ($1, $3) }
  | LPAREN choreo_type RPAREN     { $2 }

local_type:
  | UNIT_T                      { TUnit }
  | INT_T                       { TInt }
  | STRING_T                    { TString }
  | BOOL_T                      { TBool }
  | local_type TIMES local_type { TProd ($1, $3) }
  | local_type PLUS local_type  { TSum ($1, $3) }
  | LPAREN local_type RPAREN    { $2 }
  
loc_id:
  | ID { LocId $1 }

var_id:
  | ID { VarId $1 }

typ_id:
  | ID { TypId $1 }

sync_label:
  | ID { LabelId $1 }

value:
  | INT    { `Int $1 }
  | STRING { `String $1 }
  | TRUE   { `Bool true }
  | FALSE  { `Bool false }

%inline choreo_case:
  | VERTICAL choreo_pattern ARROW choreo_expr1 { ($2, $4) }

%inline local_case:
  | VERTICAL local_pattern ARROW local_expr    { ($2, $4) }

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