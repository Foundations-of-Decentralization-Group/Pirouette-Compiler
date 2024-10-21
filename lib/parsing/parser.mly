%{
  open Ast_core.Local.M
  open Ast_core.Choreo.M
%}

%token <string> ID
%token <int>    INT
%token <string> STRING
%token TRUE FALSE
%token UNIT_T INT_T STRING_T BOOL_T
%token FUN TYPE
%token UNDERSCORE
%token COLONEQ
%token PLUS MINUS TIMES DIV
%token NOT
%token AND OR
%token EQ NEQ LT LEQ GT GEQ
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA DOT COLON SEMICOLON
%token ARROW TILDE_ARROW
%token BAR
%token LET IN
%token IF THEN ELSE
%token FST SND LEFT RIGHT
%token MATCH WITH
%token EOF

%type <Ast_core.Choreo.M.stmt_block> stmt_block
%type <Ast_core.Choreo.M.stmt> stmt
%type <Ast_core.Choreo.M.expr> choreo_expr
%type <Ast_core.Choreo.M.pattern> choreo_pattern
%type <Ast_core.Choreo.M.typ> choreo_type
%type <Ast_core.Local.M.expr> local_expr
%type <Ast_core.Local.M.pattern> local_pattern
%type <Ast_core.Local.M.typ> local_type
%type <Ast_core.Local.M.bin_op> bin_op
%type <Ast_core.Local.M.value> value
%type <Ast_core.Local.M.loc_id> loc_id
%type <Ast_core.Local.M.var_id> var_id
%type <Ast_core.Local.M.typ_id> typ_id
%type <Ast_core.Local.M.sync_label> sync_label
%type <stmt_block> program

%nonassoc IN
%right ARROW
%nonassoc BAR
%nonassoc FST SND LEFT RIGHT
%right OR
%right AND
%left EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV
%nonassoc UNARY
%left DOT


%start program

%%

program:
  | stmt_block EOF { $1 }

stmt_block:
  | list(stmt) { $1 }

/* TODO: Removing the need for semicolons */
stmt:
  | choreo_pattern COLON choreo_type SEMICOLON                   { Decl ($1, $3)}
  | nonempty_list(choreo_pattern) COLONEQ choreo_expr SEMICOLON  { Assign ($1, $3) }
  | TYPE typ_id COLONEQ choreo_type SEMICOLON?                   { TypeDecl ($2, $4) }

/* Associativity increases from expr to expr3, with each precedence level falling through to the next. */
choreo_expr:
  | IF choreo_expr THEN choreo_expr ELSE choreo_expr                                                 { If ($2, $4, $6) }
  | LET stmt_block IN choreo_expr                                                                    { Let ($2, $4) }
  | FUN nonempty_list(choreo_pattern) ARROW choreo_expr                                              { FunDef ($2, $4) }
  | FST choreo_expr                                                                                  { Fst $2 }
  | SND choreo_expr                                                                                  { Snd $2 }
  | LEFT choreo_expr                                                                                 { Left $2 }
  | RIGHT choreo_expr                                                                                { Right $2 }
  | MATCH choreo_expr WITH nonempty_list(choreo_case)                                                { Match ($2, $4) }
  | loc_id LBRACKET sync_label RBRACKET TILDE_ARROW loc_id SEMICOLON choreo_expr                     { Sync ($1, $3, $6, $8) }
  | LBRACKET loc_id RBRACKET choreo_expr TILDE_ARROW loc_id                                          { Send ($2, $4, $6) }
  | LBRACKET loc_id RBRACKET choreo_expr TILDE_ARROW loc_id DOT local_pattern SEMICOLON choreo_expr  { Let ([Assign ([LocPatt ($6, $8)], Send ($2, $4, $6))], $10) }
  | choreo_expr1                                                                                     { $1 }

choreo_expr1:
  | choreo_expr1 choreo_expr2                                                                        { FunApp ($1, $2) }
  | choreo_expr2 COMMA choreo_expr2                                                                  { Pair ($1, $3) }
  | choreo_expr2                                                                                     { $1 }

choreo_expr2:
  | LPAREN RPAREN                                                                                    { Unit }
  | var_id                                                                                           { Var $1 }
  | loc_id DOT local_expr                                                                            { LocExpr ($1, $3) }
  | LPAREN choreo_expr RPAREN                                                                        { $2 }

local_expr:
  | LPAREN RPAREN                                    { Unit }
  | value                                            { Val $1 }                                                                    
  | var_id                                           { Var $1 }
  | un_op local_expr %prec UNARY                     { UnOp ($1, $2) }
  | local_expr bin_op local_expr                     { BinOp ($1, $2, $3) }
  | LET var_id COLONEQ local_expr IN local_expr      { Let ($2, $4, $6) }
  | LPAREN local_expr COMMA local_expr RPAREN        { Pair ($2, $4) }
  | FST local_expr                                   { Fst $2 }
  | SND local_expr                                   { Snd $2 }
  | LEFT local_expr                                  { Left $2 }
  | RIGHT local_expr                                 { Right $2 }
  | MATCH local_expr WITH nonempty_list(local_case)  { Match ($2, $4) }
  | LPAREN local_expr RPAREN                         { $2 }

choreo_pattern:
  | UNDERSCORE                                         { Default }
  | var_id                                             { Var $1 }
  | loc_id DOT local_pattern                           { LocPatt ($1, $3) }
  | LPAREN choreo_pattern COMMA choreo_pattern RPAREN  { Pair ($2, $4) }
  | LEFT choreo_pattern                                { Left $2 }
  | RIGHT choreo_pattern                               { Right $2 }
  | LPAREN choreo_pattern RPAREN                       { $2 }
  
local_pattern:
  | UNDERSCORE                                       { Default }
  | value                                            { Val $1 }
  | var_id                                           { Var $1 }
  | LPAREN local_pattern COMMA local_pattern RPAREN  { Pair ($2, $4) }
  | LEFT local_pattern                               { Left $2 }
  | RIGHT local_pattern                              { Right $2 }
  | LPAREN local_pattern RPAREN                      { $2 }

choreo_type:
  | UNIT_T                         { TUnit }
  | loc_id DOT local_type          { TLoc ($1, $3) }
  | choreo_type ARROW choreo_type  { TMap ($1, $3) }
  | choreo_type TIMES choreo_type  { TProd ($1, $3) }
  | choreo_type PLUS choreo_type   { TSum ($1, $3) }
  | LPAREN choreo_type RPAREN      { $2 }

local_type:
  | UNIT_T                       { TUnit }
  | INT_T                        { TInt }
  | STRING_T                     { TString }
  | BOOL_T                       { TBool }
  | local_type TIMES local_type  { TProd ($1, $3) }
  | local_type PLUS local_type   { TSum ($1, $3) }
  | LPAREN local_type RPAREN     { $2 }
  
loc_id:
  | ID { LocId $1 }

var_id:
  | ID { VarId $1 }

typ_id:
  | ID { TypId $1 }

sync_label:
  | ID { LabelId $1 }

value:
  | INT    { Int $1 }
  | STRING { String $1 }
  | TRUE   { Bool true }
  | FALSE  { Bool false }

%inline choreo_case:
  | BAR choreo_pattern ARROW choreo_expr { ($2, $4) }

%inline local_case:
  | BAR local_pattern ARROW local_expr    { ($2, $4) }

%inline un_op:
  | MINUS { Neg }
  | NOT   { Not }

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
