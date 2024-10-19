%{
  open Ast.Local
  open Ast.Choreo
%}

(** Token Definitions:
    - Defines the types and categories of tokens used by the parser.
    - Each token can carry specific data types, such as strings, integers.
*)
%token <string * (string * int * int * int)> ID
%token <int * (string * int * int * int)>    INT
%token <string * (string * int * int * int)> STRING
%token <string * int * int * int> TRUE FALSE
%token <string * int * int * int> UNIT_T INT_T STRING_T BOOL_T
%token <string * int * int * int> FUN TYPE
%token <string * int * int * int> PLUS MINUS TIMES DIV
%token <string * int * int * int> NOT
%token <string * int * int * int> AND OR
%token <string * int * int * int> EQ NEQ LT LEQ GT GEQ
%token <string * int * int * int> LPAREN RPAREN LBRACKET RBRACKET
%token <string * int * int * int> COMMA DOT COLON SEMICOLON
%token <string * int * int * int> VERTICAL UNDERSCORE
%token COLONEQ ARROW TILDE_ARROW
%token <string * int * int * int> LET IN
%token <string * int * int * int> IF THEN ELSE
%token <string * int * int * int> FST SND LEFT RIGHT
%token <string * int * int * int> MATCH WITH
%token <string * int * int * int> EOF

(** Type Declarations:
    - Specifies the types of the non-terminal symbols used in the grammar.
    - These types correspond to the data structures defined in the AST modules.
*)
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

(** Operator Precedence and Associativity:
    - Defines the precedence and associativity rules for operators to resolve ambiguities in expressions.
*)
%nonassoc IN
%nonassoc VERTICAL
%nonassoc FST SND LEFT RIGHT
%right OR
%right AND
%left EQ NEQ LT LEQ GT GEQ
%right ARROW
%left PLUS MINUS
%left TIMES DIV
%nonassoc UNARY
%left DOT


%start program

%%

(** [program] parses a stmt_block aka list of statements and add EOF at the end of code block.*)
program:
  | stmt_block EOF { Prog ($1, $2) } /* The filename is stored in EOF*/

(** [stmt_block] parses and returns the list of statements. *)
stmt_block:
  | list(stmt) { $1 }

(** [stmt] parses statements within a choreography and constructs corresponding AST nodes.

    - Returns: An AST node representing the statement.
    - Example: Parsing a pattern, a colon, a choreography type, and a semicolon results in a `Decl` node with the pattern, type, and metadata.*)
/* TODO: Removing the need for semicolons */
stmt:
  | choreo_pattern COLON choreo_type SEMICOLON                   { Decl ($1, $3, metainfo_of_ChorPatt $1)}
  | nonempty_list(choreo_pattern) COLONEQ choreo_expr SEMICOLON  { Assign ($1, $3, metainfo_of_ChorPatt_list $1) }
  | TYPE typ_id COLONEQ choreo_type SEMICOLON?                   { TypeDecl ($2, $4, $1) }

/* Associativity increases from expr to expr3, with each precedence level falling through to the next. */
choreo_expr:
  | IF choreo_expr THEN choreo_expr ELSE choreo_expr                                                 { If ($2, $4, $6, $1) }
  | LET stmt_block IN choreo_expr                                                                    { Let ($2, $4, $1) }
  | FUN nonempty_list(choreo_pattern) ARROW choreo_expr                                              { FunDef ($2, $4, $1) }
  | FST choreo_expr                                                                                  { Fst ($2, $1) }
  | SND choreo_expr                                                                                  { Snd ($2, $1) }
  | LEFT choreo_expr                                                                                 { Left ($2, $1) }
  | RIGHT choreo_expr                                                                                { Right ($2, $1) }
  | MATCH choreo_expr WITH nonempty_list(choreo_case)                                                { Match ($2, $4, $1) }
  | loc_id LBRACKET sync_label RBRACKET TILDE_ARROW loc_id SEMICOLON choreo_expr                     { Sync ($1, $3, $6, $8, metainfo_of_LocId $1) }
  | LBRACKET loc_id RBRACKET choreo_expr TILDE_ARROW loc_id                                          { Send ($2, $4, $6, $1) }
  | LBRACKET loc_id RBRACKET choreo_expr TILDE_ARROW loc_id DOT local_pattern SEMICOLON choreo_expr  { Let ([Assign ([LocPatt ($6, $8, $1)], Send ($2, $4, $6, $1), $1)], $10, $1) } //??? i added the $1 already
  | choreo_expr1                                                                                     { $1 }

choreo_expr1:
  | choreo_expr1 choreo_expr2                                                                        { FunApp ($1, $2, metainfo_of_ChorExpr $1) }
  | choreo_expr2 COMMA choreo_expr2                                                                  { Pair ($1, $3, metainfo_of_ChorExpr $1) }
  | choreo_expr2                                                                                     { $1 }

choreo_expr2:
  | LPAREN RPAREN                                                                                    { Unit $1 }
  | var_id                                                                                           { Var ($1, metainfo_of_VarId $1) }
  | loc_id DOT local_expr                                                                            { LocExpr ($1, $3, metainfo_of_LocId $1) }
  | LPAREN choreo_expr RPAREN                                                                        { $2 }

(** [local_expr] parses local expressions and constructs corresponding AST nodes.

    - Returns: An AST node representing the local expression.*)
local_expr:
  | LPAREN RPAREN                                    { Unit $1 }
  | value                                            { Val ($1, metainfo_of_Val $1) }                                                                    
  | var_id                                           { Var ($1, metainfo_of_VarId $1) }
  | un_op local_expr %prec UNARY                     { UnOp ($1, $2, metainfo_of_UnOp $1) } // ???
  | local_expr bin_op local_expr                     { BinOp ($1, $2, $3, metainfo_of_LocExpr $1) }
  // | LET var_id COLONEQ local_expr IN local_expr      { Let ($2, $4, $6, $1) }
  | LET var_id COLON local_type COLONEQ local_expr IN local_expr      { Let ($2, $4, $6, $8, $1) }
  | LPAREN local_expr COMMA local_expr RPAREN        { Pair ($2, $4, $1) }
  | FST local_expr                                   { Fst ($2, $1) }
  | SND local_expr                                   { Snd ($2, $1) }
  | LEFT local_expr                                  { Left ($2, $1) }
  | RIGHT local_expr                                 { Right ($2, $1) }
  | MATCH local_expr WITH nonempty_list(local_case)  { Match ($2, $4, $1) }
  | LPAREN local_expr RPAREN                         { $2 }

(** [choreo_pattern] parses patterns used in choreography expressions and constructs corresponding AST nodes.*)
choreo_pattern:
  | UNDERSCORE                                         { Default $1 }
  | var_id                                             { Var ($1, metainfo_of_VarId $1) }
  | loc_id DOT local_pattern                           { LocPatt ($1, $3, metainfo_of_LocId $1) }
  | LPAREN choreo_pattern COMMA choreo_pattern RPAREN  { Pair ($2, $4, $1) }
  | LEFT choreo_pattern                                { Left ($2, $1) }
  | RIGHT choreo_pattern                               { Right ($2, $1) }
  | LPAREN choreo_pattern RPAREN                       { $2 }
  
  (** [local_pattern] parses patterns used in local expressions within choreographies and constructs corresponding AST nodes.*)
local_pattern:
  | UNDERSCORE                                      { Default $1 }
  | value                                           { Val ($1, metainfo_of_Val $1) }
  | var_id                                          { Var ($1, metainfo_of_VarId $1) }
  | LPAREN local_pattern COMMA local_pattern RPAREN { Pair ($2, $4, $1) }
  | LEFT local_pattern                              { Left ($2, $1) }
  | RIGHT local_pattern                             { Right ($2, $1) }
  | LPAREN local_pattern RPAREN                     { $2 }

(** [choreo_type] parses choreography types and constructs corresponding AST nodes.

    - Returns: An AST node representing the choreography type.
*)
choreo_type:
  | UNIT_T                         { TUnit $1 }
  | loc_id DOT local_type          { TLoc ($1, $3, metainfo_of_LocId $1) }
  | choreo_type ARROW choreo_type  { TMap ($1, $3, metainfo_of_ChorTyp $1) }
  | choreo_type TIMES choreo_type  { TProd ($1, $3, metainfo_of_ChorTyp $1) }
  | choreo_type PLUS choreo_type   { TSum ($1, $3, metainfo_of_ChorTyp $1) }
  | LPAREN choreo_type RPAREN      { $2 } 

(** [local_type] parses local types and constructs corresponding AST nodes.

    - Returns: An AST node representing the local type.
*) 
local_type:
  | UNIT_T                      { TUnit $1 }
  | INT_T                       { TInt $1 }
  | STRING_T                    { TString $1 }
  | BOOL_T                      { TBool $1 }
  | local_type TIMES local_type { TProd ($1, $3, metainfo_of_LocTyp $1) }
  | local_type PLUS local_type  { TSum ($1, $3, metainfo_of_LocTyp $1 ) }
  | LPAREN local_type RPAREN     { $2 }

(** [loc_id] parses an identifier for a location and constructs a corresponding AST node.

    - Returns: A `LocId` node containing the identifier and its metadata.
    - Example: Parsing an `ID` token results in a `LocId` node with the identifier and associated metadata.
*)
loc_id:
  | ID { let (id, metainfo) = $1 in LocId (id, metainfo) }

(** [var_id] parses an identifier for a variable and constructs a corresponding AST node.

    - Returns: A `VarId` node containing the identifier and its metadata.
    - Example: Parsing an `ID` token results in a `VarId` node with the identifier and associated metadata.
*)
var_id:
  | ID { let (id, metainfo) = $1 in VarId (id, metainfo) }

typ_id:
  | ID { let (id, metainfo) = $1 in TypId (id, metainfo) }

sync_label:
  | ID { let (id, metainfo) = $1 in LabelId (id, metainfo) }

value:
  | INT    { let (i, metainfo) = $1 in Int (i, metainfo) }
  | STRING { let (s, metainfo) = $1 in String (s, metainfo) }
  | TRUE   { Bool (true, $1) }
  | FALSE  { Bool (false, $1) }

(** [choreo_case] parses case expressions for choreography expressions and constructs corresponding AST nodes.

    Each case is defined by a pattern and an expression, separated by an arrow.

    - Returns: A tuple containing the parsed pattern and the corresponding choreography expression.
    - Example: Parsing a vertical bar, a pattern, an arrow, and a choreography expression results in a tuple of the pattern and expression.
*)
%inline choreo_case:
  | VERTICAL choreo_pattern ARROW choreo_expr { ($2, $4) }

(** [local_case] parses case expressions for local expressions and constructs corresponding AST nodes.

    Similar to [case], but specifically for local expressions within a choreography.

    - Returns: A tuple containing the parsed local pattern and the corresponding local expression.
    - Example: Parsing a vertical bar, a local pattern, an arrow, and a local expression results in a tuple of the local pattern and expression.
*)
%inline local_case:
  | VERTICAL local_pattern ARROW local_expr    { ($2, $4) }

%inline un_op:
  | MINUS { Neg ($1) }
  | NOT   { Not ($1) }

(** [bin_op] parses binary operators and constructs corresponding AST nodes.

    Each operator is associated with a specific constructor that takes location information as an argument.

    - Returns: An AST node representing the binary operation.
    - Example: Parsing the token PLUS with location info at position 1 results in [Plus ($1)].
*)
%inline bin_op:
  | PLUS  { Plus ($1) }
  | MINUS { Minus ($1) }
  | TIMES { Times ($1) }
  | DIV   { Div ($1) }
  | AND   { And ($1) }
  | OR    { Or ($1) }
  | EQ    { Eq ($1) }
  | NEQ   { Neq ($1) }
  | LT    { Lt ($1) }
  | LEQ   { Leq ($1) }
  | GT    { Gt ($1) }
  | GEQ   { Geq ($1) }
