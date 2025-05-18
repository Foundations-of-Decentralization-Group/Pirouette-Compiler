%token <string> ID
%token <int> INT
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

%{
  open Ast_core.Local.M
  open Ast_core.Choreo.M
  open Parsed_ast

  let gen_pos startpos endpos =
    let open Lexing in
    { Pos_info.fname = startpos.pos_fname
    ; start = startpos.pos_lnum, startpos.pos_cnum - startpos.pos_bol
    ; stop = endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol
    }
%}

%type <Parsed_ast.Choreo.stmt_block> prog
%type <Parsed_ast.Choreo.stmt_block> stmt_block
%type <Parsed_ast.Choreo.stmt> stmt
%type <Parsed_ast.Choreo.expr> choreo_expr
%type <Parsed_ast.Choreo.pattern> choreo_pattern
%type <Parsed_ast.Choreo.typ> choreo_type
%type <Parsed_ast.Local.expr> local_expr
%type <Parsed_ast.Local.pattern> local_pattern
%type <Parsed_ast.Local.typ> local_type
%type <Parsed_ast.Local.bin_op> bin_op
%type <Parsed_ast.Local.value> value
%type <Parsed_ast.Local.loc_id> loc_id
%type <Parsed_ast.Local.var_id> var_id
%type <Parsed_ast.Local.typ_id> typ_id
%type <Parsed_ast.Local.sync_label> sync_label

%start prog

%%

prog:
  | stmt_block EOF { $1 }

stmt_block:
  | list(stmt) { $1 }

/* TODO: Removing the need for semicolons */
stmt:
  | p=choreo_pattern COLON t=choreo_type SEMICOLON { print_endline "p statement" ; Decl (p, t, gen_pos $startpos $endpos) }
  | ps=nonempty_list(choreo_pattern) COLONEQ e=choreo_expr SEMICOLON { print_endline "ps statement" ; Assign (ps, e, gen_pos $startpos $endpos) } 
  | TYPE id=typ_id COLONEQ t=choreo_type SEMICOLON? { print_endline "type decl statement" ; TypeDecl (id, t, gen_pos $startpos $endpos) }

/* Associativity increases from expr to expr3, with each precedence level falling through to the next. */
choreo_expr:
  | IF e1=choreo_expr THEN e2=choreo_expr ELSE e3=choreo_expr { If (e1, e2, e3, gen_pos $startpos $endpos) }
  | LET stmts=stmt_block IN e=choreo_expr { Let (stmts, e, gen_pos $startpos $endpos) }
  | FUN ps=nonempty_list(choreo_pattern) ARROW e=choreo_expr { FunDef (ps, e, gen_pos $startpos $endpos) }
  | FST e=choreo_expr { Fst (e, gen_pos $startpos $endpos) }
  | SND e=choreo_expr { Snd (e, gen_pos $startpos $endpos) }
  | LEFT e=choreo_expr { Left (e, gen_pos $startpos $endpos) }
  | RIGHT e=choreo_expr { Right (e, gen_pos $startpos $endpos) }
  | MATCH e=choreo_expr WITH cases=nonempty_list(choreo_case) { Match (e, cases, gen_pos $startpos $endpos) }
  | id1=loc_id LBRACKET l=sync_label RBRACKET TILDE_ARROW id2=loc_id SEMICOLON e=choreo_expr { Sync (id1, l, id2, e, gen_pos $startpos $endpos) }
  | LBRACKET id1=loc_id RBRACKET e=choreo_expr TILDE_ARROW id2=loc_id { Send (id1, e, id2, gen_pos $startpos $endpos) }
  | LBRACKET id1=loc_id RBRACKET e1=choreo_expr TILDE_ARROW id2=loc_id DOT p=local_pattern SEMICOLON e2=choreo_expr
      { Let ([Assign ([LocPat (id2, p, gen_pos $startpos(id2) $endpos(p))], Send (id1, e1, id2, gen_pos $startpos($1) $endpos(id2)), gen_pos $startpos($1) $endpos(p))], e2, gen_pos $startpos $endpos) }
  | e1=choreo_expr1 COMMA e2=choreo_expr1 { Pair (e1, e2, gen_pos $startpos $endpos) }
  | choreo_expr1 { $1 }

choreo_expr1:
  | e1=choreo_expr1 e2=choreo_expr2 { FunApp (e1, e2, gen_pos $startpos $endpos) }
  | choreo_expr2 { $1 }

choreo_expr2:
  | LPAREN RPAREN { Unit (gen_pos $startpos $endpos) }
  | id=var_id { Var (id, gen_pos $startpos $endpos) }
  | id=loc_id DOT e=local_expr { LocExpr (id, e, gen_pos $startpos $endpos) }
  | LPAREN e=choreo_expr RPAREN { Choreo.set_info_expr (gen_pos $startpos $endpos) e }

local_expr:
  | LPAREN RPAREN { Unit (gen_pos $startpos $endpos) }
  | v=value { Val (v, gen_pos $startpos $endpos) }
  | id=var_id { Var (id, gen_pos $startpos $endpos) }
  | op=un_op e=local_expr %prec UNARY { UnOp (op, e, gen_pos $startpos $endpos) }
  | e1=local_expr op=bin_op e2=local_expr { BinOp (e1, op, e2, gen_pos $startpos $endpos) }
  | LET id=var_id COLONEQ e1=local_expr IN e2=local_expr { Let (id, e1, e2, gen_pos $startpos $endpos) }
  | LPAREN e1=local_expr COMMA e2=local_expr RPAREN { Pair (e1, e2, gen_pos $startpos $endpos) }
  | FST e=local_expr { Fst (e, gen_pos $startpos $endpos) }
  | SND e=local_expr { Snd (e, gen_pos $startpos $endpos) }
  | LEFT e=local_expr { Left (e, gen_pos $startpos $endpos) }
  | RIGHT e=local_expr { Right (e, gen_pos $startpos $endpos) }
  | MATCH e=local_expr WITH cases=nonempty_list(local_case) { Match (e, cases, gen_pos $startpos $endpos) }
  | LPAREN e=local_expr RPAREN { Local.set_info_expr (gen_pos $startpos $endpos) e }

choreo_pattern:
  | UNDERSCORE { Default (gen_pos $startpos $endpos) }
  | id=var_id { Var (id, gen_pos $startpos $endpos) }
  | id=loc_id DOT p=local_pattern { LocPat (id, p, gen_pos $startpos $endpos) }
  | LPAREN p1=choreo_pattern COMMA p2=choreo_pattern RPAREN { Pair (p1, p2, gen_pos $startpos $endpos) }
  | LEFT p=choreo_pattern { Left (p, gen_pos $startpos $endpos) }
  | RIGHT p=choreo_pattern { Right (p, gen_pos $startpos $endpos) }
  | LPAREN p=choreo_pattern RPAREN { Choreo.set_info_pattern (gen_pos $startpos $endpos) p }
  
local_pattern:
  | UNDERSCORE { Default (gen_pos $startpos $endpos) }
  | v=value { Val (v, gen_pos $startpos $endpos) }
  | x=var_id { Var (x, gen_pos $startpos $endpos) }
  | LPAREN p1=local_pattern COMMA p2=local_pattern RPAREN { Pair (p1, p2, gen_pos $startpos $endpos) }
  | LEFT p=local_pattern { Left (p, gen_pos $startpos $endpos) }
  | RIGHT p=local_pattern { Right (p, gen_pos $startpos $endpos) }
  | LPAREN p=local_pattern RPAREN { Local.set_info_pattern (gen_pos $startpos $endpos) p }

choreo_type:
  | UNIT_T { TUnit (gen_pos $startpos $endpos) }
  | id=loc_id DOT t=local_type { TLoc (id, t, gen_pos $startpos $endpos) }
  | t1=choreo_type ARROW t2=choreo_type { TMap (t1, t2, gen_pos $startpos $endpos) }
  | t1=choreo_type TIMES t2=choreo_type { TProd (t1, t2, gen_pos $startpos $endpos) }
  | t1=choreo_type PLUS t2=choreo_type { TSum (t1, t2, gen_pos $startpos $endpos) }
  | LPAREN t=choreo_type RPAREN { Choreo.set_info_typ (gen_pos $startpos $endpos) t }

local_type:
  | UNIT_T { TUnit (gen_pos $startpos $endpos) }
  | INT_T { TInt (gen_pos $startpos $endpos) }
  | STRING_T { TString (gen_pos $startpos $endpos) }
  | BOOL_T { TBool (gen_pos $startpos $endpos) }
  | t1=local_type TIMES t2=local_type { TProd (t1, t2, gen_pos $startpos $endpos) }
  | t1=local_type PLUS t2=local_type { TSum (t1, t2, gen_pos $startpos $endpos) }
  | LPAREN t=local_type RPAREN { Local.set_info_typ (gen_pos $startpos $endpos) t }
  
loc_id:
  | id=ID { LocId (id, gen_pos $startpos $endpos) }

var_id:
  | id=ID { VarId (id, gen_pos $startpos $endpos) }

typ_id:
  | id=ID { TypId (id, gen_pos $startpos $endpos) }

sync_label:
  | id=ID { LabelId (id, gen_pos $startpos $endpos) }

value:
  | i=INT { Int (i, gen_pos $startpos $endpos) }
  | s=STRING { String (s, gen_pos $startpos $endpos) }
  | TRUE { Bool (true, gen_pos $startpos $endpos) }
  | FALSE { Bool (false, gen_pos $startpos $endpos) }

%inline choreo_case:
  | BAR p=choreo_pattern ARROW e=choreo_expr { p, e }

%inline local_case:
  | BAR p=local_pattern ARROW e=local_expr { p, e }

%inline un_op:
  | MINUS { Neg (gen_pos $startpos $endpos) }
  | NOT { Not (gen_pos $startpos $endpos) }

%inline bin_op:
  | PLUS { Plus (gen_pos $startpos $endpos) }
  | MINUS { Minus (gen_pos $startpos $endpos) }
  | TIMES { Times (gen_pos $startpos $endpos) }
  | DIV { Div (gen_pos $startpos $endpos) }
  | AND { And (gen_pos $startpos $endpos) }
  | OR { Or (gen_pos $startpos $endpos) }
  | EQ { Eq (gen_pos $startpos $endpos) }
  | NEQ { Neq (gen_pos $startpos $endpos) }
  | LT { Lt (gen_pos $startpos $endpos) }
  | LEQ { Leq (gen_pos $startpos $endpos) }
  | GT { Gt (gen_pos $startpos $endpos) }
  | GEQ { Geq (gen_pos $startpos $endpos) }
