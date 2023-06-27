%{
    open Expr
%}

%token <string> Identifier
%token <string> Condition
%token <string> SyncLbl
%token <string> ChoreoVars
%token <string> Type
%token <int> INT
%token <string> STRING
%token <string> BOOL
%token Plus "+"
%token Minus "-"
%token Product "*"
%token Division "/"
%token LParen "("
%token RParen ")"
%token LSqParen "["
%token RSqParen "]"
%token If "if"
%token Else "else"
%token Then "then"
%token Comm_S "~>"
%token Fun "fun"
%token Colon ":"
%token Assignment ":="
%token Let "let"
%token In "in"
%token Dot "."
%token Terminate ";"
%token END
%token EOF

//Precedence Rules (Lower -> Higher)
%nonassoc In
%nonassoc Let
%nonassoc Fun
%nonassoc Then 
%nonassoc Else
%nonassoc Terminate
%right Assignment
%nonassoc Comm_S
%nonassoc Dot
%nonassoc Colon
%nonassoc Condition
%left Plus Minus
%left Product Division
%nonassoc LParen RParen LSqParen RSqParen

%start <Expr.expr option> prog

%%

let line_end := END | EOF

let prog :=
    | EOF; {None}
    | e = expr; line_end; {Some e}

let choreo_vars := 
    | i = ChoreoVars; {ChoreoVars i}

let variable := 
    | name = Identifier; Colon; typ = Type; {Variable {name; typ = Some typ}}
    | name = Identifier; {Variable {name; typ = None}}

//to be removed upon confirmation
let mappr :=
    | n = Identifier; LSqParen; a = sub_expr; RSqParen; 
        {Map {name = n; arg = a; typ = None}} 

let conditionals := 
    | lft = sub_expr; op = Condition; rght = sub_expr;
        {Condition {lft; op; rght; typ = Some "bool"}}
    | LParen; c = conditionals; RParen;
        {c}

let if_thn_else := 
    | If; ift = choreographies; Then; thn = choreographies; Else; el = choreographies;
        {Branch {ift; thn; el; typ = None}}

let sync := 
    | sndr = Identifier; LSqParen; d = SyncLbl; RSqParen; Comm_S; rcvr = Identifier; Terminate; thn = choreographies;
        {Sync {sndr; d; rcvr; thn; typ = None}}

let integer :=
    | v = INT; {INT v}

let string :=
    | v = STRING; {STRING v}

let bool :=
    | v = BOOL; {BOOL (bool_of_string v)}

let sub_expr := 
    | integer
    | string
    | bool
    | eq
    | mappr
    | variable
    | conditionals

let fun_expr := 
    | Fun; name = Identifier; LParen; arg = choreographies; RParen; Colon; typ = Type; Assignment; body = choreographies;
            {Fun {name; arg; body; typ = Some typ}}
    | Fun; name = Identifier; LParen; arg = choreographies; RParen; Assignment; body = choreographies;
        {Fun {name; arg; body; typ = None}}

let application :=
    | LParen; funct = choreographies; RParen; argument = choreographies;
        {Application {funct; argument; typ = None}} 

let calling :=
    | name = Identifier; arg = le;
        {Calling {name; arg; typ = None}}

let le := 
    | l = Identifier; Dot; e = sub_expr;
        {Assoc {loc = l; arg = e; typ = None}}

let choreographies := 
    | if_thn_else
    | let_in
    | le
    | sync
    | choreo_vars
    | fun_expr
    | application
    | calling

let let_in :=  
    // | e = le; Comm_S; r = Identifier; Dot; b = variable; Terminate; cp = choreographies;
    //     {Let {fst = Assoc {loc = r; arg = b}; snd = Snd {sndr = e; name = r}; thn= cp}}
    | c = choreographies; Comm_S; r = Identifier; Dot; b = variable; Terminate; cp = choreographies;
        {Let {fst = Assoc {loc = r; arg = b; typ = None}; snd = Snd {sndr = c; name = r; typ = None}; thn= cp; typ = None}}
    | Let; e = Identifier; Dot; v = variable; Assignment; r = Identifier; Dot; b = variable; Comm_S; 
        Identifier; Terminate; In; cp = choreographies;
        {Let {fst = Assoc {loc = e; arg = v; typ = None}; snd = Snd {sndr = Assoc{loc = r; arg = b; typ = None}; name = e; typ = None}; thn= cp; typ = None}}   
    | Let; e = Identifier; Dot; v = variable; Assignment; c = choreographies; In; cp = choreographies;
        {Let {fst = Assoc {loc = e; arg = v; typ = None}; snd = c; thn= cp; typ = None}}  

let operator :=
    | lft = sub_expr; Plus; rght = sub_expr; 
        {Plus {lft; rght; typ = Some "int"}}
    | lft = sub_expr; Minus; rght = sub_expr; 
        {Minus {lft; rght; typ = Some "int"}}
    | lft = sub_expr; Product; rght = sub_expr; 
        {Product {lft; rght; typ = Some "int"}}
    | lft = sub_expr; Division; rght = sub_expr; 
        {Division {lft; rght; typ = Some "int"}}

let eq := 
    | LParen; op = operator; RParen;
        {op}
    | op = operator; 
        {op}

let expr := 
    // | conditionals
    | choreographies
