%{
    open Expr
%}

%token <string> Identifier
%token <int> Val
%token <string> Condition
%token <string> SyncLbl
%token <string> ChoreoVars
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
%token Comm_S "@>"
%token Fun "fun"
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
    | i = Identifier; {Variable i}

let mappr :=
    | n = Identifier; LSqParen; a = sub_expr; RSqParen; 
        {Map {name = n; arg = a}} 

let conditionals := 
    | lft = sub_expr; op = Condition; rght = sub_expr;
        {Condition {lft; op; rght}}
    | LParen; c = conditionals; RParen;
        {c}

let if_thn_else := 
    | If; ift = choreographies; Then; thn = choreographies; Else; el = choreographies;
        {Branch {ift; thn; el}}

let sync := 
    | sndr = Identifier; LSqParen; d = SyncLbl; RSqParen; Comm_S; rcvr = Identifier; Terminate;
        {Sync {sndr; d; rcvr}}

let value :=
    | v = Val; {Value v}

let sub_expr := 
    | value
    | eq
    | mappr
    | variable
    | conditionals

let fun_expr := 
    | Fun; name = Identifier; LParen; arg = choreographies; RParen; Assignment; body = choreographies;
        {Fun {name; arg; body}}

let application :=
    | LParen; funct = choreographies; RParen; argument = choreographies;
        {Application {funct; argument}} 
    // | funct = choreographies; argument = choreographies;
    //     {Application {funct; argument}}


let le := 
    | l = Identifier; Dot; e = sub_expr;
        {Assoc {loc = l; arg = e}}

let choreographies := 
    | if_thn_else
    | let_in
    | le
    | sync
    | choreo_vars
    | fun_expr
    | application

let let_in :=  
    | c = choreographies; Comm_S; r = Identifier; Dot; b = variable; Terminate; cp = choreographies;
        {Let {fst = Assoc {loc = r; arg = b}; snd = Snd {sndr = c; name = r}; thn= cp}}
    | Let; e = Identifier; Dot; v = variable; Assignment; c = choreographies; In; cp = choreographies;
        {Let {fst = Assoc {loc = e; arg = v}; snd = Snd {sndr = c; name = e}; thn= cp}}      

let operator :=
    | lft = sub_expr; Plus; rght = sub_expr; 
        {Plus {lft; rght}}
    | lft = sub_expr; Minus; rght = sub_expr; 
        {Minus {lft; rght}}
    | lft = sub_expr; Product; rght = sub_expr; 
        {Product {lft; rght}}
    | lft = sub_expr; Division; rght = sub_expr; 
        {Division {lft; rght}}

let eq := 
    | LParen; op = operator; RParen;
        {op}
    | op = operator; 
        {op}
 
let main := 
    | choreographies
    // move to commS implementation
    // | sr = commS; arg = expr;
    //     {Seq {fst = sr; thn = arg}}
    | sr = sync; arg = expr;
        {Seq {fst = sr; thn = arg}}

// let sub_expr :=
//     | variable
//     | LParen; e = expr; RParen; {e}

// let abstraction ==
//   | Lambda; p = Identifier; Dot; e = expr;
//     { Abstraction { param = p; body = e } }

// let application :=
//   | sub_expr
//   | f = application; a = sub_expr;
//     {Application { funct = f; argument = a }}

let expr := 
    | main
