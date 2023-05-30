%{
    open Expr
%}

%token <string> Identifier
%token <int> Val
%token <string> Operator
%token <string> Condition
%token <string> SyncLbl
%token LParen "("
%token RParen ")"
%token LSqParen "["
%token RSqParen "]"
%token Equal "="
%token If "if"
%token Else "else"
%token Then "then"
%token Comm_S "@>"
%token Function "function"
%token Assignment ":="
%token Let "let"
%token In "in"
%token Dot "."
%token Terminate ";"
%token END
%token EOF

%start <Expr.expr option> prog

%%

let line_end := END | EOF

let prog :=
    | EOF; {None}
    | e = expr; line_end; {Some e}

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
    | If; ift = choreographies; Then; thn = choreographies; Else; el = choreographies; Terminate;
        {Branch {ift; thn; el}}

let sync := 
    | sndr = Identifier; LSqParen; d = SyncLbl; RSqParen; Comm_S; rcvr = Identifier; Terminate;
        {Sync {sndr; d; rcvr}}

let sub_expr := 
    | eq
    | mappr
    | variable
    | v = Val; {Value v}
    | conditionals

let le := 
    | l = Identifier; Dot; e = sub_expr;
        {Assoc {loc = l; arg = e}}

let choreographies := 
    | if_thn_else
    | let_in
    | le
    | sync
    | variable

let let_in := 
    | c = choreographies; Comm_S; r = Identifier; Dot; vp = variable; Terminate;
        {Let {fst = Assoc {loc = r; arg = vp}; snd = Snd {sndr = c; name = r}; thn = None}}  
    | c = choreographies; Comm_S; r = Identifier; Dot; b = variable; Terminate; cp = choreographies;
        {Let {fst = Assoc {loc = r; arg = b}; snd = Snd {sndr = c; name = r}; thn= cp}}
    | Let; e = Identifier; Dot; v = variable; Assignment; c = choreographies; In; cp = choreographies;
        {Let {fst = Assoc {loc = e; arg = v}; snd = Snd {sndr = c; name = e}; thn= cp}}      

let eq := 
    | LParen; lft = sub_expr; op = Operator; rght = sub_expr; RParen;
        {Op {lft; op; rght}}
    | lft = sub_expr; op = Operator; rght = sub_expr;
        {Op {lft; op; rght}}

let commS :=
    | s = Identifier; Dot; msg = sub_expr; Comm_S; r = Identifier; Dot; b = variable;
        {Comm_S {sndr = Assoc {loc = s; arg = msg}; rcvr = Assoc {loc = r; arg = b}}}
    | s = Identifier; Dot; msg = sub_expr; Comm_S; r = Identifier; Dot; b = variable; Terminate;
        {Comm_S {sndr = Assoc {loc = s; arg = msg}; rcvr = Assoc {loc = r; arg = b}}}
 
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
