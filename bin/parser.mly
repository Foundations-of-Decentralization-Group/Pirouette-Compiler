%{
    open Expr
%}

%token <string> Identifier
%token <int> Val
%token <string> Operator
%token <string> Condition
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

let ifbranch := 
    | conditionals
    | i = Identifier; Dot; LParen; c = conditionals; RParen;
        {Assoc {loc = i; arg = c}}

let if_thn_else := 
    | If; ift = ifbranch; Then; thn = sub_expr; Else; el = sub_expr; Terminate;
        {Branch {ift; thn; el}}

let sub_expr := 
    | eq
    | mappr
    | variable
    | if_thn_else
    | v = Val; {Value v}

let eq := 
    | LParen; lft = sub_expr; op = Operator; rght = sub_expr; RParen;
        {Op {lft; op; rght}}

let commS :=
    | s = Identifier; Dot; msg = sub_expr; Comm_S; r = Identifier; Dot; b = variable; Terminate;
        {Comm_S {sndr = Assoc {loc = s; arg = msg}; rcvr = Assoc {loc = r; arg = b}}}
 
let main := 
    | commS
    | if_thn_else
    | sr = commS; arg = expr;
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
