%{
    open Expr
%}

%token <string> Identifier
%token <int> Val
%token LParen "("
%token RParen ")"
%token LSqParen "["
%token RSqParen "]"
%token LessThan "<"
%token GreaterThan ">"
%token Equal "="
%token If "if"
%token Else "else"
%token Then "then"
%token Comm_S "@>"
%token Function "function"
%token Assignment ":="
%token Divide "/"
%token Multiply "*"
%token Add "+"
%token Subtract "-"
%token Let "let"
%token In "in"
%token Dot "."
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
    | n = Identifier; LSqParen; a = value; RSqParen; 
        {Map {name = n; arg = a}} 

let value := 
    | mappr
    | variable
    | v = Val; {Value v}

let commS :=
    | s = Identifier; Dot; msg = value; Comm_S; r = Identifier; Dot; b = variable; 
        {Comm_S {sndr = Sndr {name = s; arg = msg}; rcvr = Rcvr {name = r; arg = b}}}
 
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
    | commS
