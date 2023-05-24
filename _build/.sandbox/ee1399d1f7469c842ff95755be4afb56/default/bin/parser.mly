%{
    open Expr
%}

%token <string> Identifier
%token LParen "("
%token RParen ")"
%token Lambda "/"
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
 
let sub_expr :=
    | variable
    | LParen; e = expr; RParen; {e}

let abstraction ==
  | LAMBDA; p = IDENT; Dot; e = expr;
    { Abstraction { param = p; body = e } }

let application :=
  | sub_expr
  | e1 = application; e2 = sub_expr;
    { Application { funct = e1; argument = e2 } }

let expr := 
    | abstraction
    | application
