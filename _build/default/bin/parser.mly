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
  | Lambda; p = Identifier; Dot; e = expr;
    { Abstraction { param = p; body = e } }

let application :=
  | sub_expr
  | f = application; a = sub_expr;
    {Application { funct = f; argument = a }}

let expr := 
    | abstraction
    | application
