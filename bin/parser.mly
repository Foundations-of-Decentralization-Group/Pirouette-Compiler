%{
    open Expr
%}

%token <string> Identifier
%token <string> SyncLbl
%token <string> ChoreoVars
%token <int> INT
%token <string> STRING
%token <string> BOOL
%token Plus "+"
%token Minus "-"
%token Product "*"
%token Division "/"
%token Gt ">"
%token Lt "<"
%token Eq "="
%token LParen "("
%token RParen ")"
%token LSqParen "["
%token RSqParen "]"
%token If "if"
%token Else "else"
%token Then "then"
%token Comm_S "~>"
%token Arrow "->"
%token BoolType "bool"
%token IntType "int"
%token StringType "string"
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
%nonassoc In BoolType StringType IntType
%nonassoc Let
%nonassoc Fun
%nonassoc Then 
%nonassoc Else
%nonassoc Terminate
%right Assignment
%nonassoc Comm_S
%nonassoc Dot
%nonassoc Colon
%left Gt Lt Eq 
%left Plus Minus
%left Product Division
%nonassoc LParen RParen LSqParen RSqParen

%start <Expr.expr option> prog

%%

let line_end := END | EOF

let prog :=
    | EOF; {None}
    | e = expr; line_end; {Some e}


let ltyp :=
    | BoolType; {BoolType}
    | IntType; {IntType}
    | StringType; {StringType}

let gtyp :=
  | loc = Identifier; Dot; typ = ltyp;
    { DotType (Location loc, typ) }
  | ityp = gtyp; Arrow; otyp = gtyp;
    { ArrowType (ityp, otyp) }

let variable := 
    | LParen; name = Identifier; Colon; typ = z; RParen; 
        {Variable (Name name, Some typ)}
    | name = Identifier; 
        {Variable (Name name, None)}

let choreo_vars := 
    | name = ChoreoVars; Colon; typ = gtyp; 
        {ChoreoVars (Name name, Some typ)}

let binop :=
    | Gt; {Gt}
    | Lt; {Lt}
    | Eq; {Eq}

let if_condition := 
    | lft = sub_expr; op = binop; rght = sub_expr;
        {Condition (lft, op, rght, Some BoolType)}
    | LParen; c = if_condition; RParen;
        {c}

let if_thn_else := 
    | If; ift = choreographies; Then; thn = choreographies; Else; el = choreographies;
        {Branch (ift, thn, el, None)}

let sync := 
    | sndr = Identifier; LSqParen; d = SyncLbl; RSqParen; Comm_S; rcvr = Identifier; Terminate; thn = choreographies;
        {Sync (Location sndr, Direction d, Location rcvr, thn, None)}

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
    | variable
    | if_condition

// person.name ~> person2.x   let person2.x := person.name ~> person2 in ---

// fun F (X) : (T1 -> T2) := C1 in C2
//  fun F (X : T1 ) : T2 := C1 in C2 use this
// name = ChoreoVars; Colon; ityp = gtyp; 
let fun_expr :=    
    //arg = choreovars
    | Fun; name = Identifier; LParen; arg_name = ChoreoVars; Colon; ityp = gtyp; RParen; Colon; otyp = gtyp; Assignment; body = choreographies;
            {FunG (Name name, ChoreoVars (Name arg_name, Some ityp), body, Some (ArrowType(ityp, otyp)))}
    | Fun; name = Identifier; LParen; arg = choreo_vars; RParen; Assignment; body = choreographies;
            {FunG (Name name, arg, body, None)}
    //arg = l.e Fun funct (l.(x:int)) : person.int := Body 
    | Fun; name = Identifier; LParen; loc = Identifier; Dot; LParen; bndr = Identifier; Colon; BoolType; RParen; 
        RParen; Colon; ityp = gtyp; Assignment; body = choreographies;
            {FunL (Name name, Location loc, Variable (Name bndr, Some BoolType), body, Some (ArrowType(DotType(Location loc, BoolType), ityp)))}
    | Fun; name = Identifier; LParen; loc = Identifier; Dot; LParen; bndr = Identifier; Colon; IntType; RParen; 
        RParen; Colon; ityp = gtyp; Assignment; body = choreographies;
            {FunL (Name name, Location loc, Variable (Name bndr, Some IntType), body, Some (ArrowType(DotType(Location loc, IntType), ityp)))}
    | Fun; name = Identifier; LParen; loc = Identifier; Dot; LParen; bndr = Identifier; Colon; StringType; RParen; 
        RParen; Colon; ityp = gtyp; Assignment; body = choreographies;
            {FunL (Name name, Location loc, Variable (Name bndr, Some StringType), body, Some (ArrowType(DotType(Location loc, StringType), ityp)))}
    | Fun; name = Identifier; LParen; loc = Identifier; Dot; arg = variable; RParen; Assignment; body = choreographies;
            {FunL (Name name, Location loc, arg, body, None)}

let application :=
    | LParen; funct = choreographies; RParen; arg = choreographies;
        {Application (funct, arg, None)} 

let calling :=
    | name = Identifier; arg = le;
        {Calling (Name name, arg, None)}

let le :=
    | l = Identifier; Dot; e = sub_expr;
        {Assoc (Location l, e, None)}

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
    | c = choreographies; Comm_S; rcvr = Identifier; Dot; bndr = variable; Terminate; cp = choreographies;
        {Let (Location rcvr, bndr, Snd (c, Location rcvr, None), cp, None)}                         
    | Let; rcvr = Identifier; Dot; bndr = variable; Assignment; sndr = le; Comm_S; Identifier; Terminate; In; cp = choreographies;
        {Let (Location rcvr, bndr, Snd (sndr, Location rcvr, None), cp, None)}   
    | Let; sndr = Identifier; Dot; bndr = variable; Assignment; c = choreographies; In; cp = choreographies;
        {Let (Location sndr, bndr, c, cp, None)}  

let operator :=
    | lft = sub_expr; Plus; rght = sub_expr; 
        {Plus (lft, rght, Some IntType)}
    | lft = sub_expr; Minus; rght = sub_expr; 
        {Minus (lft, rght, Some IntType)}
    | lft = sub_expr; Product; rght = sub_expr; 
        {Product (lft, rght, Some IntType)}
    | lft = sub_expr; Division; rght = sub_expr; 
        {Division (lft, rght, Some IntType)}

let eq := 
    | LParen; op = operator; RParen;
        {op}
    | op = operator; 
        {op}

let expr := 
    | choreographies
    // | sub_expr
