1: /* net_parser.mly */
2: 
3: %{
4:   open Ast_core.Net.M
5:   open Ast_core.Local.M
6:   open Parsed_ast
7: 
8:   let gen_pos startpos endpos =
9:     let open Lexing in
10:      { Pos_info.fname = startpos.pos_fname;
11:        start = startpos.pos_lnum, startpos.pos_cnum - startpos.pos_bol;
12:        stop = endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol;
13:      }
14: 
15:   (* Helper stubs for setting info on local AST nodes.
16:      Replace these with your own implementations if needed. *)
17:   let local_set_info_expr pos e = e
18:   let local_set_info_typ pos t = t
19:   let local_set_info_pattern pos p = p
20: %}
21: 
22: %token <string> ID
23: %token <int> INT
24: %token <string> STRING
25: %token TRUE FALSE
26: %token UNIT_T INT_T STRING_T BOOL_T
27: %token FUN TYPE RET RECV SEND CHOOSEFOR ALLOWCHOICE
28: %token UNDERSCORE
29: %token COLONEQ
30: %token PLUS MINUS TIMES DIV
31: %token NOT
32: %token AND OR
33: %token EQ NEQ LT LEQ GT GEQ
34: %token LPAREN RPAREN LBRACKET RBRACKET
35: %token COMMA DOT COLON SEMICOLON
36: %token ARROW
37: %token BAR
38: %token LET IN
39: %token IF THEN ELSE
40: %token FST SND LEFT RIGHT
41: %token MATCH WITH
42: %token FOREIGN
43: %token EOF
44: 
45: %nonassoc IN
46: %right ARROW
47: %nonassoc BAR
48: %right OR
49: %right AND
50: %left EQ NEQ LT LEQ GT GEQ
51: %left PLUS MINUS
52: %left TIMES DIV
53: %nonassoc UNARY
54: %left DOT
55: 
56: %type <Parsed_ast.Net.stmt_block> net_prog
57: %type <Parsed_ast.Net.stmt_block> net_stmt_block
58: %type <Parsed_ast.Net.stmt> net_stmt
59: %type <Parsed_ast.Net.expr> net_expr
60: %type <Parsed_ast.Net.typ> net_type
61: 
62: %type <Parsed_ast.Local.expr> local_expr
63: %type <Parsed_ast.Local.pattern> local_pattern
64: %type <Parsed_ast.Local.typ> local_type
65: %type <Parsed_ast.Local.var_id> var_id
66: %type <Parsed_ast.Local.loc_id> loc_id
67: %type <Parsed_ast.Local.typ_id> typ_id
68: %type <Parsed_ast.Local.sync_label> sync_label
69: %type <Parsed_ast.Local.value> value
70: 
71: %inline local_case:
72:   | BAR p=local_pattern ARROW e=local_expr { (p, e) }
73: 
74: %inline un_op:
75:   | MINUS { Neg (gen_pos $startpos $endpos) }
76:   | NOT { Not (gen_pos $startpos $endpos) }
77: 
78: %inline bin_op:
79:   | PLUS { Plus (gen_pos $startpos $endpos) }
80:   | MINUS { Minus (gen_pos $startpos $endpos) }
81:   | TIMES { Times (gen_pos $startpos $endpos) }
82:   | DIV   { Div (gen_pos $startpos $endpos) }
83:   | AND   { And (gen_pos $startpos $endpos) }
84:   | OR    { Or (gen_pos $startpos $endpos) }
85:   | EQ    { Eq (gen_pos $startpos $endpos) }
86:   | NEQ   { Neq (gen_pos $startpos $endpos) }
87:   | LT    { Lt (gen_pos $startpos $endpos) }
88:   | LEQ   { Leq (gen_pos $startpos $endpos) }
89:   | GT    { Gt (gen_pos $startpos $endpos) }
90:   | GEQ   { Geq (gen_pos $startpos $endpos) }
91: 
92: %inline nonempty_list(X)
93:   | X              { [$1] }
94:   | X nonempty_list(X) { $1 :: $2 }
95: 
96: %inline list(X)
97:   | /* empty */    { [] }
98:   | nonempty_list(X) { $1 }
99: 
100: %start net_prog
101: %%
102: 
103: net_prog:
104:   | net_stmt_block EOF { $1 }
105: 
106: net_stmt_block:
107:   | list(net_stmt) { $1 }
108: 
109: net_stmt:
110:   | p=local_pattern COLON t=net_type SEMICOLON
111:       { Decl (p, t, gen_pos $startpos $endpos) }
112:   | ps=nonempty_list(local_pattern) COLONEQ e=net_expr SEMICOLON
113:       { Assign (ps, e, gen_pos $startpos $endpos) }
114:   | TYPE id=typ_id COLONEQ t=net_type SEMICOLON?
115:       { TypeDecl (id, t, gen_pos $startpos $endpos) }
116:   | f=foreign_decl { f }
117: 
118: (* Network expressions corresponding to Ast_core.Net.expr *)
119: net_expr:
120:   | IF e1=net_expr THEN e2=net_expr ELSE e3=net_expr
121:       { If (e1, e2, e3, gen_pos $startpos $endpos) }
122:   | LET stmts=net_stmt_block IN e=net_expr
123:       { Let (stmts, e, gen_pos $startpos $endpos) }
124:   | RET e=local_expr
125:       { Ret (e, gen_pos $startpos $endpos) }
126:   | net_expr SEND id=loc_id
127:       { Send ($1, id, gen_pos $startpos $endpos) }
128:   | RECV id=loc_id
129:       { Recv (id, gen_pos $startpos $endpos) }
130:   | CHOOSEFOR lab=sync_label id=loc_id e=net_expr
131:       { ChooseFor (lab, id, e, gen_pos $startpos $endpos) }
132:   | ALLOWCHOICE id=loc_id LBRACKET cases=nonempty_list(choice_case) RBRACKET
133:       { AllowChoice (id, cases, gen_pos $startpos $endpos) }
134:   | FUN ps=nonempty_list(local_pattern) ARROW e=net_expr
135:       { FunDef (ps, e, gen_pos $startpos $endpos) }
136:   | net_expr1 { $1 }
137: 
138: net_expr1:
139:   | net_expr1 net_expr2
140:       { FunApp ($1, $2, gen_pos $startpos $endpos) }
141:   | net_expr2 { $1 }
142:   | net_expr1 COMMA net_expr2
143:       { Pair ($1, $3, gen_pos $startpos $endpos) }
144: 
145: net_expr2:
146:   | LPAREN RPAREN { Unit (gen_pos $startpos $endpos) }
147:   | id=var_id { Var (id, gen_pos $startpos $endpos) }
148:   | LPAREN e=net_expr RPAREN { e }
149: 
150: (* Network types corresponding to Ast_core.Net.typ *)
151: net_type:
152:   | UNIT_T { TUnit (gen_pos $startpos $endpos) }
153:   | net_local_type { TLoc ($1, gen_pos $startpos $endpos) }
154:   | net_type ARROW net_type
155:       { TMap ($1, $3, gen_pos $startpos $endpos) }
156:   | net_type TIMES net_type
157:       { TProd ($1, $3, gen_pos $startpos $endpos) }
158:   | net_type PLUS net_type
159:       { TSum ($1, $3, gen_pos $startpos $endpos) }
160:   | LPAREN t=net_type RPAREN
161:       { local_set_info_typ (gen_pos $startpos $endpos) t }
162: 
163: net_local_type:
164:   | INT_T { TInt (gen_pos $startpos $endpos) }
165:   | STRING_T { TString (gen_pos $startpos $endpos) }
166:   | BOOL_T { TBool (gen_pos $startpos $endpos) }
167:   | net_local_type TIMES net_local_type
168:       { TProd ($1, $3, gen_pos $startpos $endpos) }
169:   | net_local_type PLUS net_local_type
170:       { TSum ($1, $3, gen_pos $startpos $endpos) }
171:   | LPAREN t=net_local_type RPAREN
172:       { local_set_info_typ (gen_pos $startpos $endpos) t }
173: 
174: (* Local expressions used in RET and within function bodies *)
175: local_expr:
176:   | LPAREN RPAREN { Unit (gen_pos $startpos $endpos) }
177:   | v=value { Val (v, gen_pos $startpos $endpos) }
178:   | id=var_id { Var (id, gen_pos $startpos $endpos) }
179:   | op=un_op e=local_expr %prec UNARY
180:       { UnOp (op, e, gen_pos $startpos $endpos) }
181:   | e1=local_expr op=bin_op e2=local_expr
182:       { BinOp (e1, op, e2, gen_pos $startpos $endpos) }
183:   | LET id=var_id COLON t=local_type COLONEQ e1=local_expr IN e2=local_expr
184:       { Let (id, t, e1, e2, gen_pos $startpos $endpos) }
185:   | LPAREN e1=local_expr COMMA e2=local_expr RPAREN
186:       { Pair (e1, e2, gen_pos $startpos $endpos) }
187:   | FST e=local_expr { Fst (e, gen_pos $startpos $endpos) }
188:   | SND e=local_expr { Snd (e, gen_pos $startpos $endpos) }
189:   | LEFT e=local_expr { Left (e, gen_pos $startpos $endpos) }
190:   | RIGHT e=local_expr { Right (e, gen_pos $startpos $endpos) }
191:   | MATCH e=local_expr WITH cases=nonempty_list(local_case)
192:       { Match (e, cases, gen_pos $startpos $endpos) }
193:   | LPAREN e=local_expr RPAREN { local_set_info_expr (gen_pos $startpos $endpos) e }
194: 
195: (* Local types, similar to the definitions in the choreo parser *)
196: local_type:
197:   | UNIT_T { TUnit (gen_pos $startpos $endpos) }
198:   | INT_T { TInt (gen_pos $startpos $endpos) }
199:   | STRING_T { TString (gen_pos $startpos $endpos) }
200:   | BOOL_T { TBool (gen_pos $startpos $endpos) }
201:   | local_type TIMES local_type { TProd ($1, $3, gen_pos $startpos $endpos) }
202:   | local_type PLUS local_type { TSum ($1, $3, gen_pos $startpos $endpos) }
203:   | LPAREN t=local_type RPAREN { local_set_info_typ (gen_pos $startpos $endpos) t }
204: 
205: (* Local patterns, again following the style of the choreography parser *)
206: local_pattern:
207:   | UNDERSCORE { Default (gen_pos $startpos $endpos) }
208:   | v=value { Val (v, gen_pos $startpos $endpos) }
209:   | x=var_id { Var (x, gen_pos $startpos $endpos) }
210:   | LPAREN p1=local_pattern COMMA p2=local_pattern RPAREN
211:       { Pair (p1, p2, gen_pos $startpos $endpos) }
212:   | LEFT p=local_pattern { Left (p, gen_pos $startpos $endpos) }
213:   | RIGHT p=local_pattern { Right (p, gen_pos $startpos $endpos) }
214:   | LPAREN p=local_pattern RPAREN
215:       { local_set_info_pattern (gen_pos $startpos $endpos) p }
216: 
217: foreign_decl:
218:   | FOREIGN id=var_id COLON t=net_type COLONEQ s=STRING SEMICOLON
219:       { ForeignDecl (id, t, s, gen_pos $startpos $endpos) }
220: 
221: %inline choice_case:
222:   | BAR lab=sync_label ARROW e=net_expr { (lab, e) }
223:
224: (* End of net_parser.mly *)