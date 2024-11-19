# Implementing FFI (Foreign Function Interface) in the Compiler

This guide explains how to extend the compiler to support FFI, allowing integration with external functions. We’ll walk through the key components and steps required, leveraging the updated implementation in `choreo.ml` and `choreo.mli`. Additionally, we’ll discuss the design decisions behind making FFI declarations as statements and placing them within the `choreo` module instead of the `local` module. We’ll also review the changes made to `pprint_ast.ml`.

## Table of Contents

1. [Syntax Design](#syntax-design)  
2. [Lexer Extension](#lexer-extension)  
3. [Parser Extension](#parser-extension)  
   - 3.1. [Add Token Declaration](#31-add-token-declaration)  
   - 3.2. [Define Grammar Rules](#32-define-grammar-rules)  
4. [AST Modification](#ast-modification)  
   - 4.1. [Update Choreo AST Nodes](#41-update-choreo-ast-nodes)  
   - 4.2. [Why Statements Over Expressions?](#42-why-statements-over-expressions)  
   - 4.3. [Placement in Choreo Module](#43-placement-in-choreo-module)  
5. [AST Utility and Pretty Printing](#ast-utility-and-pretty-printing)  
   - 5.1. [Reviewing pprint_ast.ml](#51-reviewing-pprint_astml)  
   - 5.2. [Key Updates](#52-key-updates)  
   - 5.3. [Benefits of Pretty Printing](#53-benefits-of-pretty-printing)  
6. [Code Generation](#code-generation)  
   - 6.1. [Emit FFI Bindings](#61-emit-ffi-bindings)  
7. [Why These Components?](#why-these-components)  
8. [Conclusion](#conclusion)  

---

## 1. Syntax Design

### Why Update Syntax Design?

To integrate external functions seamlessly, the compiler needs a clear and consistent syntax for FFI declarations. This ensures that developers can easily declare and use external functions within their codebase.

### How to Implement with FFI Example

We design the concrete syntax for FFI declarations using the `foreign` keyword. This syntax allows us to:
- Declare external functions with the `foreign` keyword.
- Specify the function name and type.
- Provide the external function identifier as a string.

Example:
```ocaml
foreign myFunction : SomeType := "external_function";


## 2. Lexer Extension

Add new tokens to the lexer to recognize FFI-specific syntax:

```ocaml:lib/parsing/lexer.mll
let read = parse
  ...
  | "foreign" { FOREIGN }```markdown
# Implementing FFI (Foreign Function Interface) in the Compiler

This guide explains how to extend the compiler to support FFI, allowing integration with external functions. We’ll walk through the key components and steps required, leveraging the updated implementation in `choreo.ml` and `choreo.mli`. Additionally, we’ll discuss the design decisions behind making FFI declarations as statements and placing them within the `choreo` module instead of the `local` module. We’ll also review the changes made to `pprint_ast.ml`.

## Table of Contents

1. [Syntax Design](#syntax-design)  
2. [Lexer Extension](#lexer-extension)  
3. [Parser Extension](#parser-extension)  
   - 3.1. [Add Token Declaration](#31-add-token-declaration)  
   - 3.2. [Define Grammar Rules](#32-define-grammar-rules)  
4. [AST Modification](#ast-modification)  
   - 4.1. [Update Choreo AST Nodes](#41-update-choreo-ast-nodes)  
   - 4.2. [Why Statements Over Expressions?](#42-why-statements-over-expressions)  
   - 4.3. [Placement in Choreo Module](#43-placement-in-choreo-module)  
5. [AST Utility and Pretty Printing](#ast-utility-and-pretty-printing)  
   - 5.1. [Reviewing pprint_ast.ml](#51-reviewing-pprint_astml)  
   - 5.2. [Key Updates](#52-key-updates)  
   - 5.3. [Benefits of Pretty Printing](#53-benefits-of-pretty-printing)  
6. [Code Generation](#code-generation)  
   - 6.1. [Emit FFI Bindings](#61-emit-ffi-bindings)  
7. [Why These Components?](#why-these-components)  
8. [Conclusion](#conclusion)  

---

## 1. Syntax Design

### Why Update Syntax Design?

To integrate external functions seamlessly, the compiler needs a clear and consistent syntax for FFI declarations. This ensures that developers can easily declare and use external functions within their codebase.

### How to Implement with FFI Example

We design the concrete syntax for FFI declarations using the `foreign` keyword. This syntax allows us to:
- Declare external functions with the `foreign` keyword.
- Specify the function name and type.
- Provide the external function identifier as a string.

Example:
```ocaml
foreign myFunction : SomeType := "external_function";
```

---

## 2. Lexer Extension

### Why Update the Lexer?

The lexer must recognize new syntax elements introduced for FFI declarations. Without updating the lexer, the compiler would fail to tokenize FFI-specific keywords and constructs, leading to parsing errors.

### How to Implement with FFI Example

Add new tokens to the lexer:
```ocaml
(* lib/parsing/lexer.mll *)
let read = parse
  ...
  | "foreign" { FOREIGN }
  ...
```

The lexer now recognizes:
- The `foreign` keyword.
- String literals for external function names.

---

## 3. Parser Extension

### Why Update the Parser?

The parser needs to understand the new FFI declarations’ structure and integrate them into the language’s grammar.

### 3.1. Add Token Declaration

Declare the new `FOREIGN` token:
```ocaml
(* lib/parsing/parser.mly *)
%token FOREIGN
```

### 3.2. Define Grammar Rules

Add grammar rules to parse FFI declarations:
```ocaml
(* lib/parsing/parser.mly *)
foreign_decl:
  | FOREIGN id=var_id COLON t=choreo_type COLONEQ s=STRING SEMICOLON 
    { ForeignDecl (id, t, s, gen_pos $startpos $endpos) }
```

---

## 4. AST Modification

### Why Modify the AST?

The AST must include new nodes to represent FFI declarations, enabling the compiler to process them.

### 4.1. Update Choreo AST Nodes

```ocaml
(* lib/ast_core/choreo.ml *)
module M = struct
  ...
  type 'a stmt =
    | Decl of 'a pattern * 'a typ * 'a
    | ForeignDecl of 'a Local.var_id * 'a typ * string * 'a
  ...
end
```

### 4.2. Why Statements Over Expressions?

- **Clarity of Structure:** FFI declarations introduce bindings rather than producing a value.  
- **Syntax Semantics:** Statements represent actions or instructions.  
- **Modularity:** Maintains a clear separation of concerns.  

### 4.3. Placement in Choreo Module

- **Module Responsibility:** FFI declarations align with choreography-specific constructs.  
- **Scalability:** Supports scalability for future features.  

---

## 5. AST Utility and Pretty Printing

### Why Update AST Utilities and Pretty Printing?

Updating the AST structure requires corresponding updates to the pretty-printing utilities to handle new constructs.

### 5.1. Reviewing pprint_ast.ml

Add handling for `ForeignDecl` in `pprint_ast.ml`.

### 5.2. Key Updates

```ocaml
(* lib/ast_utils/pprint_ast.ml *)
and pprint_choreo_stmt ppf (stmt : 'a Choreo.stmt) =
  match stmt with
  | ForeignDecl (VarId (id, _), t, s, _) ->
    fprintf ppf "@[<h>foreign %s : %a := \"%s\"@]" id pprint_choreo_type t s
```

### 5.3. Benefits of Pretty Printing

- **Debugging:** Clear and structured representations.  
- **Documentation:** Visualize the structure of the code.  
- **Maintenance:** Readable output for complex AST nodes.  

---

## 6. Code Generation

### Why Update Code Generation?

To ensure that FFI declarations are correctly translated into executable code.

### 6.1. Emit FFI Bindings

Add code generation rules:
```ocaml
(* lib/codegen/emit_core.ml *)
let emit_foreign_decl id typ external_name =
  [%expr 
    let [%p pvar id] = External.[%e evar external_name] in
    [%e type_annotation typ]
  ]
```

---

## 7. Why These Components?

Each component plays a critical role:
- **Lexer/Parser:** Provides syntax recognition.  
- **AST:** Represents program structure.  
- **Code Generation:** Produces target code.  

---

## 8. Conclusion

By updating the AST to include `ForeignDecl` and representing FFI declarations as statements, the compiler achieves a cleaner and more organized architecture. Updates in `pprint_ast.ml` ensure accurate representation during debugging. This robust implementation ensures seamless FFI integration.

```
  ...
```

The lexer needs to recognize:
- The `foreign` keyword
- String literals for external function names
- Other existing tokens like identifiers and types

## 3. Parser Extension

### 3.1. Add Token Declaration

```ocaml:lib/parsing/parser.mly
%token FOREIGN
```

### 3.2. Define Grammar Rules

Add grammar rules to parse FFI declarations:

```ocaml:lib/parsing/parser.mly
foreign_decl:
  | FOREIGN id=var_id COLON t=choreo_type COLONEQ s=STRING SEMICOLON 
    { ForeignDecl (id, t, s, gen_pos $startpos $endpos) }
```

This rule:
- Matches the `foreign` keyword
- Captures the function identifier
- Parses the type annotation
- Captures the external function name string
- Generates an AST node with position information

## 4. AST Modification

### 4.1. Update Choreo AST Nodes

We have moved the FFI declaration to the `choreo` module to better encapsulate choreography-specific constructs.

```ocaml:lib/ast_core/choreo.ml
module M = struct
  ...
  type 'a stmt =
    | Decl of 'a pattern * 'a typ * 'a
    | Assign of 'a pattern list * 'a expr * 'a (* list is only for F P1 P2 ... Pn := C *)
    | TypeDecl of 'a Local.typ_id * 'a typ * 'a
    | ForeignDecl of 'a Local.var_id * 'a typ * string * 'a
  ...
end
```

```ocaml:lib/ast_core/choreo.mli
module M : sig
  ...
  type 'a stmt =
    | Decl of 'a pattern * 'a typ * 'a
    | Assign of 'a pattern list * 'a expr * 'a
    | TypeDecl of 'a Local.M.typ_id * 'a typ * 'a
    | ForeignDecl of 'a Local.M.var_id * 'a typ * string * 'a
  ...
end
```

### 4.2. Why Statements Over Expressions?

We chose to represent FFI declarations as **statements** instead of **expressions** for the following reasons:

- **Clarity of Structure**: FFI declarations are inherently about introducing bindings rather than producing a value. As such, they fit naturally within the statement category, which handles variable declarations and assignments.
  
- **Syntax Semantics**: Statements often represent actions or instructions in the code, whereas expressions represent evaluations that return values. FFI declarations are about declaring external interfaces, aligning with the action-oriented nature of statements.

- **Modularity**: Keeping declarations as statements allows for a clearer separation between different kinds of actions (e.g., FFI declarations vs. variable assignments).

### 4.3. Placement in Choreo Module

We implemented FFI declarations within the `choreo` module (`choreo.ml` and `choreo.mli`) instead of the `local` module for the following reasons:

- **Module Responsibility**: The `choreo` module is responsible for choreography-specific constructs, and FFI declarations are more aligned with the choreography's role in interfacing with external functions.
  
- **Separation of Concerns**: The `local` module handles local identifiers, types, and expressions that are more granular and specific to individual components, whereas `choreo` manages the broader orchestration, including external interactions.

- **Scalability**: Placing FFI declarations within `choreo` allows for better scalability as more choreography-related features are added, maintaining a clean and organized module structure.

## 5. AST Utility and Pretty Printing

### 5.1. Reviewing `pprint_ast.ml`

The `pprint_ast.ml` module is responsible for pretty-printing the AST for debugging and visualization purposes. With the updated AST structure in `choreo.ml` and `choreo.mli`, we've made corresponding updates to ensure accurate and readable output.

```ocaml:lib/ast_utils/pprint_ast.ml
(* 
   File: pprint_ast.ml
   Date: 09/18/2024

   A pretty print library to print Pirouette code in format using
   the Format module in OCaml: https://v2.ocaml.org/api/Format.html

   Format.formatter fmt: 
   print code to:
   - standard output Stdlib.stdout: Format.std_formatter
   - standard error Stdlib.stderr: Format.err_formatter
   - string {use stdbuffer in Format lib}: Format.str_formatter & Format.flush_str_formatter
   - file: Format.formatter_of_out_channel (open_out "file_name")
*)
...
(** [pprint_choreo_stmt] takes a formatter [ppf] and a statement,
    and prints the formatted code of the statement

    - Variants of statements include Decl, Assign, TypeDecl, ForeignDecl
    - Calls helper functions [pprint_choreo_pattern], [pprint_choreo_type],
      [pprint_choreo_expr] to pretty print the statement *)
and pprint_choreo_stmt ppf (stmt : 'a Choreo.stmt) =
  match stmt with
  | Decl (p, t, _) ->
    fprintf ppf "@[<h>%a : %a;@]" pprint_choreo_pattern p pprint_choreo_type t
  | Assign (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>%a :=@ %a;@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | TypeDecl (TypId (id, _), t, _) ->
    fprintf ppf "@[<h>type %s := %a;@]" id pprint_choreo_type t
  | ForeignDecl (VarId (id, _), t, s, _) ->
    fprintf ppf "@[<h>foreign %s : %a := \"%s\"@]" id pprint_choreo_type t s
...
```

### 5.2. Key Updates

- **Foreign Declarations**: Added handling for `ForeignDecl` within `pprint_choreo_stmt` to ensure FFI declarations are correctly printed.
  
- **Modular Pretty-Printing**: Leveraged helper functions like `pprint_choreo_pattern`, `pprint_choreo_type`, and `pprint_choreo_expr` to maintain readability and manage complexity.

### 5.3. Benefits of Pretty Printing

- **Debugging**: Enhanced debugging capabilities by providing clear and structured representations of the AST.

- **Documentation**: Serves as a documentation aid by allowing developers to visualize the structure of the code.

- **Maintenance**: Facilitates easier maintenance and updates to the compiler by providing readable output for complex AST nodes.

## 6. Code Generation

### 6.1. Emit FFI Bindings

Add code generation rules for FFI declarations:

```ocaml:lib/codegen/emit_core.ml
let emit_foreign_decl id typ external_name =
  [%expr 
    let [%p pvar id] = External.[%e evar external_name] in
    [%e type_annotation typ]
  ]
```

This generates:
- External function binding
- Type annotations
- Runtime checks if needed

## Why These Components?

1. **Lexer/Parser**: 
   - Provides syntax recognition
   - Ensures correct grammar
   - Generates structured AST
   - Reference: `lib/parsing/parser.mly` lines 251-254

2. **AST**:
   - Represents program structure
   - Enables static analysis
   - Facilitates transformations
   - Reference: `lib/ast_utils/jsonify_ast.ml` lines 133-154

3. **Code Generation**:
   - Produces target code
   - Handles runtime integration
   - Manages FFI bindings
   - Reference: `lib/codegen/emit_core.ml` lines 76-103

# Conclusion

By updating the AST to include `ForeignDecl` within the `choreo` module and representing FFI declarations as statements, we achieve a cleaner and more organized compiler architecture. The adjustments in `pprint_ast.ml` ensure that these new constructs are accurately represented during debugging and documentation. Adhering to best practices guarantees that the FFI integration is safe, efficient, and maintainable.Implementing FFI (Foreign Function Interface) in the Compiler

This guide explains how to extend the compiler to support FFI, allowing integration with external functions. We’ll walk through the key components and steps required, leveraging the updated implementation in choreo.ml and choreo.mli. Additionally, we’ll discuss the design decisions behind making FFI declarations as statements and placing them within the choreo module instead of the local module. We’ll also review the changes made to pprint_ast.ml.

Table of Contents

	1.	Syntax Design
	2.	Lexer Extension
	3.	Parser Extension
	•	3.1. Add Token Declaration
	•	3.2. Define Grammar Rules
	4.	AST Modification
	•	4.1. Update Choreo AST Nodes
	•	4.2. Why Statements Over Expressions?
	•	4.3. Placement in Choreo Module
	5.	AST Utility and Pretty Printing
	•	5.1. Reviewing pprint_ast.ml
	•	5.2. Key Updates
	•	5.3. Benefits of Pretty Printing
	6.	Code Generation
	•	6.1. Emit FFI Bindings
	7.	Why These Components?
	8.	Conclusion

1. Syntax Design

Why Update Syntax Design?

To integrate external functions seamlessly, the compiler needs a clear and consistent syntax for FFI declarations. This ensures that developers can easily declare and use external functions within their codebase.

How to Implement with FFI Example

We design the concrete syntax for FFI declarations using the foreign keyword. This syntax allows us to:
	•	Declare external functions with the foreign keyword.
	•	Specify the function name and type.
	•	Provide the external function identifier as a string.

foreign myFunction : SomeType := "external_function";

This declaration enables the compiler to recognize and process external functions correctly.

2. Lexer Extension

Why Update the Lexer?

The lexer must recognize new syntax elements introduced for FFI declarations. Without updating the lexer, the compiler would fail to tokenize FFI-specific keywords and constructs, leading to parsing errors.

How to Implement with FFI Example

Add new tokens to the lexer to recognize FFI-specific syntax:

(* lib/parsing/lexer.mll *)
let read = parse
  ...
  | "foreign" { FOREIGN }
  ...

The lexer now recognizes:
	•	The foreign keyword.
	•	String literals for external function names.
	•	Other existing tokens like identifiers and types.

3. Parser Extension

Why Update the Parser?

The parser needs to understand the new FFI declarations’ structure and integrate them into the language’s grammar. This ensures that FFI statements are correctly interpreted and transformed into the Abstract Syntax Tree (AST).

How to Implement with FFI Example

3.1. Add Token Declaration

Declare the new FOREIGN token in the parser to handle FFI declarations:

(* lib/parsing/parser.mly *)
%token FOREIGN

3.2. Define Grammar Rules

Add grammar rules to parse FFI declarations:

(* lib/parsing/parser.mly *)
foreign_decl:
  | FOREIGN id=var_id COLON t=choreo_type COLONEQ s=STRING SEMICOLON 
    { ForeignDecl (id, t, s, gen_pos $startpos $endpos) }

This rule:
	•	Matches the foreign keyword.
	•	Captures the function identifier.
	•	Parses the type annotation.
	•	Captures the external function name string.
	•	Generates an AST node with position information.

4. AST Modification

Why Modify the AST?

To accurately represent FFI declarations within the compiler’s internal structures, the AST must include new nodes corresponding to these declarations. This allows subsequent compiler phases to process FFI declarations appropriately.

How to Implement with FFI Example

4.1. Update Choreo AST Nodes

Move the FFI declaration to the choreo module to better encapsulate choreography-specific constructs:

(* lib/ast_core/choreo.ml *)
module M = struct
  ...
  type 'a stmt =
    | Decl of 'a pattern * 'a typ * 'a
    | Assign of 'a pattern list * 'a expr * 'a (* list is only for F P1 P2 ... Pn := C *)
    | TypeDecl of 'a Local.typ_id * 'a typ * 'a
    | ForeignDecl of 'a Local.var_id * 'a typ * string * 'a
  ...
end

(* lib/ast_core/choreo.mli *)
module M : sig
  ...
  type 'a stmt =
    | Decl of 'a pattern * 'a typ * 'a
    | Assign of 'a pattern list * 'a expr * 'a
    | TypeDecl of 'a Local.M.typ_id * 'a typ * 'a
    | ForeignDecl of 'a Local.M.var_id * 'a typ * string * 'a
  ...
end

4.2. Why Statements Over Expressions?

We chose to represent FFI declarations as statements instead of expressions for the following reasons:
	•	Clarity of Structure: FFI declarations introduce bindings rather than producing a value, fitting naturally within statements.
	•	Syntax Semantics: Statements represent actions or instructions, aligning with the action-oriented nature of FFI declarations.
	•	Modularity: Keeping declarations as statements allows for clearer separation between different kinds of actions, such as FFI declarations versus variable assignments.

4.3. Placement in Choreo Module

FFI declarations are implemented within the choreo module instead of the local module for the following reasons:
	•	Module Responsibility: The choreo module handles choreography-specific constructs, and FFI declarations align with interfacing external functions.
	•	Separation of Concerns: The local module manages local identifiers, types, and expressions, while choreo manages broader orchestration, including external interactions.
	•	Scalability: Placing FFI declarations within choreo supports scalability as more choreography-related features are added, maintaining a clean and organized module structure.

5. AST Utility and Pretty Printing

Why Update AST Utilities and Pretty Printing?

As the AST structure evolves to include FFI declarations, the utilities responsible for handling and displaying the AST must also be updated. This ensures that FFI constructs are accurately represented during debugging and documentation.

How to Implement with FFI Example

5.1. Reviewing pprint_ast.ml

The pprint_ast.ml module is responsible for pretty-printing the AST for debugging and visualization purposes. With the updated AST structure in choreo.ml and choreo.mli, corresponding updates are necessary to handle FFI declarations.

5.2. Key Updates

	•	Foreign Declarations: Added handling for ForeignDecl within pprint_choreo_stmt to ensure FFI declarations are correctly printed.
	•	Modular Pretty-Printing: Leveraged helper functions like pprint_choreo_pattern, pprint_choreo_type, and pprint_choreo_expr to maintain readability and manage complexity.

(* lib/ast_utils/pprint_ast.ml *)
(** [pprint_choreo_stmt] takes a formatter [ppf] and a statement,
    and prints the formatted code of the statement

    - Variants of statements include Decl, Assign, TypeDecl, ForeignDecl
    - Calls helper functions [pprint_choreo_pattern], [pprint_choreo_type],
      [pprint_choreo_expr] to pretty print the statement *)
and pprint_choreo_stmt ppf (stmt : 'a Choreo.stmt) =
  match stmt with
  | Decl (p, t, _) ->
    fprintf ppf "@[<h>%a : %a;@]" pprint_choreo_pattern p pprint_choreo_type t
  | Assign (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>%a :=@ %a;@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | TypeDecl (TypId (id, _), t, _) ->
    fprintf ppf "@[<h>type %s := %a;@]" id pprint_choreo_type t
  | ForeignDecl (VarId (id, _), t, s, _) ->
    fprintf ppf "@[<h>foreign %s : %a := \"%s\"@]" id pprint_choreo_type t s

5.3. Benefits of Pretty Printing

	•	Debugging: Enhanced debugging capabilities by providing clear and structured representations of the AST.
	•	Documentation: Serves as a documentation aid by allowing developers to visualize the structure of the code.
	•	Maintenance: Facilitates easier maintenance and updates to the compiler by providing readable output for complex AST nodes.

6. Code Generation

Why Update Code Generation?

To ensure that FFI declarations are correctly translated into executable code, the code generation phase must handle FFI constructs. This involves emitting appropriate bindings and type annotations for external functions.

How to Implement with FFI Example

6.1. Emit FFI Bindings

Add code generation rules for FFI declarations:

(* lib/codegen/emit_core.ml *)
let emit_foreign_decl id typ external_name =
  [%expr 
    let [%p pvar id] = External.[%e evar external_name] in
    [%e type_annotation typ]
  ]

This generates:
	•	External function binding.
	•	Type annotations.
	•	Runtime checks if needed.

Why These Components?

	1.	Lexer/Parser:
	•	Purpose: Provides syntax recognition, ensures correct grammar, generates structured AST.
	•	Reference: lib/parsing/parser.mly lines 251-254.
	2.	AST:
	•	Purpose: Represents program structure, enables static analysis, facilitates transformations.
	•	Reference: lib/ast_utils/jsonify_ast.ml lines 133-154.
	3.	Code Generation:
	•	Purpose: Produces target code, handles runtime integration, manages FFI bindings.
	•	Reference: lib/codegen/emit_core.ml lines 76-103.

Each component plays a critical role in integrating FFI support into the compiler, ensuring that external functions are seamlessly incorporated into the language.

Conclusion

By updating the AST to include ForeignDecl within the choreo module and representing FFI declarations as statements, we achieve a cleaner and more organized compiler architecture. The adjustments in pprint_ast.ml ensure that these new constructs are accurately represented during debugging and documentation. Adhering to best practices guarantees that the FFI integration is safe, efficient, and maintainable.

Each section provided a clear rationale for the necessary updates, followed by concrete implementation steps using the FFI example. This structured approach ensures that the compiler’s support for FFI is robust and well-integrated.