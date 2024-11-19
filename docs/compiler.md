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
    [%e type_annotation typ]a
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
