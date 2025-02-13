let emit_foreign_decl name return_type extern_name =
  let loc = Location.none in
  (* Helper to extract module and function names from the extern string *)
  let get_module_and_function extern =
    if extern = "" then failwith "Empty external function" else
    if extern.[0] = '@' then
      let extern_trim = String.sub extern 1 (String.length extern - 1) in
      match String.split_on_char ':' extern_trim with
      | [module_path; function_name] ->
          (* Use the last component of the module path and capitalize it *)
          let module_components = String.split_on_char '/' module_path in
          let mod_name =
            match List.rev module_components with
            | last :: _ -> String.capitalize_ascii last
            | [] -> failwith "Invalid external function format"
          in
          (mod_name, function_name)
      | _ -> failwith "Invalid external function format. Expected @file:function"
    else
      (* For a basic external, use the provided name as function name and its capitalized form as module *)
      let function_name = extern in
      let mod_name = String.capitalize_ascii function_name in
      (mod_name, function_name)
  in
  let (mod_name, fn_name) = get_module_and_function extern_name in
  (* Create an identifier for Module.function_name *)
  let long_ident = Longident.Ldot (Longident.Lident mod_name, fn_name) in
  let fn_exp =
    Ast_helper.Exp.ident ~loc { loc; txt = long_ident }
  in
  (* Wrap the external function identifier in parentheses *)
  let paren_fn_exp = Ast_helper.Exp.pexp_paren ~loc fn_exp in
  (* Create pattern for argument: arg *)
  let arg_pat =
    Ast_helper.Pat.var ~loc { loc; txt = "arg" }
  in
  (* Apply the parenthesized external function to the argument *)
  let applied =
    Ast_helper.Exp.apply ~loc paren_fn_exp
      [ (Nolabel, Ast_helper.Exp.ident ~loc { loc; txt = Longident.Lident "arg" }) ]
  in
  (* Build the wrapper function: fun arg -> (Module.function_name) arg *)
  let fun_expr = Ast_helper.Exp.fun_ ~loc Nolabel None arg_pat applied in
  (* Bind the wrapper function to the provided name *)
  let binding =
    Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc { loc; txt = name }) fun_expr
  in
  binding