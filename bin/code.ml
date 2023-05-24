(* LEXER

type token =
  | LParen   (* ( *)
  | RParen   (* ) *)
  | Lambda   (* λ *)
  | Dot      (* . *)
  | Identifier of string  (* variable name *)

let rec tokenize_input = function
  | [] -> []
  | '(' :: rest -> LParen :: tokenize_input rest
  | ')' :: rest -> RParen :: tokenize_input rest
  | '|' :: rest -> Lambda :: tokenize_input rest
  | '.' :: rest -> Dot :: tokenize_input rest
  | ' ' :: rest -> tokenize_input rest
  | c :: rest when is_alpha c ->
      let identifier, remaining_input = extract_identifier (Char.escaped c) rest in
      Identifier identifier :: tokenize_input remaining_input
  | _ :: rest -> tokenize_input rest

and is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

and extract_identifier identifier = function
  | [] -> identifier, []
  | c :: rest when is_alpha c -> extract_identifier (identifier ^ Char.escaped c) rest
  | rest -> identifier, rest


(* PARSER *)

type expression =
  | Variable of string
  | Abstraction of string * expression
  | Application of expression * expression


(* let rec parse_expression : token list -> (expression * token list) = function
  | [] -> failwith "Unexpected end of input"
  | [Identifier x] -> (Variable x, [])
  | Identifier x :: Dot :: rest ->
      let body, rem_tokens = parse_expression rest in 
        (Abstraction (x, body), rem_tokens)
  | Identifier x :: rest ->
    let body, rem_tokens = parse_expression rest in (
      match body with
      | y  -> (Application (Variable x,  y), rem_tokens)
    )
  | LParen :: rest ->
      let exp1, remaining_input = parse_application rest in (
        match remaining_input with
          | RParen :: rest' -> (exp1, rest')
          | _ -> failwith "Mismatched parentheses"
      )
  | Lambda :: rest -> parse_expression rest
  | _ -> failwith "Invalid expression"

  and parse_application tokens =
    let exp1, remaining_input = parse_expression tokens in
      match remaining_input with
        | [] -> exp1, []
        | RParen :: rest -> exp1, RParen :: rest
        | exp2_tokens ->
            let exp2, rest = parse_application exp2_tokens in
            Application (exp1, exp2), rest *)
          
let rec parse_expression = function
  | [] -> failwith "Unexpected end of input"
  | Identifier x :: rest ->
      (match rest with
      | Dot :: rest' ->
          let body = parse_expression rest' in
          Abstraction (x, body)
      | [] -> Variable x
      | _ -> parse_application (Variable x) rest)
  | LParen :: rest ->
      let exp, remaining_input = parse_application (Variable "") rest in
      (match remaining_input with
      | RParen :: rest' -> exp
      | _ -> failwith "Mismatched parentheses")
  | _ -> failwith "Invalid expression"

and parse_application func rem = function
  | [] -> func, []
  | RParen :: rest -> func, RParen :: rest
  | tokens ->
      let arg, remaining_input = parse_expression tokens in
      parse_application (Application (func, arg)) remaining_input

(* EXAMPLE USAGE *)

let input : string = "(|x.x)"

let tokens : token list = tokenize_input (List.of_seq (String.to_seq input));;

let parsed_expression, rem_tokens  = parse_expression tokens

(* Printing the parsed expression *)
let rec string_of_expression : expression -> string = function
  | Variable x -> x
  | Abstraction (x, body) -> "λ" ^ x ^ ". " ^ string_of_expression body
  | Application (e1, e2) -> "(" ^ string_of_expression e1 ^ " " ^ string_of_expression e2 ^ ")"

let output = string_of_expression parsed_expression;;

(* Output: "(λx. (x x)) (λy. y)" *)
print_endline output
 *)
