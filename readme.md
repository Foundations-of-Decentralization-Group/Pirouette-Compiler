
# Pirouette Implementation | Lexer, Parser, TypeChecker, Endpoint Projection and Ocaml Translation


We present an implementation of Pirouette _(Andrew K. Hirsch and Deepak Garg)_, the first language for typed higher-order functional choreographic programming. Pirouette offers programmers the ability to write a centralized functional program and compile it via endpoint projection into programs for each node in a distributed system. Moreover, Pirouette is defined generically over a (local) language of messages, and lifts guarantees about the message type system to its own. Message type soundness also guarantees deadlock freedom. 
### Limitations


 - ~~FunL and FunG in parser.ml and expr.ml to be replaced by Fun _(only args that a function can take are ChoreoVars)_~~
 - ~~Address a limitation where Uppercase variable names (ChoreoVars) are not recognized in OCaml translation~~
 - ~~Redundant Code in Ctrl.ml to be replaced with Expr.ml~~
 - Test coverage to be improved for all modules 
 - ~~End to end flow should be established across multiple phases~~
 - Better error handling in parser, type-checker, endpoint-projection
 - Channels can only send one type of data _(i.e if an int is sent over channel x then we can just send values of type int)_  
 - Toggle rec keyword from functions based on them being recursive
 - Function-Application implementation issue - type mismatch in different endpoint entities
 - Sender as choreovar not implemented
 - Sockets to be introduced and made more configuration driven 

## Authors

- [@shrutigupta99](https://www.github.com/ShrutiGupta99)
- [@prashantgodhwani](https://www.github.com/prashantgodhwani)
- [@akhirsch](https://www.github.com/akhirsch)

## Appendix
 
[Andrew K. Hirsch and Deepak Garg. 2022. Pirouette: higher-order typed functional choreographies](https://doi.org/10.1145/3498684)