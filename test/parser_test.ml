open OUnit2
open Parser_core.Ast
open Parser_core.Parser_interface

let peq (s: string) (v: 'a) =
  assert_equal v (parse_program s)