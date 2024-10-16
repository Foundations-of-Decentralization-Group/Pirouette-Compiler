val jsonify_choreo_stmt_block
  :  Ast.Choreo.stmt_block
  -> [> `List of
        ([> `Assoc of (string * 'a) list
         | `Bool of bool
         | `Int of int
         | `List of 'a list
         | `String of string
         ]
         as
         'a)
          list
     ]

val jsonify_net_stmt_block
  :  Ast.Net.stmt_block
  -> [> `List of
        ([> `Assoc of (string * 'a) list
         | `Bool of bool
         | `Int of int
         | `List of 'a list
         | `String of string
         ]
         as
         'a)
          list
     ]
