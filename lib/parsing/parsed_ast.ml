module Pos_info = struct
  type t =
    { fname : string
    ; start : int * int (* line, column *)
    ; stop : int * int (* line, column *)
    }
end

module Local = Ast_core.Local.With (Pos_info)
module Choreo = Ast_core.Choreo.With (Pos_info)
