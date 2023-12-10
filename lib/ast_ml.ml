module rec S_P_TREE : sig
  open Local_ast.L_P_TREE

  type location = string

  type tycon = string

  and position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

  and pattern =
    | Left of pattern
    | Right of pattern
    | Pattern of (location * lpattern)
    | Pair of (pattern * pattern)
    | Var of var
    | Wild

  and sendWithBinder =  {
    sender: location;
    receiver: location;
    binder: var;
    expression: local_expr;
    then_: choreography;
  }

  and sendWithoutBinder = {
    sender: location;
    receiver: location;
    choreo: choreography;
  }

  and syncData = {
    sender: location;
    receiver: location;
    label: var;
    then_: choreography;
  }

  and letData = {
    declBlock: decl_block;
    in_: choreography;
  }

  and matchData = {
    matcher: choreography;
    branches: (pattern * choreography) list;
  }

  and pair = (choreography * choreography)

  and choreography =
    | Unit
    | ChoreoVar of {data: var; position: position}
    | Expression of {data: (location * local_expr); position: position}
    | SendWithoutBinder of {data: sendWithoutBinder; position: position}
    | SendWitBinder of {data: sendWithBinder; position: position}
    | Branch of {data: (choreography * choreography * choreography); position: position}
    | Sync of {data: syncData; position: position;}
    | Let of {data: letData; position: position;}
    | Pair of {data: pair; position: position}
    | Fst of {data: pair; position: position}
    | Snd of {data: pair; position: position}
    | Match of {data: matchData; position: position;}
    | Left of {data: matchData; position: position}
    | Right of {data: matchData; position: position}

  and assignment =
    | CType of {var: var; tycon: tycon}
    | VarAsgn of {var: (var * tycon option); location: location option; choreo: choreography}
    | Fun of {var: var; args: pattern list; choreo: choreography}

  and dec =
    | FunDec of {var: var; tycons: tycon list}
    | VarAsgnDec of {var: var; tycon: tycon; location: location option}

  and exp_dec = dec

  and decl_block_item = DecItem of dec | AssignmentItem of assignment

  and decl_block = T of {declBlockItems: decl_block_item list}

  type program = T of {declBlock: decl_block}

  (* let fileInfo = {
    style = Control.style;
    suffix = string;
    display = t Control.display;
  } *)

  (*Function toFile*)
end = S_P_TREE