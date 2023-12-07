module type S_P_TREE = sig
  module Position : sig
    (*pos_fname is the file name;
      pos_lnum is the line number; 
      pos_bol is the offset of the beginning of the line (number of characters between the beginning of the lexbuf and the beginning of the line); 
      pos_cnum is the offset of the position (number of characters between the beginning of the lexbuf and the position).  
    *)
    type t = {
      pos_fname : string;
      pos_lnum : int;
      pos_bol : int;
      pos_cnum : int;
    }
    type position = (t * t)
  end

  module Pattern : sig
    type t =
      | Left of t
      | Right of t
      | LocalPattern of (Location.t * LPattern.t)
      | Pair of (t * t)
      | Var of Var.t
      | Wild
  end
  
  module Choreography : sig
    type sugaredSend = 
      | SendWithBinder of {
          sender: Location.t;
          receiver: Location.t;
          binder: Var.t;
          expression: LocalExpr.t;
          then_: t
        } 
      | SendWOBinder of {
          sender: Location.t;
          receiver: Location.t;
          expression: LocalExpr.t;
        } 

    (*add with to make mutually recursive *)
    and t =
      | Unit
      (*Removing tycon: Tycon.t option because the type should be declared in Dec*)
      | ChoreoVar of {data: Var.t; position: Position.position}
      (*Removing tycon: Tycon.t option because the type should be declared in Dec*)
      | Expression of {data: (Location.t * LocalExpr.t); 
          position: Position.position}
      (*Removing tycon: Tycon.t option because the type should be declared in Dec*)
      | Send of {data: sugaredSend; position: Position.position}
      (*Removing tycon: Tycon.t option because the type should be declared in Dec*)
      | Branch of {
          data: (t * t * t); 
          position: Position.position
        }
      | Sync of {
          data: {
            sender: Location.t;
            receiver: Location.t;
            label: Var.t;
            then_: t
          };
          position: Position.position
        }
      | Let of {
        data: {
          declBlock: DeclBlock.t;
          in_: t
        },
        position: Position.position
      }
      | Pair of {
          data of t * t,
          position: Position.position
        }
      | Fst of {
        data of Pair.t,
        position: Position.position
      }
      | Snd of {
        data of Pair.t,
        position: Position.position
      }
      | Match of {
          data: {
            matcher: t;
            branches: (Pattern.t * t) list
          },
          position: Position.position
        }
      | Left of {
        data of Match.t,
        position: Position.position
      }
      | Right of {
        data of Match.t,
        position: Position.position
      }
  end

  module Assignment : sig
    type t =
      | CType of {var: Var.t; tycon: Tycon.t}
      | VarAsgn of {(Var.t * Tyvar.t option); location: Location.t option; choreo: Choreography.t}
      | Fun of {var: Var.t; args: Pattern.t list; choreo: Choreography.t}
  end

  module Dec : sig
    type t =
      | Fun of {var: Var.t; tyvars: Tyvar.t list}
      | VarAsgn of {var: Var.t; tyvar: Tyvar.t; location: Location.t option}
  end

  sharing type Exp.dec = Dec.t

  module DeclBlock : sig
    type declBlockItem = DecItem of Dec.t | AssignmentItem of Assignment.t

    type t = T of {declBlockItems: declBlockItem list}
  end

  module Program : sig
    type t = T of {declBlock: DeclBlock.t}

    val fileInfo : {
      style: Control.style;
      suffix: string;
      display: t Control.display;
    }

    (*Function toFile*)
  end
end
