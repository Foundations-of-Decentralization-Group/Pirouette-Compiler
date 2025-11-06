(** {b Module Ast_core.Choreo:} Core choreography abstract syntax tree definitions: This module defines the AST structures for choreographic programs,
    including types, patterns, expressions, and statements that operate
    across multiple distributed locations. 
  *)

(** {b module M:} This module defines the AST structures for choreographic programs.
All AST nodes are parameterized by a metadata type ['a], allowing different
    compiler passes to attach their own annotations (source locations, type 
    information, etc.). *)
module M : sig

  (** {b Type identifiers} [typ_id] (name + metadata). *)

  type 'a typ_id = Typ_Id of string * 'a
(** 
    Internal AST Structure: [Typ_Id(name, meta)]  
    - [name]: type name as a string (e.g., "Sum", "Result").  
    - [meta]: node metadata (source, typing info, etc.).

    Pirouette Syntax: 
    {[
      let tid = Typ_Id ("Result", unit)
    ]}

    Ocaml:
    *)

   (** {b Choreographic types} ['a typ] represent the types of values and communications
    in a choreography. Each node carries metadata of type ['a], allowing compiler
    passes to attach annotations such as source locations or type information.*)

  type 'a typ =
    | TUnit of 'a
    (** Unit type: represents the absence of a meaningful value.
    
    Internal AST Structure: [TUnit(metadata)]
        - [metadata]: node metadata
    
    Pirouette Syntax:
      {[
        (* Function returning unit *)
        print_message : string -> unit
        
        (* Function taking unit as argument *)
        get_time : unit -> int
      ]}
    
    OCaml:
      {[
        let unit_type = TUnit(())
        in
        (* unit_type now represents the unit type in the AST *)
        unit_type
      ]} *)

    | TLoc of 'a Local.M.loc_id * 'a Local.M.typ * 'a
    (** Location-qualified type: a value of a local type at a specific location

    Internal AST Structure: [TLoc(location, local_type, meta)]
        - [location]: the location identifier
        - [local_type]: the local type at that location
        - [meta]: node metadata

    Pirouette Syntax:
      {[
        Alice.int
      ]}

    Ocaml:
      {[
        let alice_int = 
          TLoc(Local.M.Loc_Id("Alice", ()), Local.M.TInt(()), ()) (* () is unit type metadata*)
        in
        alice_int
      ]} *)

    | TVar of 'a typ_id * 'a
    (** Type variable 

    Internal AST Structure: [TVar(type_id, meta)]

    Pirouette Syntax:
      {[
        'a
      ]}
        
    Ocaml:
    *)

    | TMap of 'a typ * 'a typ * 'a
    (** Function type (domain -> codomain)

    Internal AST Structure: [TMap(domain, codomain, meta)]

    Pirouette Syntax:
      {[
        int -> string
      ]}*)

    | TProd of 'a typ * 'a typ * 'a
    (** Product type (pair).

    Internal AST Structure: [TProd(left, right, meta)]

    Pirouette Syntax:
      {[
        int * string
      ]}
        
    Ocaml:
    *)

    | TSum of 'a typ * 'a typ * 'a
    (** Sum type: tagged union representing a choice between two alternatives.
          
      A value is either Left (first type) or Right (second type), but not both.

      Internal AST Structure: [TSum(left, right, meta)]

      Pirouette Syntax:
      {[          
        int + string            (* either int or string *)
        bool + unit             (* either bool or unit *)
        (int * int) + string    (* either a pair or a string *)
      ]}
        
      Ocaml:
      {[
        let int_or_string = 
          TSum(TInt(()), TString(()), ())
        in
        int_or_string
      ]}*)

  (** {b Choreographic patterns} ['a pattern] for destructuring values, annotated with metadata of type ['a].
      Patterns can match values distributed across multiple locations. *)

  type 'a pattern =
    | Default of 'a
    (** Wildcard that matches any data and binds nothing. 

    Internal AST Structure: [Default(meta)]

    Pirouette Syntax:
      {[
        _
      ]}*)

    | Var of 'a Local.M.var_id * 'a
    (** Variable binding 

    Internal AST Structure: [Var(var_id, meta)]

    Pirouette Syntax:
      {[
        x
      ]}*)

    | Pair of 'a pattern * 'a pattern * 'a
    (** Pair pattern for destructuring products. Contains a pattern for the first 
        element, a pattern for the second element, and metadata. 
        
      Internal AST Structure: [Pair(left_pattern, right_pattern, meta)]

    Pirouette Syntax:
      {[
        (x, y)
      ]}
      The pattern matcher recursively works through both sub-patterns to destructure nested pairs. *)

    | LocPat of 'a Local.M.loc_id * 'a Local.M.pattern * 'a
    (** Location-qualified pattern: matches a local pattern at a specific location 

    Internal AST Structure: [LocPat(location, local_pattern, meta)]

    Pirouette Syntax:
      {[
        Alice.x
      ]}*)
    | Left of 'a pattern * 'a
    (** Left constructor of sum type. 
    
    Internal AST Structure: [Left(pattern, meta)]

    Pirouette Syntax:
      {[
        Left x
      ]}
    
    Ocaml:
      *)

    | Right of 'a pattern * 'a
    (** Right constructor of sum type.

      Internal AST Structure: [Right(pattern, meta)]

    Pirouette Syntax:
      {[
        Right x
      ]}
        
    Ocaml:
    *)

  (** {b Choreographic expressions} ['a expr] annotated with metadata of type ['a].
      
      Expressions describe computations and communications in a choreography,
      including message passing between locations. *)

  type 'a expr =
    | Unit of 'a
    (** Unit value 
    
    Internal AST Structure: [Unit(meta)]

    Pirouette Syntax:
      {[
        ()
      ]}
    
    Ocaml:
      *)

    | Var of 'a Local.M.var_id * 'a
    (** Variable reference.

      Internal AST Structure: [Var(var_id, meta)]

    Pirouette Syntax:
      {[
        x
      ]}
        
    Ocaml:
    *)

    | LocExpr of 'a Local.M.loc_id * 'a Local.M.expr * 'a
    (** Location-qualified expression: a local expression at a specific location 
    
    Internal AST Structure: [LocExpr(location, local_expr, meta)]

    Pirouette Syntax:
      {[
        Alice.x
      ]}
    
    Ocaml:  *)

    | Send of 'a Local.M.loc_id * 'a expr * 'a Local.M.loc_id * 'a
    (** Message send:

      Internal AST Structure: [Send(sender, value, receiver, meta)]
        - [sender]: origin location
        - [value]: expression being transmitted
        - [receiver]: destination location
        - [meta]: node metadata (source span, type info)

    Pirouette Syntax:
      {[
        send Alice 42 -> Bob
        ]}

    Ocaml: 
      {[let s = Send ("Alice", Int 42, "Bob", m_send)
      ]}*)

    | Sync of 'a Local.M.loc_id * 'a Local.M.sync_label * 'a Local.M.loc_id * 'a expr * 'a
    (** Synchronizing choices: transfers control flow, One location makes a choice and synchronizes with another location about which 
    branch to take.

    Internal AST Structure: [Sync(chooser, label, peer, cont, meta)]
      - [chooser]: location making the choice
      - [label]: selected branch label
      - [peer]: location to be informed
      - [cont]: continuation expression after synchronization
      - [meta]: metadata for this sync
      
      
    Pirouette Syntax:
      {[
        select Alice Ready -> Bob; <continuation>
      ]}
        
    Ocaml:
    *)

    | If of 'a expr * 'a expr * 'a expr * 'a
    (** Conditional expression: Standard if-then-else conditional
    
    Internal AST Structure: [If(cond, then_expr, else_expr, meta)]

    Pirouette Syntax:
      {[
        if cond then e1 else e2
      ]}

    Ocaml:
    *)

    | Let of 'a stmt_block * 'a expr * 'a
    (** Let binding of a statement block: Introduces local bindings 
    
    Internal AST Structure: [Let(stmt_block, body, meta)]

    Pirouette Syntax:
      {[
        let x = 5 in x + 1
      ]}
        
    Ocaml:
    *)

    | FunDef of 'a pattern list * 'a expr * 'a
    (** Function definition 
    
    Internal AST Structure: [FunDef(params, body, meta)]

    Pirouette Syntax:
      {[
        fun x y -> x + y
      ]}
        
    Ocaml:
    *)

    | FunApp of 'a expr * 'a expr * 'a
    (** Function application.

      Internal AST Structure: [FunApp(function_expr, argument_expr, meta)]

    Pirouette Syntax:
      {[
        add 3 (*fuction(add)arguement(3)*)
      ]}
        
    Ocaml:
    *)

    | Pair of 'a expr * 'a expr * 'a
    (** Pair construction: Creates a pair value containing two expressions. 
      Internal AST Structure: [Pair(left_expr, right_expr, meta)]

    Pirouette Syntax:
      {[
        (x, y)
      ]} 
        
    Ocaml:
    *)

    | Fst of 'a expr * 'a
    (** First projection from pair: extracts the first element from a pair expression.
    
    Internal AST Structure: [Fst(pair_expr, meta)]

    Pirouette Syntax:
      {[
        fst p
      ]}

    Ocaml:
    {[
      let p = (5, "hello")
      let x = fst p     (* x = 5 *)
    ]} *)

    | Snd of 'a expr * 'a
    (** Second projection from pair: extracts the second element from a pair expression.
    
    Internal AST Structure: [Snd(pair_expr, meta)]

    Pirouette Syntax:
      {[
        snd p
      ]}

    Ocaml:
    {[
      let p = (5, "hello")
      let y = snd p     (* y = "hello" *)
    ]} *)

    | Left of 'a expr * 'a
    (** Left injection into sum type: constructs a Left value containing the given expression.
      
      The ['a expr] can be any expression type. The result is a sum type value tagged as Left.

      Internal AST Structure:[Left(payload, meta)]
      - [payload]: any expression; the *resulting value* is tagged as Left.
      - [meta]: metadata for the Left node.

      Pirouette Syntax:
      {[
        Left 5
      ]}

    Ocaml:
      {[
      Left (Int 5, m) 
      ]}*)

    | Right of 'a expr * 'a
    (** Right injection into sum type: constructs a Left value containing the given expression.
      
    The ['a expr] can be any expression type. The result is a sum type value tagged as Right.

    Internal AST Structure:[Right(payload, meta)]
    - [payload]: any expression; the resulting value is tagged as Right.
    - [meta]: metadata for the Right node.

    Pirouette Syntax:
      {[
      Right "hello"
      ]}    

    Ocaml:
      {[
        Right (String "hello", m) 
      ]}*)

    | Match of 'a expr * ('a pattern * 'a expr) list * 'a
    (** Pattern matching expression: matches an expression against a list of pattern-case pairs.
      
      The first ['a expr] is the value to match, the list contains pattern-case pairs where
      each pattern is tried in order until one matches, then its corresponding expression is evaluated.
      
      Requires: At least one pattern must match the value (exhaustiveness).
      Raises: Non-exhaustive matches are allowed in the AST; a non-exhaustive match may raise Match_failure.
      
      Internal AST Structure: [Match(expr, cases, meta)]
        - [expr]: expression to inspect
        - [cases]: list of (pattern, expression) pairs
        - [meta]: metadata for the match node

      Pirouette Syntax:

      Ocaml:
      {[
        match x with
        | Left v -> v + 1
        | Right v -> v - 1
      ]} *)

(** Choreographic statements annotated with metadata of type ['a]. 
    Statements declare variables, types, and perform assignments. *)
  and 'a stmt =
    | Decl of 'a pattern * 'a typ * 'a
    (** Type declaration: declares that a pattern has a specific type.
        
    Internal AST Structure: [Decl(pattern, typ, meta)]
        
    Pirouette Syntax:
        {[
          x : int
        ]} 
          
    Ocaml:
    *)

    | Assign of 'a pattern list * 'a expr * 'a
    (** Assignment: binds patterns to the result of evaluating an expression.
        
      The ['a pattern list] contains patterns to bind, ['a expr] is the value to assign,
      and ['a] is metadata. Multiple patterns allow destructuring of tuple results.
        
      Internal AST Structure: [Assign(patterns, expr, meta)]

      Pirouette Syntax:
        {[
          x := 5
          (x, y) := (1, 2)
        ]} 
          
      Ocaml:
      *)

    | TypeDecl of 'a Local.M.typ_id * 'a typ * 'a
    (** Type alias declaration: defines a new type name for an existing type.
        
    Internal AST Structure: [TypeDecl(type_id, typ, meta)]
        
      Pirouette Syntax:
        {[
          type Result = int * int
        ]} 
      
      Ocaml:
        *)

    | ForeignDecl of 'a Local.M.var_id * 'a typ * string * 'a
    (** Foreign function declaration: declares an external function with its type and external name.
        
    Internal AST Structure: [ForeignDecl(var_id, typ, external_name, meta)]

      The ['a Local.M.var_id] is the internal name, ['a typ] is its type, 
      [string] is the external name (e.g., from C library), and ['a] is metadata.
        
      Pirouette Syntax:
        {[
          foreign sqrt : float -> float = "sqrt"
        ]} 
          
      Ocaml:
      *)

(** A block of statements forming a sequence of declarations and assignments.
    Statements in a block are executed in order, with each statement potentially
    introducing bindings visible to subsequent statements. *)
  and 'a stmt_block = 'a stmt list
end

(** {b module With (Info : sig ... end):} Functor for creating AST types with concrete metadata. A functor is a parametrized module that takes one or more modules 
as arguments and returns a new module as a result.

The polymorphic AST types (['a typ], ['a expr], etc.) allow any metadata type ['a].
    This functor instantiates those types with a specific metadata type [Info.t], making
    the AST concrete and providing utilities to access and modify the metadata annotations.
    
    {b Why use this functor?}
    
    Different compiler passes need different metadata:
    - Parser attaches source locations
    - Type checker attaches type information
    - Code generator attaches target-specific data
    
    This functor lets each pass work with its own metadata type while reusing
    the same AST structure.*)
module With : functor
    (Info : sig
       type t (** The concrete metadata type for this instantiation. AST types with [Info.t] metadata *)
     end)
    -> sig
  type nonrec typ_id = Info.t M.typ_id
  type nonrec typ = Info.t M.typ
  type nonrec pattern = Info.t M.pattern
  type nonrec expr = Info.t M.expr
  type nonrec stmt = Info.t M.stmt
  type nonrec stmt_block = stmt list

  (** [get_info_typid tid] is the metadata annotation from type identifier [tid].*)
  val get_info_typid : typ_id -> Info.t
  
  (** [get_info_typ t] is the metadata annotation from type [t].*)
  val get_info_typ : typ -> Info.t

  (** [get_info_pattern p] is the metadata annotation from pattern [p].*)
  val get_info_pattern : pattern -> Info.t
  
  (** [get_info_expr e] is the metadata annotation from expression [e].*)
  val get_info_expr : expr -> Info.t
  
  (** [get_info_stmt s] is the metadata annotation from statement [s].*)
  val get_info_stmt : stmt -> Info.t
  
  (** [set_info_typid info tid] is type identifier [tid] with its metadata replaced by [info].*)
  val set_info_typid : Info.t -> typ_id -> typ_id
  
  (** [set_info_typ info t] is type [t] with its metadata replaced by [info].*)
  val set_info_typ : Info.t -> typ -> typ
  
  (**[set_info_pattern info t] is pattern [p] with its metadata replaced by [info].*)
  val set_info_pattern : Info.t -> pattern -> pattern
  
  (** [set_info_expr info e] is expression [e] with its metadata replaced by [info].*)
  val set_info_expr : Info.t -> expr -> expr
  
  (** [set_info_stmt info s] is statement [s] with its metadata replaced by [info].*)
  val set_info_stmt : Info.t -> stmt -> stmt
  

end
