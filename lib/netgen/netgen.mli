(** Network code generation via endpoint projection.
    
    This module implements endpoint projection (EPP), the core transformation 
    that converts choreographic programs into executable network code for 
    individual endpoints. Given a global choreography and a target location, 
    EPP generates the local program that location should execute.
    
    Endpoint projection transforms:
    - Choreographic [send] operations → [Send] and [Recv] primitives
    - Choreographic [select] operations → [ChooseFor] and [AllowChoice] primitives
    - Location-qualified expressions → local computations
    
    This transformation ensures that distributed programs maintain the 
    communication patterns and safety properties specified in the choreography. *)

val epp_choreo_to_net
  :  'a Ast_core.Choreo.M.stmt list
  -> string
  -> 'a Ast_core.Net.M.stmt list
  (** [epp_choreo_to_net] projects choreographic statement list 
      [stmts] onto endpoint [location], producing network IR statements.
      
      Performs endpoint projection (EPP) to extract the behavior of a specific
      location from a global choreography. The resulting network IR contains
      explicit send/receive operations and choice coordination for that endpoint.
      
      Parameters:
      - [stmts]: choreographic statement list representing the global protocol
      - [location]: name of the endpoint to project for (e.g., "Alice", "Bob")
      
      Returns the network IR statement list for the specified location, with
      choreographic constructs transformed into explicit communication primitives.
      
      {b Input (Pirouette choreography):}
      {[
        x : Alice.int;
        x := [Alice] 5;
        send Alice x -> Bob;
        y : Bob.int;
        y := [Bob] x + 1
      ]}
      
      {b OCaml:}
      {[
        (* Project for Alice *)
        let alice_stmts = epp_choreo_to_net choreo_stmts "Alice" in
        
        (* Project for Bob *)
        let bob_stmts = epp_choreo_to_net choreo_stmts "Bob" in
        
        (* Visualize results *)
        print_endline "Alice's code:";
        pprint_net_ast stdout alice_stmts;
        print_endline "\nBob's code:";
        pprint_net_ast stdout bob_stmts
      ]}
      
      {b Expected (Alice's projected code):}
      {[
        x : int;
        x := Ret(5);
        Send(Ret(x), Bob)
      ]}
      
      {b Expected (Bob's projected code):}
      {[
        x : int;
        x := Recv(Alice);
        y : int;
        y := Ret(x + 1)
      ]}
      
      {b Note:} Endpoint projection preserves the semantics of the choreography:
      - Alice computes [x] locally and sends it
      - Bob receives [x] and uses it to compute [y]
      - Communication is made explicit via [Send] and [Recv]
      
      {b Raises:} May raise projection errors if the choreography cannot be 
      safely projected (e.g., location appears in conflicting branches). *)
