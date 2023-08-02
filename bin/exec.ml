open Ocaml_backend
open Backend_intf
open Utils

exception InvalidProgramException of string


let ast1 = Ast{ code = 
(Controlang.Ctrl.Let (
   (Controlang.Ctrl.Ret (
      (Controlang.LocalCtrl.Variable ((Basictypes.Name "x"), Controlang.Int)),
      Controlang.Int)),
   (Controlang.Ctrl.Ret ((Controlang.LocalCtrl.INT 5), Controlang.Int)),
   (Controlang.Ctrl.Let (Controlang.Ctrl.Unit, Controlang.Ctrl.Unit,
      (Controlang.Ctrl.Branch (
         (Controlang.Ctrl.Ret (
            (Controlang.LocalCtrl.Condition (
               (Controlang.LocalCtrl.Variable ((Basictypes.Name "x"),
                  Controlang.Int)),
               Basictypes.Gt, (Controlang.LocalCtrl.INT 2), Controlang.Bool)),
            Controlang.Bool)),
         (Controlang.Ctrl.Branch (
            (Controlang.Ctrl.Ret (
               (Controlang.LocalCtrl.Condition (
                  (Controlang.LocalCtrl.Variable ((Basictypes.Name "x"),
                     Controlang.Int)),
                  Basictypes.Gt, (Controlang.LocalCtrl.INT 10),
                  Controlang.Bool)),
               Controlang.Bool)),
            (Controlang.Ctrl.Choose ((Basictypes.Direction "L"),
               (Basictypes.Location "p2"),
               (Controlang.Ctrl.Rcv (
                  (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                     Controlang.Int)),
                  (Basictypes.Location "p2"),
                  (Controlang.Ctrl.Ret (
                     (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                        Controlang.Int)),
                     Controlang.Int)),
                  Controlang.Int)),
               Controlang.Int)),
            (Controlang.Ctrl.Choose ((Basictypes.Direction "R"),
               (Basictypes.Location "p2"),
               (Controlang.Ctrl.Rcv (
                  (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                     Controlang.Int)),
                  (Basictypes.Location "p2"),
                  (Controlang.Ctrl.Ret (
                     (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                        Controlang.Int)),
                     Controlang.Int)),
                  Controlang.Int)),
               Controlang.Int)),
            Controlang.Int)),
         (Controlang.Ctrl.Branch (
            (Controlang.Ctrl.Ret (
               (Controlang.LocalCtrl.Condition (
                  (Controlang.LocalCtrl.Variable ((Basictypes.Name "x"),
                     Controlang.Int)),
                  Basictypes.Gt, (Controlang.LocalCtrl.INT 10),
                  Controlang.Bool)),
               Controlang.Bool)),
            (Controlang.Ctrl.Choose ((Basictypes.Direction "L"),
               (Basictypes.Location "p2"),
               (Controlang.Ctrl.Rcv (
                  (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                     Controlang.Int)),
                  (Basictypes.Location "p2"),
                  (Controlang.Ctrl.Ret (
                     (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                        Controlang.Int)),
                     Controlang.Int)),
                  Controlang.Int)),
               Controlang.Int)),
            (Controlang.Ctrl.Choose ((Basictypes.Direction "R"),
               (Basictypes.Location "p2"),
               (Controlang.Ctrl.Rcv (
                  (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                     Controlang.Int)),
                  (Basictypes.Location "p2"),
                  (Controlang.Ctrl.Ret (
                     (Controlang.LocalCtrl.Variable ((Basictypes.Name "z"),
                        Controlang.Int)),
                     Controlang.Int)),
                  Controlang.Int)),
               Controlang.Int)),
            Controlang.Int)),
         Controlang.Int)),
      Controlang.Int)),
   Controlang.Int)); prop = "p1"}

let ast2 = Ast{ code = 
(Controlang.Ctrl.Let (Controlang.Ctrl.Unit, Controlang.Ctrl.Unit,
   (Controlang.Ctrl.Let (
      (Controlang.Ctrl.Ret (
         (Controlang.LocalCtrl.Variable ((Basictypes.Name "y"),
            Controlang.Int)),
         Controlang.Int)),
      (Controlang.Ctrl.Ret ((Controlang.LocalCtrl.INT 10), Controlang.Int)),
      (Controlang.Ctrl.AllowLR ((Basictypes.Location "p1"),
         (Controlang.Ctrl.Snd (
            (Controlang.LocalCtrl.Plus (
               (Controlang.LocalCtrl.Variable ((Basictypes.Name "y"),
                  Controlang.Int)),
               (Controlang.LocalCtrl.INT 2), Controlang.Int)),
            (Basictypes.Location "p1"), Controlang.Ctrl.Unit, Controlang.Int
            )),
         (Controlang.Ctrl.Snd (
            (Controlang.LocalCtrl.Minus (
               (Controlang.LocalCtrl.Variable ((Basictypes.Name "y"),
                  Controlang.Int)),
               (Controlang.LocalCtrl.INT 2), Controlang.Int)),
            (Basictypes.Location "p1"), Controlang.Ctrl.Unit, Controlang.Int
            )),
         Controlang.Int)),
      Controlang.Int)),
   Controlang.Int)); prop = "p2"}


let operation lang ctrl_ast = 
   let confMap = read_config_file "config.conf" in
   match lang with
      | "ocaml" -> Ocaml_backend.main ctrl_ast confMap "bin/output.ml"
      | _ -> raise (InvalidProgramException "Invalid Program construct encountered")


(* Test the implementation *)
let () = operation "ocaml" [ast1; ast2]


