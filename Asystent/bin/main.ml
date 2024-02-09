(* open Asystent
open Peano
open User_formula 



open Proof.Proof(Peano)

(* 
let t = UForall("x", UForall("y", UImp(URel("=", [UVar "x"; UVar "y"]), URel("=", [UVar "y"; UVar "x"]))));;
let pf = proof [] t;;
let pf = forall_intro pf "x";;
let pf = forall_intro pf "y";;
let pf = imp_intro "H1" pf;;
let t = UImp(URel("=", [UVar "x"; UVar "y" ]),UImp(URel("=", [UVar "x"; UVar "x"]), URel("=", [UVar "y"; UVar "x"])) );;
let pf = apply t pf;;

let t = UForall("y", UImp(URel("=", [UVar "x"; UVar "y" ]),UImp(URel("=", [UVar "x"; UVar "x"]), URel("=", [UVar "y"; UVar "x"])) ));;
let pf = forall_elim pf (UVar "y") t;;

let t = UForall("x", UForall("y", UImp(URel("=", [UVar "x"; UVar "y" ]),UImp(URel("=", [UVar "x"; UVar "x"]), URel("=", [UVar "y"; UVar "x"])) )));;
let pf = forall_elim pf (UVar "x") t;;

let a =  EqElim("x", "y", "a", (URel ("=",[ UVar "a"; UVar "x"])));;

let pf = apply_axiom a pf;;

let pf = apply_assm "H1" pf;;

let pf = forall_elim pf (UVar "x") (UForall("x", URel("=", [UVar "x"; UVar "x"])));;

let pf =  apply_axiom (EqRefl "x") pf;;






let a = EqRefl "m";;
let a = axiom a;;
let f = UForall("a", URel("=", [UVar "a"; UVar "a"]));;
let pf = proof [] f;;
let pf = apply_thm a pf;;

*)