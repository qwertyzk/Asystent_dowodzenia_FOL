open Formula
open User_formula
open Utils

module Peano = struct
type axiom =
| EqRefl    of string (* ∀x.x = x *)
| EqElim    of string * string * string * u_formula (* ∀y.∀z. (y = z) ⇒ (φ{x→y} ⇒ φ{x→z}) *)
| PlusZ     of string (* ∀n.0 + n = n *)
| PlusS     of string * string (* ∀n.∀m.S(n) + m = S(n + m) *)
| Induction of string * string * u_formula

let axiom ax =
  match ax with
  | EqRefl(x) ->
      Forall(x, Rel("=", [Bound(0); Bound(0)]))
  | EqElim(n1, n2, x, f) ->
    let map = VarMap.empty
      |> VarMap.add n1 0
      |> VarMap.add n2 1 in 
    let dbf = convert map 2 f in
      Forall(n1,
        Forall(n2,
          Imp(
            Rel("=", [Bound(1); Bound(0)]),
            Imp(
              subst_in_formula x (Bound 1) dbf,
              subst_in_formula x (Bound 0) dbf))))
  | PlusZ (n) ->
      Forall(n,
        Rel("=",
          [ Func("+", [Func("zero", []); Bound(0)])
          ; Bound(0)]))
  | PlusS (n, m) ->
      Forall(n,
        Forall(m,
          Rel("=",
            [ Func("+", [Func("s", [Bound(1)]); Bound(0)])
            ; Func("s", [Func("+", [Bound(1); Bound(0)])])
            ])))
  | Induction(x, n, f) ->
      let dbf=db_convert f in
      Imp(
        subst_in_formula x (Func("z", [])) dbf,
        Imp(
          Forall(n, Imp(
            subst_in_formula x (Bound 0) dbf,
            subst_in_formula x (Func("s", [Bound (0)])) dbf)),
          Forall(x,dbf)))
  end