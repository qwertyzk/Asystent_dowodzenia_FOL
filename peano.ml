open Formula
open User_formula
open Utils

module T = struct
type axiom =
| EqRefl of string (* ∀x.x = x *)
| EqElim of string * string * string * u_formula (* ∀y.∀z.y = z ⇒φ{x→y}⇒φ{x→z} *)
| PlusZ (* ∀n.0 + n = n *)
| PlusS (* ∀n.∀m.S(n) + m = S(n + m) *)
| Induction of string * u_formula

let axiom ax =
  match ax with
  | EqRefl(x) ->
      Forall(x, Rel("=", [Bound(1); Bound(1)]))
  | EqElim(n1, n2, x, f) ->
      let ff = db_convert f in
      Forall(n1,
        Forall(n2,
          Imp(Rel("=", [Bound(2); Bound(1)]),
            Imp(subst_in_formula x (Bound 2) ff,
                subst_in_formula x (Bound 1) ff))))
  | PlusZ (n) ->
    (Forall(n, Rel("=",
      [Sym("+", [Sym("zero", []); Bound(1)]);
      Bound(1)])))
  | PlusS ->
    let n = fresh_var [] in
    let m = fresh_var [Leaf(F_Var(n))] in
    de_brujin (All_u(n, All_u(m, Rel_u("=",
    [ Sym_u("+", [Sym_u("s", [Var_u(n)]); Var_u(m)])
    ; Sym_u("s", [Sym_u("+", [Var_u(n); Var_u(m)])])
    ]))))
  | Induction(x, f) ->
    let ff=de_brujin f in
    let n = fresh_var [ff] in
    Imp(
    substitute_in_formula x (Sym("z", [])) ff,
    Imp(
    All(n, Imp(
    substitute_with_d_in_formula x (B_Var 1) ff,
    substitute_with_d_in_formula x (Sym("s", [B_Var(1)])) ff)),
    de_brujin (All_u(x,f))))
  end