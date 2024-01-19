module type Theory=sig
  open Formula

  type axiom
  val axiom : axiom -> formula
end

module Logic(T : Theory): sig

open Formula
open Utils

(** reprezentacja twierdzeń *)
type theorem = formula list * formula

val axiom: T.axiom -> theorem

(** założenia twierdzenia *)
val assumptions : theorem -> formula list

(** teza twierdzenia *)
val consequence : theorem -> formula

(** by_assumption f konstruuje następujący dowód
  -------(Ax)
  {f} ⊢ f  *)
val by_assumption : formula -> theorem

(** imp_i f thm konstruuje następujący dowód

       thm
      Γ ⊢ φ
 ---------------(→I)
 Γ \ {f} ⊢ f → φ *)
val imp_i : formula -> theorem -> theorem

(** imp_e thm1 thm2 konstruuje następujący dowód

    thm1      thm2
 Γ ⊢ φ → ψ    Δ ⊢ φ 
 ------------------(→E)
 Γ ∪ Δ ⊢ ψ *)
val imp_e : theorem -> theorem -> theorem

(** bot_e f thm konstruuje następujący dowód

   thm
  Γ ⊢ ⊥
  -----(⊥E)
  Γ ⊢ f *)
val bot_e : formula -> theorem -> theorem

(**Zasada wprowadzania kwantyfikatora ogólnego *)
val forall_i : theorem->string->theorem

(**Zasada elminacji kwantyfikatora ogólnego *)
(* val forall_e : theorem->term->theorem *)

end