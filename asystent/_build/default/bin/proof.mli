module type Theory = sig
    open Formula
    type axiom
    val axiom:axiom->formula
end

module Proof(T : Theory) : sig

    open Formula
    open User_formula
    open Logic.Logic(T)
    open Utils

    type proof

    type goal = (string * formula) list * formula

    type proof_tree =
    | Goal    of goal
    | Lemat   of theorem
    | BotE    of goal * proof_tree
    | ImpI    of goal * proof_tree
    | ImpE    of goal * proof_tree * proof_tree
    | ForAllI of goal * proof_tree
    | ForAllE of goal * proof_tree * term

    type context = 
    | Root
    | C_BotE    of goal * context 
    | C_ImpI    of goal * context 
    | C_ImpE_L  of goal * context * proof_tree
    | C_ImpE_R  of goal * proof_tree * context 
    | C_ForAllI of goal * context 
    | C_ForAllE of goal * context * term (*na ciul ten term?*)

    (** Tworzy pusty dowód podanego twierdzenia *)
    val proof : (string * u_formula) list -> u_formula -> proof

    (** Zamienia ukończony dowód na twierdzenie *)
    val qed : proof -> theorem


    (** Jeśli dowód jest ukończony, zwraca None. W przeciwnym wypadku
    zwraca Some(Γ, φ), gdzie Γ oraz φ to odpowiednio dostępne
    założenia oraz formuła do udowodnienia w aktywnym podcelu *)
    val goal : proof -> ((string * formula) list * formula) option

    (** Przesuwa cyklicznie aktywny podcel na następny (od lewej do prawej) *)
    val next : proof -> proof

    val traverse_down : proof_tree -> context -> proof
    val traverse_up : proof_tree -> context -> proof

    (** Wywołanie intro name pf odpowiada regule wprowadzania implikacji.
    To znaczy aktywna dziura wypełniana jest regułą:

    (nowy aktywny cel)
    (name,ψ) :: Γ ⊢ φ
    -----------------(→I)
        Γ ⊢ ψ → φ

    Jeśli aktywny cel nie jest implikacją, wywołanie kończy się błędem *)
    val imp_intro : string -> proof -> proof

    (** Wywołanie apply ψ₀ pf odpowiada jednocześnie eliminacji implikacji
    i eliminacji fałszu. Tzn. jeśli do udowodnienia jest φ, a ψ₀ jest
    postaci ψ₁ → ... → ψₙ → φ to aktywna dziura wypełniana jest regułami
    
    (nowy aktywny cel) (nowy cel)
            Γ ⊢ ψ₀          Γ ⊢ ψ₁
            ----------------------(→E)  (nowy cel)
                    ...                   Γ ⊢ ψₙ
                    ----------------------------(→E)
                                Γ ⊢ φ

    Natomiast jeśli ψ₀ jest postaci ψ₁ → ... → ψₙ → ⊥ to aktywna dziura
    wypełniana jest regułami

    (nowy aktywny cel) (nowy cel)
            Γ ⊢ ψ₀          Γ ⊢ ψ₁
            ----------------------(→E)  (nowy cel)
                    ...                   Γ ⊢ ψₙ
                    ----------------------------(→E)
                                Γ ⊢ ⊥
                                -----(⊥E)
                                Γ ⊢ φ *)
    val apply : formula -> proof -> proof

    (** Wywołanie apply_thm thm pf
    działa podobnie do apply (Logic.consequence thm) pf, tyle że
    aktywna dziura od razu jest wypełniana dowodem thm.
    Nowa aktywna dziura jest pierwszą na prawo po tej, która została
    wypełniona przez thm *)

    val apply_thm : theorem -> proof -> proof

    val complete_goal : proof_tree -> context -> proof

    val complete_hole : proof -> theorem -> proof

    (** Wywołanie apply_assm name pf
    działa tak jak apply (Logic.by_assumption f) pf,
    gdzie f jest założeniem o nazwie name *)
    val apply_assm : string -> proof -> proof

    (* val pp_print_proof : Format.formatter -> proof -> unit *)

end