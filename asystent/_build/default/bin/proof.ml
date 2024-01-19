module type Theory = sig
  open Lib
  open Formula
  type axiom
  val axiom:axiom->formula
end

module Proof(T : Theory) =
struct
  open Lib
  open Formula
  open User_formula
  open Logic.Logic(T)
  open Utils

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
    | C_ForAllE of goal * context * term (*na chuj ten term?*)

  type proof = 
  | Comp of theorem
  | Incomp of goal * context (* ten goal to zawsze ma byc AKTYWNY cel*)

  (*w tym miejscu zamieniamy ludzka formula na nieludzka*)
  let proof gamma f =
    let gamma' = List.map (fun x -> ((fst x),(db_convert (snd x)))) gamma in
    let f' = db_convert f in
      Incomp ((gamma', f'), Root)

  let qed pf =
    match pf with
    | Comp th -> th
    | Incomp _ -> failwith "incomplete proof"
    
  let goal pf =
    match pf with
    | Comp _ -> None
    | Incomp (g, _) -> Some g

    (* to sie zapetli jak podamy kompletne drzewo*)
  let rec traverse_down tree ctx = 
    match tree with
    | Goal g     -> Incomp(g, ctx)
    | Lemat _    -> traverse_up tree ctx
    | BotE (g, t) -> traverse_down t (C_BotE (g, ctx))
    | ImpI(g, t)   -> traverse_down t (C_ImpI (g, ctx))
    | ImpE(g, left, right) -> traverse_down right (C_ImpE_R (g, left, ctx))
    | ForAllI(g, t) -> traverse_down t (C_ForAllI (g, ctx))
    | ForAllE(g, t, term) -> traverse_down t (C_ForAllE (g, ctx, term))

  and traverse_up tree ctx =
    match ctx with
    | Root -> traverse_down tree ctx
    | C_BotE(g, ctx)   -> traverse_up (BotE(g, tree)) ctx
    | C_ImpI(g, ctx) ->  traverse_up (ImpI(g, tree)) ctx
    | C_ImpE_L(g, ctx, right) ->  traverse_up (ImpE(g, tree, right)) ctx
    | C_ImpE_R(g, left, ctx) -> traverse_down left (C_ImpE_L (g, ctx, tree))
    | C_ForAllI(g, ctx) -> traverse_up (ForAllI(g, tree)) ctx
    | C_ForAllE(g, ctx, term) -> traverse_up (ForAllE(g, tree, term)) ctx


  (* teoretycznie mozna zaczac od up lub down, whatever, wyjdzie na to samo, ale down od razu wykrywa Goal, a my od niego zaczynamy wiec zwrocimy dostany cel, co jest bez sensu*)
  let next = function
    | Comp _            -> failwith "Cannot find next target in finished proof"
    | Incomp(g, ctx) -> traverse_up (Goal g) ctx



  let add_assum (s, f) (a : (string * formula) list) = 
    if List.exists (fun (ls, lf) -> ls = s) a
      then failwith "jest już założenie o tej nazwie" 
      else (s, f) :: a

  let imp_intro name pf =
    match pf with
    | Comp _ -> failwith "brak luk"
    | Incomp ((a, f) as g, ctx) ->
      match f with
      | Imp (p, q) -> 
        let new_g = (add_assum (name, p) a, q) in
        Incomp (new_g, (C_ImpI (g, ctx)))
      | _ -> failwith "oczekiwano Imp"

(*
    let get_ctx p =
      match p with
      | Incomp (g, ctx) -> ctx
      | Comp _ -> failwith "kkk"


      (* tu pomysl czy lepiej zachowac getter czy zmienic proof na rekord*)
    let rec apply f pf =
      match pf with 
      | Comp _ -> failwith "brak luk"
      | Incomp ((a,goal_form) as g, ctx) ->
        match f with
        | Imp (p,q) -> Incomp ((a,f), C_ImpE_L((a,q), (get_ctx (apply q pf)), (Goal (a, p))))
        | _ -> 
          if f = goal_form then (Incomp (g, ctx))
          else if f = Bot then (Incomp ((a,f), C_BotE(g, ctx)))
          else
            failwith "guwno"
  *)

    let apply formula pf =
      match pf with 
      | Comp _ -> failwith "brak luk"
      | Incomp ((a,goal_f) as g, ctx) ->
        let rec help f =
          match f with
          | Imp (p,q) -> C_ImpE_L((a,q), (help q), (Goal (a, p)))
          | Bot -> C_BotE(g, ctx)
          | _ when f = goal_f -> ctx
          | _ -> failwith "guwno"
          in
        Incomp((a,formula) , help formula)

    let toTheorem tree =
      match tree with
      | Goal (a,g)     -> ((List.map snd a), g)
      | Lemat _    -> failwith "gw"
      | BotE ((a,g), t) -> ((List.map snd a), g)
      | ImpI((a,g), t)   -> ((List.map snd a), g)
      | ImpE((a,g), left, right) -> ((List.map snd a), g)
      | ForAllI((a,g), t) -> ((List.map snd a), g)
      | ForAllE((a,g), t, term) -> ((List.map snd a), g)
  


    let rec complete_goal tree ctx = 
      match ctx with
      | Root -> Comp(toTheorem tree)
      | C_ImpE_L(g, ctx, right) ->
        begin match right with
        | Lemat l -> complete_goal (ImpE(g, Lemat(toTheorem tree), right)) ctx
        | _ -> traverse_down right (C_ImpE_R (g, Lemat(toTheorem tree), ctx))
        end
      | C_ImpE_R(g, left, ctx) -> 
        begin match left with
        | Lemat l -> complete_goal (ImpE(g, left, Lemat(toTheorem tree))) ctx
        | _ -> traverse_down left (C_ImpE_L (g, ctx, Lemat(toTheorem tree)))
        end
      | C_BotE(g, ctx)   -> complete_goal (BotE(g, Lemat(toTheorem tree))) ctx
      | C_ImpI(g, ctx) ->  complete_goal (ImpI(g, Lemat(toTheorem tree))) ctx
      | C_ForAllI(g, ctx) -> complete_goal (ForAllI(g, Lemat(toTheorem tree))) ctx
      | C_ForAllE(g, ctx, term) -> complete_goal (ForAllE(g, Lemat(toTheorem tree), term)) ctx
  
    let rec complete_hole proof theorem = 
      match proof with
      | Comp _ -> proof
      | Incomp ((a,g), ctx) ->
        if consequence theorem = g
          then complete_goal (Goal(a,g)) ctx
          else failwith "bad theorem"

    let apply_thm th pf =
      match (apply (snd th) pf) with
      | Incomp(g, ctx) as proof  -> complete_hole proof th
      | _ -> failwith "nwm"

    let apply_assm name pf =
      match pf with
    | Comp _ -> pf
    | Incomp ((a,f), ctx) ->
        let assm = List.assoc name a in
        apply_thm (by_assumption assm) pf



  let forall_intro pf =
    match pf with
    | Comp _ -> failwith "brak luk"
    | Incomp ((a,f), ctx) ->
      match f with
      | Forall (x, formula) -> 
        let g = (a, formula) in
        Incomp (g, (C_ForAllI ((a,f), ctx)))
      | _   -> failwith "oczekiwano Forall"

(*
  let forall_elim pf x t form=
    match pf with
    | Comp _ -> failwith "brak luk"
    | Incomp ((a,f), ctx) ->
        if eq_formula f (subst_in_formula x t form) 
        then
          let g = (a, Forall (x, form)) in
          Incomp (g, (C_ForAllE ((a,f), ctx)))
        else
          failwith "niepoprawne dane"
*)
    
end  