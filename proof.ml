module type Theory = sig
  open Formula
  type axiom
  val axiom:axiom->formula
end

module Proof(T : Theory) =
struct
  open Formula
  open User_formula
  open Logic.Logic(T)
  open Utils

  type goal = (string * formula) list * formula


  type proof_tree =
    | Goal of goal
    | Lemat of theorem
    | BotE of goal * proof_tree
    | ImpI of goal * proof_tree
    | ImpE of goal * proof_tree * proof_tree
    | ForAllI of goal * proof_tree
    | ForAllE of goal * proof_tree * term

  type context = 
    | Root
    | C_BotE of   goal * context 
    | C_ImpI of   goal * context 
    | C_ImpE_L of goal * context * proof_tree
    | C_ImpE_R of goal * proof_tree * context 
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
    | Incomp (_, g) -> Some g

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


  (* teoretycznie mozna zaczac od up lub down, whatever, wyjdzie na to samo, ale down od razu wykrywa Goal, a my od niego zaczynamy wiec zwrocimy dostany cel*)
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
          else failwith "guwno"
          
    

  let forall_intro pf =
    match pf with
    | Comp _ -> failwith "brak luk"
    | Incomp (ctx, {f; a}) ->
      match f with
      | Forall (x, formula) -> 
        let g = {f = formula; a} in
        Incomp ((C_ForAllI {a; f; ctx}), (Goal g))
      | _   -> failwith "oczekiwano Forall"


  let forall_elim pf x t form=
    match pf with
    | Comp _ -> failwith "brak luk"
    | Incomp (ctx, {f; a}) ->
        if eq_formula f (subst_in_formula x t form) 
        then
          let g = {f = Forall (x, form); a} in
          in Incomp ((C_ForAllE {a; f; ctx}), (Goal g))
        else
          failwith "niepoprawne dane"

    
end  