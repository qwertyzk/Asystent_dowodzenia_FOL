module type Theory = sig
  open Formula
  type axiom
  val axiom:axiom->formula
end

module Proof(T : Theory) =
struct

  open Formula
  open Logic.Logic(T)
  open Utils
  open Format


  type goal = (string * formula) list * formula


  type proof_tree =
    | Goal of    goal
    | Lemat of   theorem
    | BotE of    goal * proof_tree
    | ImpI of    goal * proof_tree
    | ImpE of    goal * proof_tree * proof_tree
    | ForAllI of goal * proof_tree
    | ForAllE of goal * proof_tree


  type context = 
    | Root
    | C_BotE of    goal * context 
    | C_ImpI of    goal * context 
    | C_ImpE_L of  goal * context * proof_tree
    | C_ImpE_R of  goal * proof_tree * context 
    | C_ForAllI of goal * context 
    | C_ForAllE of goal * context


  type proof = 
  | Comp of   theorem
  | Incomp of goal * context


  let proof gamma f =
    let gamma' = List.map (fun x -> ((fst x),(db_convert (snd x)))) gamma in
    let f' = db_convert f in
      Incomp ((gamma', f'), Root)


  let qed pf =
    match pf with
    | Comp th   -> th
    | Incomp _  -> failwith "Proof is incomplete."
    

  let goal pf =
    match pf with
    | Comp _        -> None
    | Incomp (g, _) -> Some g


  let rec traverse_down tree ctx = 
    match tree with
    | Goal g               -> Incomp(g, ctx)
    | Lemat _              -> traverse_up tree ctx
    | BotE (g, t)          -> traverse_down t (C_BotE (g, ctx))
    | ImpI(g, t)           -> traverse_down t (C_ImpI (g, ctx))
    | ImpE(g, left, right) -> traverse_down right (C_ImpE_R (g, left, ctx))
    | ForAllI(g, t)        -> traverse_down t (C_ForAllI (g, ctx))
    | ForAllE(g, t)        -> traverse_down t (C_ForAllE (g, ctx))


  and traverse_up tree ctx =
    match ctx with
    | Root                    -> traverse_down tree ctx
    | C_BotE(g, ctx)          -> traverse_up (BotE(g, tree)) ctx
    | C_ImpI(g, ctx)          -> traverse_up (ImpI(g, tree)) ctx
    | C_ImpE_L(g, ctx, right) -> traverse_up (ImpE(g, tree, right)) ctx
    | C_ImpE_R(g, left, ctx)  -> traverse_down left (C_ImpE_L (g, ctx, tree))
    | C_ForAllI(g, ctx)       -> traverse_up (ForAllI(g, tree)) ctx
    | C_ForAllE(g, ctx)       -> traverse_up (ForAllE(g, tree)) ctx


  let next = function
    | Comp _         -> failwith "Proof is complete - no goals left."
    | Incomp(g, ctx) -> traverse_up (Goal g) ctx


  let add_assumption assm_name f assm_list = 
    if List.exists (fun (a_name, _) -> a_name = assm_name) assm_list
      then failwith "Assumption with given name already exists. Choose another name." 
      else (assm_name, f) :: assm_list

  let imp_intro name pf =
    match pf with
    | Comp _ -> failwith "Proof is already complete."
    | Incomp ((assm_list, f) as g, ctx) ->
      match f with
      | Imp (p, q) -> 
        let new_g = ((add_assumption name p assm_list), q) in
        Incomp (new_g, (C_ImpI(g, ctx)))
      | _ -> failwith "Expected implication."


  let apply_form formula pf =
    match pf with 
    | Comp _ -> failwith "Proof is already complete."
    | Incomp ((assm_list, pf_goal) as g, ctx) ->
      let rec help f =
        match f with
        | Imp (p,q)          -> C_ImpE_L((assm_list,q), (help q), (Goal (assm_list, p)))
        | Bot                -> C_BotE(g, ctx)
        | _ when (eq_form f pf_goal) -> ctx
        | _                  -> failwith "Wrong formula."
        in
      Incomp((assm_list,formula) , help formula)

  let apply u_form pf = 
    let dbf = db_convert u_form in
    apply_form dbf pf

  


  let toTheorem tree =
    match tree with
    | Goal (a,g)               -> ((List.map snd a), g)
    | Lemat _                  -> failwith "co"
    | BotE ((a,g), _)          -> ((List.map snd a), g)
    | ImpI((a,g), _)           -> ((List.map snd a), g)
    | ImpE((a,g), _, _) -> ((List.map snd a), g)
    | ForAllI((a,g), _)        -> ((List.map snd a), g)
    | ForAllE((a,g), _)        -> ((List.map snd a), g)


  let rec complete_goal tree ctx = 
    match ctx with
    | Root -> Comp(toTheorem tree)
    | C_BotE(g, ctx)          -> complete_goal (BotE(g, Lemat(toTheorem tree))) ctx
    | C_ImpI(g, ctx)          -> complete_goal (ImpI(g, Lemat(toTheorem tree))) ctx
    | C_ForAllI(g, ctx)       -> complete_goal (ForAllI(g, Lemat(toTheorem tree))) ctx
    | C_ForAllE(g, ctx)       -> complete_goal (ForAllE(g, Lemat(toTheorem tree))) ctx
    | C_ImpE_L(g, ctx, right) ->
      begin match right with
      | Lemat _   -> complete_goal (ImpE(g, Lemat(toTheorem tree), right)) ctx
      | _         -> traverse_down right (C_ImpE_R (g, Lemat(toTheorem tree), ctx))
      end
    | C_ImpE_R(g, left, ctx)  -> 
      begin match left with
      | Lemat _   -> complete_goal (ImpE(g, left, Lemat(toTheorem tree))) ctx
      | _         -> traverse_down left (C_ImpE_L (g, ctx, Lemat(toTheorem tree)))
      end


  let complete proof theorem = 
    match proof with
    | Comp _ -> failwith "Proof is already complete."
    | Incomp ((_ , pf_goal) as g, ctx) ->
      if consequence theorem = pf_goal
        then complete_goal (Goal(g)) ctx
        else failwith "Wrong theorem."

  let apply_thm th pf =
    match pf with
    | Comp _    -> failwith "Proof is already complete."
    | Incomp _  -> 
        let incomplete_pf = (apply_form (consequence th) pf) in
        complete incomplete_pf th

  let apply_assm name pf =
    match pf with
  | Comp _              -> pf
  | Incomp ((a,_), _) ->
      let assm = List.assoc name a in
      apply_thm (by_assumption assm) pf



  let forall_intro pf fresh =
    match pf with
    | Comp _ -> failwith "Proof is already complete."
    | Incomp ((a,f), ctx) ->
      match f with
      | Forall (_, ff) -> 
        if is_fresh fresh a then
          let f' = remove_bound (Free fresh) ff in
          Incomp ((a,f'), (C_ForAllI ((a,f), ctx)))
        else failwith "Not a fresh variable."
      | _   -> failwith "Expected Forall."


  let forall_elim pf t form=
    match pf with
    | Comp _ -> failwith "Proof is already complete."
    | Incomp ((a,f), ctx) ->
      let dbf = db_convert form in
      match dbf with 
      | Forall(_, ff) ->
        let dbt = convert_term t in
        let f' = remove_bound dbt ff in
        if eq_form f f' then
          let g = (a, dbf) in
          Incomp (g, (C_ForAllE ((a,f), ctx)))
        else failwith "Wrong formula."
      | _ -> failwith "Expected Forall."
    
    

  let pp_print_assumption fmtr (var, dbf) =
    let u_form = rev_db_convert dbf in
    fprintf fmtr "(%s, %a)" var pp_print_Uformula u_form


  let pp_print_assumptions fmtr assms =
    pp_open_hvbox fmtr 2;
    begin match assms with
    | [] -> ()
    | (tag, f) :: fs ->
      pp_print_string fmtr "[";
      pp_print_assumption fmtr (tag, f);
      List.iter (fun (tag, f) ->
        pp_print_string fmtr ",";
        pp_print_space fmtr ();
        pp_print_assumption fmtr (tag, f)
      ) fs;
      pp_print_string fmtr "]";
      pp_print_space fmtr ()
    end;
    pp_close_box fmtr ()
          
  let pp_print_goal fmtr (assm, dbf) =
    let u_form = rev_db_convert dbf in
    fprintf fmtr "[%a] ⊢ %a" pp_print_assumptions assm pp_print_Uformula u_form


  let rec pp_print_proof_tree fmtr tree =
    pp_open_hvbox fmtr 2;
    begin
      match tree with
      | Goal goal -> fprintf fmtr "Goal: %a" pp_print_goal goal
      | Lemat theorem -> fprintf fmtr "Lemat: %a" pp_print_theorem theorem
      | BotE (goal, subtree) ->
        fprintf fmtr "BotE: %a,@ %a" pp_print_goal goal pp_print_proof_tree subtree
      | ImpI (goal, subtree) ->
        fprintf fmtr "ImpI: %a,@ %a" pp_print_goal goal pp_print_proof_tree subtree
      | ImpE (goal, left_subtree, right_subtree) ->
        fprintf fmtr "ImpE: %a,@ %a,@ %a" pp_print_goal goal pp_print_proof_tree left_subtree pp_print_proof_tree right_subtree
      | ForAllI (goal, subtree) ->
        fprintf fmtr "ForAllI: %a,@ %a" pp_print_goal goal pp_print_proof_tree subtree
      | ForAllE (goal, subtree) ->
        fprintf fmtr "ForAllE: %a,@ %a" pp_print_goal goal pp_print_proof_tree subtree
    end;
    pp_close_box fmtr ()


  let rec pp_print_context fmtr context =
    match context with
    | Root -> ()
    | C_BotE (goal, ctx) ->
      fprintf fmtr "BotE: %a,@ %a" pp_print_goal goal pp_print_context ctx
    | C_ImpI (goal, ctx) ->
      fprintf fmtr "ImpI: %a,@ %a" pp_print_goal goal pp_print_context ctx
    | C_ImpE_L (goal, ctx, subtree) ->
      fprintf fmtr "ImpE_L: %a,@ %a,@ %a" pp_print_goal goal pp_print_context ctx pp_print_proof_tree subtree
    | C_ImpE_R (goal, subtree, ctx) ->
      fprintf fmtr "ImpE_R: %a,@ %a,@ %a" pp_print_goal goal pp_print_proof_tree subtree pp_print_context ctx
    | C_ForAllI (goal, ctx) ->
      fprintf fmtr "ForAllI: %a,@ %a" pp_print_goal goal pp_print_context ctx
    | C_ForAllE (goal, ctx) ->
      fprintf fmtr "ForAllE: %a,@ %a" pp_print_goal goal pp_print_context ctx
  

  let pp_print_proof fmtr proof =
    match proof with
    | Comp theorem ->
      pp_open_hvbox fmtr 2;
      fprintf fmtr "Proven:@ %a" pp_print_theorem theorem;
      pp_close_box fmtr ()
    | Incomp (goal, context) ->
      pp_open_vbox fmtr 2;
      fprintf fmtr "Unproven:@ %a,@ %a" pp_print_goal goal pp_print_context context;
      pp_close_box fmtr ()
  
end  