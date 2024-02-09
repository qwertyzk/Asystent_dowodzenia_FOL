open Formula
open User_formula

        
module VarMap = Map.Make(String)
module IntMap = Map.Make(Int)


let rec free_in_term v t = 
  match t with
  | Free x          -> x = v
  | Bound _         -> false
  | Func (_, ts)    -> List.exists (free_in_term v) ts
 

let rec free_in_formula v f =
  match f with
  | Rel (_, ts)   -> List.exists (free_in_term v) ts
  | Forall (_, f) -> free_in_formula v f
  | Imp (f1, f2)  -> free_in_formula v f1 || free_in_formula v f2
  | Bot           -> false


let is_fresh x assms =
  List.for_all (fun a->free_in_formula x (snd a)) assms


let rec db_term var_map k t =
  match t with
  | UFunc (f, ts) -> Func(f, (List.map (db_term var_map k) ts))
  | UVar s ->
      if VarMap.mem s var_map then 
        let d = (VarMap.find s var_map) in
        Bound(k - d - 1)
      else Free(s)

let rec convert var_map k f= 
  match f with
  | UBot            -> Bot 
  | URel (r, terms) -> Rel(r, (List.map (db_term var_map k) terms))
  | UImp (f1, f2)   -> Imp((convert var_map k f1),(convert var_map k f2))
  | UForall (x, f)  -> Forall(x, (convert (VarMap.add x k var_map) (k+1) f))
  
let db_convert f = 
  convert VarMap.empty 0 f


let rec rev_db_term var_map k t =
  match t with
  | Func (f, ts) -> UFunc(f, (List.map (rev_db_term var_map k) ts))
  | Free x       -> UVar x
  | Bound s      ->
    match (IntMap.find_opt (k - s - 1) var_map) with
    | Some name -> UVar name
    | None -> failwith "Failure during de Bruijn index elimination."

  
let rev_db_convert f =
  let rec convert var_map k f= 
    match f with
    | Bot            -> UBot 
    | Rel (r, terms) -> URel(r, (List.map (rev_db_term var_map k) terms))
    | Imp (f1, f2)   -> UImp((convert var_map k f1),(convert var_map k f2))
    | Forall (x, f)  -> UForall(x, (convert (IntMap.add k x var_map) (k+1) f)) in
  convert IntMap.empty 0 f

let convert_term t =
  db_term VarMap.empty 0 t
  

let rec inc_term t =
  match t with
  |Free _       -> t
  |Bound i      -> Bound (i+1)
  |Func (n, ts) -> Func(n, (List.map inc_term ts))


let rec subst_in_term v t_sub term = 
  match term with
  | Free x when x = v -> t_sub 
  | Func (f, ts)      -> Func (f, List.map (subst_in_term v t_sub) ts)
  | _                 -> term


let rec subst_in_formula v t_sub f =
  match f with
  | Bot           -> Bot
  | Rel (r, ts)   -> Rel (r, List.map (subst_in_term v t_sub) ts)
  | Imp (f1, f2)  -> Imp (subst_in_formula v t_sub f1, subst_in_formula v t_sub f2)
  | Forall (x, f) -> Forall (x, subst_in_formula v (inc_term t_sub) f)

(**porównywanie termów i formuł*)
let rec eq_term t1 t2=
  match(t1,t2) with
  | (Free(s1), Free(s2))        -> s1=s2
  | (Bound(n1), Bound(n2))      -> n1=n2
  | (Func(n1,xs), Func(n2,ys))  -> (n1=n2) && List.for_all2 (eq_term) xs ys
  | _                           -> false

let rec eq_form f1 f2= 
  match (f1,f2) with
  | (Bot,Bot)                     -> true
  | (Rel(sym1,xs), Rel(sym2,ys))  -> (sym1=sym2) &&  List.for_all2 (eq_term) xs ys
  | (Imp(f3,f4), Imp(f5,f6))      -> (eq_form f3 f5) && (eq_form f4 f6)
  | (Forall(_,f3), Forall(_,f4))  -> (eq_form f3 f4)
  | _                             -> false 



(* funkcja ktora dla kwantyfikatora usuwa go i podstawia dana zmienna
   pod jego wiazania*)

   let rec remove_bound_in_term k t_sub t =
    match t with
    | Free _              -> t
    | Bound i when i = k  -> t_sub
    | Bound _             -> t
    | Func (name, ts)     -> Func(name, List.map (remove_bound_in_term k t_sub) ts)


   let remove_bound term formula = 
    let rec help k f= 
      match f with
      | Bot            -> Bot 
      | Rel(r, terms)  -> Rel(r, (List.map (remove_bound_in_term k term) terms))
      | Imp(f1, f2)    -> Imp((help k f1),(help k f2))
      | Forall(x, f)   -> Forall (x, help (k+1) f) in
    help 0 formula


    let pp_print_Uformula fmtr f =
      Format.pp_print_string fmtr (string_of_Uformula f)
      

    let pp_print_formula fmtr f =
      pp_print_Uformula fmtr (rev_db_convert f)




      

