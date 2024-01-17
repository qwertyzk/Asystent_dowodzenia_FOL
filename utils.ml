open Formula
open User_formula
        
module VarMap = Map.Make(String)

(* Sprawdza, czy zmienna jest wolna w termie *)
let rec free_in_term v t = 
  match t with
  | Free x -> x = v
  | Bound _ -> false
  | Func (_, terms) -> List.exists (free_in_term v) terms
 
(* Sprawdza, czy zmienna jest wolna w formule *)
let rec free_in_formula v f =
  match f with
  | Rel (_, terms) -> List.exists (free_in_term v) terms
  | Forall (x, f') -> free_in_formula v f'
  | Imp (f1, f2) -> free_in_formula v f1 || free_in_formula v f2
  | Bot -> false


(*
let rec subst_in_term sub t_sub term = 
  match term with
  | Free x -> if x = sub then t_sub else term
  | Bound _ -> term
  | Func (f, terms) ->  Func (f, List.map (subst_in_term sub t_sub) terms)
*)

(*sub to term pod ktory podstawiamy a nie nazwa zmiennej*)
let rec subst_in_term sub t_sub term = 
  match term with
  | Func (f, terms) ->  Func (f, List.map (subst_in_term sub t_sub) terms)
  | x when x = sub -> t_sub 
  | _ -> term

(*
let rec subst_in_formula sub t_sub f =
  match f with
  | Rel (r, terms) -> 
    Rel (r, List.map (subst_in_term sub t_sub) terms)
  | Imp (f1, f2) -> 
    Imp (subst_in_formula sub t_sub f1, subst_in_formula sub t_sub f2)
  | Bot -> Bot
  | Forall (x, f') ->
    if x = sub then f
    else Forall (x, subst_in_formula sub t_sub f')
*)

(**konwersja formuly podanej przez uzytkownika na formule z indeksami de Bruijna*)
let rec db_term var_map k t =
  match t with
  |UVar (s) ->
    if VarMap.mem s var_map then 
      let d = (VarMap.find s var_map) in Bound(k - d)
    else Free(s)
  |UFunc (f, terms) -> Func(f, (List.map (db_term var_map k) terms))


let db_convert f = 
  let rec convert var_map k f= 
    match f with
    |UBot -> Bot 
    |URel(r, terms) -> Rel(r, (List.map (db_term var_map k) terms))
    |UImp(f1, f2) -> Imp((convert var_map k f1),(convert var_map k f2))
    |UForall(x, f) -> Forall(x, (convert (VarMap.add x k var_map) (k+1) f)) in
  convert VarMap.empty 0 f

let rec print_term_u = function
  | UVar v -> v
  | UFunc (f, args) -> 
      f ^ "(" ^ String.concat ", " (List.map print_term_u args) ^ ")"

let rec print_formula_u = function
  | UBot -> "⊥"
  | URel (rel, args) -> 
      rel ^ "(" ^ String.concat ", " (List.map print_term_u args) ^ ")"
  | UImp (f1, f2) -> 
      "(" ^ print_formula_u f1 ^ " → " ^ print_formula_u f2 ^ ")"
  | UForall (v, f) -> 
      "∀" ^ v ^ ".(" ^ print_formula_u f ^ ")"

let rec print_term t = 
  match t with
  | Free s -> s
  | Bound i -> "BoundVar" ^ string_of_int i
  | Func (f, terms) -> f ^ "(" ^ (String.concat ", " (List.map print_term terms)) ^ ")"

let rec print_formula f = 
  match f with
  | Rel (r, terms) -> r ^ "(" ^ (String.concat ", " (List.map print_term terms)) ^ ")"
  | Forall (x, f') -> "∀" ^ x ^ ". " ^ (print_formula f')
  | Imp (f1, f2) -> "(" ^ (print_formula f1) ^ " → " ^ (print_formula f2) ^ ")"
  | Bot -> "⊥"