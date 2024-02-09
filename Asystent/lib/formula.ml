type term = 
  | Free  of string
  | Bound of int (*indeks de Bruijna*)
  | Func  of string * term list

type formula = 
  | Bot
  | Imp     of formula * formula
  | Rel     of string * term list
  | Forall  of string * formula

let rec string_of_term t = 
  match t with
  | Free s          -> s
  | Bound i         -> string_of_int i
  | Func (f, terms) -> f ^ "(" ^ (String.concat ", " (List.map string_of_term terms)) ^ ")"

let rec string_of_formula f = 
  match f with
  | Imp (f1, f2)    -> "(" ^ (string_of_formula f1) ^ " → " ^ (string_of_formula f2) ^ ")"
  | Bot             -> "⊥"
  | Rel (r, terms)  -> r ^ "(" ^ (String.concat ", " (List.map string_of_term terms)) ^ ")"
  | Forall (x, f)   -> "∀" ^ x ^ ".(" ^ string_of_formula f ^ ")"


