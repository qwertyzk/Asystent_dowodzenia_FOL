(*definicja formuł podawanych przez uzytkownika*)
type u_term =
  |UVar   of string
  |UFunc  of string * u_term list 

type u_formula =
  |UBot
  |UImp     of u_formula * u_formula
  |URel     of string * u_term list 
  |UForall  of string * u_formula


let rec string_of_Uterm t = 
  match t with
  | UVar s -> s
  | UFunc (f, terms) -> f ^ "(" ^ (String.concat ", " (List.map string_of_Uterm terms)) ^ ")"

let rec string_of_Uformula f = 
  match f with
  | URel (r, terms) -> r ^ "(" ^ (String.concat ", " (List.map string_of_Uterm terms)) ^ ")"
  | UForall (x, f) -> "∀" ^ x ^ ".(" ^ string_of_Uformula f ^ ")"
  | UImp (f1, f2) -> "(" ^ (string_of_Uformula f1) ^ " → " ^ (string_of_Uformula f2) ^ ")"
  | UBot -> "⊥"
  