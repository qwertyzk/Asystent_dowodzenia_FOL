type term = 
  | Free  of string
  | Bound of int (*indeks de Bruijna*)
  | Func  of string * term list

type formula = 
  | Bot
  | Imp     of formula * formula
  | Rel     of string * term list
  | Forall  of string * formula

val string_of_formula : formula -> string
val string_of_term : term -> string
