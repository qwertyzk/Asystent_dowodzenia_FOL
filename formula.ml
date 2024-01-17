type term = 
  | Free of string
  | Bound of int (*indeks de Bruijna*)
  | Func of string * term list

type formula =
  | Rel of string * term list
  | Forall of string * formula
  | Imp of formula * formula
  | Bot