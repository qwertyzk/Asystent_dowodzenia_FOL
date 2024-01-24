(*definicja formuł podawanych przez uzytkownika*)
type u_term =
  |UVar   of string
  |UFunc  of string * u_term list 

type u_formula =
  |UBot
  |UImp     of u_formula * u_formula
  |URel     of string * u_term list 
  |UForall  of string * u_formula

val string_of_Uformula : u_formula -> string
val string_of_Uterm : u_term -> string
