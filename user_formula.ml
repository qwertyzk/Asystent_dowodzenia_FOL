(*definicja formuł podawanych przez uzytkownika*)
type term_u=
  |Var_u of string
  |Func_u of string * term_u list 

type formula_u=
  |Bot_u
  |Rel_u of string * term_u list 
  |Imp_u of formula_u * formula_u
  |Forall_u of string * formula_u