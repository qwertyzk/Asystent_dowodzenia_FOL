open Formula
open User_formula

val free_in_term : string->term->bool
val free_in_formula : string->formula->bool

(* val subst_in_term : string->term->term->term *)
(* val subst_in_formula : string->term->formula->formula *)

val db_convert : u_formula -> formula

val print_formula_u : u_formula -> string
val print_formula : formula -> string

