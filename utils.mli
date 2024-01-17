open Formula
open User_formula

val free_in_term : string->term->bool
val free_in_formula : string->formula->bool

val subst_in_term : string->term->term->term
val subst_in_formula : string->term->formula->formula

val db_convert : formula_u -> formula

val print_formula_u : formula_u -> string
val print_formula : formula -> string

