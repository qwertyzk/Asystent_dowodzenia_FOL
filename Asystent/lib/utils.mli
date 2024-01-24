open Formula
open User_formula


(* Sprawdza, czy zmienna jest wolna w termie/formule *)
val free_in_term : string->term->bool
val free_in_formula : string->formula->bool

(* Sprawdza, czy zmienna w liście nazwanych założeń *)
val is_fresh : string -> ('a * formula) list -> bool

(* Konwersja formuł podanych przez użytkownika do postaci z indeksami de Bruijna i na odwrót *)
val db_convert : u_formula -> formula
val rev_db_convert : formula -> u_formula

(* Konwersja termu podanego przez użytkownika *)
val convert_term : u_term -> term

(* Inkrementuje wszystkie indeksy de Bruijna w termie. Przydaje się, gdy podstawiamy zmienną, która ma być związana. *)
val inc_term : term -> term

(* Podstawianie termu (t_sub) pod zmienną wolną (v) w termie/formule *)
val subst_in_formula : string->term->formula->formula

(* Sprawdzanie równości formuł *)
val eq_form : formula -> formula -> bool

(* Przyjmuje formułę spod kwantyfikatora i podstawia podany term pod kwantyfikowaną zmienną. *)
val remove_bound : term -> formula -> formula

val pp_print_Uformula : Format.formatter -> u_formula -> unit
val pp_print_formula : Format.formatter -> formula -> unit