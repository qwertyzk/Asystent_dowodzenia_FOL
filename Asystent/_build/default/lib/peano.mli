open Formula
open User_formula

module Peano : sig
    (* Nazwy są pobierane od użytkownika, a nie generowane. (Nie muszą być świeże, bo program operuje na indeksach. Nazwy są tylko dla użytkownika, dlatego może je sam wybrać.)*)
    type axiom =
    | EqRefl of string (* ∀x.x = x *)
    | EqElim of string * string * string * u_formula (* ∀y.∀z.y = z ⇒φ{x→y}⇒φ{x→z} *)
    | PlusZ of string (* ∀n.0 + n = n *)
    | PlusS of string * string (* ∀n.∀m.S(n) + m = S(n + m) *)
    | Induction of string * string * u_formula

    (* zamienia aksjomat na formułę *)
    val axiom : axiom -> formula
end