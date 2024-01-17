module type Theory = sig
  open Formula
  type axiom
  val axiom : axiom -> formula
end
 
module Logic(T : Theory) =
struct

  open Formula
  open Utils
(* w tym pliku jeszcze jest chaos. Operujemy na formula czy user formula?*)
  type theorem = formula list * formula

  let axiom ax = ([], T.axiom ax)

  let assumptions (a , _) = a
  let consequence (_, c) = c

  let rec rem xs f = 
    match xs with
    | [] -> []
    | (x :: xs) when x = f -> xs 
    | (x :: xs) -> x :: rem xs f

  let sum xs ys =
      xs @ List.filter (fun x -> not (List.mem x xs)) ys

  let by_assumption f = 
    [f], f

  let imp_i f thm =
    let a = assumptions thm in
    let c = consequence thm in
    rem a f, Imp (f, c)

  let imp_e th1 th2 =
    let a1 = assumptions th1 in
    let a2 = assumptions th2 in
    let c1 = consequence th1 in
    let c2 = consequence th2 in
    match c1 with
    | Imp (a, b) when a = c2 -> (sum a1 a2, b);
    | _ -> failwith "error in implication elimination"

  let bot_e f thm =
    let a = assumptions thm in
    let c = consequence thm in
    match c with
    | Bot -> (a, f)
    | _ -> failwith "error in bottom elimination"
    
  let forall_i thm x =
    let a = assumptions thm in
    let c = consequence thm in
    if List.exists (free_in_formula x) a then
      failwith "error in forall introduction"
    else
      (a, Forall(x, c))

  let forall_e thm t =
    let a = assumptions thm in
    let c = consequence thm in
    match c with
    | Forall (x, f) -> (a, subst_in_formula x t f)
    | _ -> failwith "error in forall elimination"

end
