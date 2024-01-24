module type Theory=sig
    open Formula
  
    type axiom
    val axiom : axiom -> formula
  end
  
module Logic(T : Theory): sig
  
  open Formula
  
  type theorem = formula list * formula

  (* Zamienia aksjomat na twierdzenie *)
  val axiom: T.axiom -> theorem
  
  (* Gettery do założeń i formuły z twierdzenia *)
  val assumptions : theorem -> formula list
  val consequence : theorem -> formula
  
  (* Zakłada prawdziwość formuły (wrzuca ją do założeń) *)
  val by_assumption : formula -> theorem

  val pp_print_theorem : Format.formatter -> theorem -> unit
    
end