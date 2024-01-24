open Format
module type Theory = sig
  open Formula
  type axiom
  val axiom : axiom -> formula
end
 
module Logic(T : Theory) =
struct

  open Formula
  open Utils
  
  type theorem = formula list * formula

  let axiom ax = ([], T.axiom ax)

  let assumptions (a , _) = a
  let consequence (_, c) = c
    
  let by_assumption f = 
    [f], f

  let pp_print_theorem fmtr thm =
    pp_open_hvbox fmtr 2;
    pp_print_string fmtr "[";
    begin match assumptions thm with
    | [] -> ()
    | f :: fs ->
      pp_print_formula fmtr f;
      fs |> List.iter (fun f ->
        pp_print_string fmtr ",";
        pp_print_space fmtr ();
        pp_print_formula fmtr f);
      pp_print_space fmtr ()
    end;
    pp_print_string fmtr "]";
    pp_print_space fmtr ();
    pp_open_hbox fmtr ();
    pp_print_string fmtr "⊢";
    pp_print_space fmtr ();
    pp_print_formula fmtr (consequence thm);
    pp_close_box fmtr ();
    pp_close_box fmtr ()


end