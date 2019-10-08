
module Focus = Jsoniq_focus
module Semantics = Jsoniq_semantics
module Suggestions = Jsoniq_suggestions
	       
class place (lis : lis) (focus : Focus.focus) =
object
  inherit [lis,Focus.focus,Semantics.extent,Suggestions.suggestion] Lis.place lis focus

  method eval k_extent k_suggestions =
    let sem = Semantics.sem_focus focus in
    let ext = Semantics.extent sem in
    k_extent ext;
    let lsugg = Suggestions.suggestions focus sem ext in
    k_suggestions lsugg

  method activate sugg =
    let transf = sugg in
    match Focus.apply_transf transf focus with
    | Some new_focus ->
       let new_focus =
	 match Focus.focus_next_Empty new_focus with
	 | Some new_foc -> new_foc
	 | None -> new_focus in
       Some (new place lis new_focus)
    | None -> None

  method abort = ()
end

and lis =
object (self)
  inherit [place] Lis.lis

  method initial_place = new place (self :> lis) Focus.initial_focus
end
