
module Focus = Jsoniq_focus
module Semantics = Jsoniq_semantics
module Suggestions = Jsoniq_suggestions
	       
class place (lis : lis) (focus : Focus.focus) =
object
  inherit [lis,Focus.focus,Semantics.extent,Suggestions.suggestion] Lis.place lis focus

  val mutable extent : Semantics.extent option = None
								    
  method eval k_extent k_suggestions =
    let sem = Semantics.sem_focus focus in
    let ext = Semantics.extent sem in
    extent <- Some ext;
    k_extent ext;
    let llsugg = Suggestions.suggestions focus sem ext in
    k_suggestions llsugg

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

  method json = Focus.focus_to_yojson focus

  method results =
    match extent with
    | None -> failwith "Results are not yet available"
    | Some ext -> Jsoniq_files.mime_contents_of_extent ext
end

and lis =
object (self)
  inherit [place] Lis.lis

  method initial_place =
    new place (self :> lis) Focus.initial_focus

  method place_of_json json =
    match Focus.focus_of_yojson json with
    | Result.Ok foc -> new place (self :> lis) foc
    | Result.Error msg -> invalid_arg msg
end
