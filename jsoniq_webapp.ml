
module Lis = Jsoniq_lis

(* LIS building *)
let make_lis (args : (string * string) list) = new Lis.lis

(* rendering words and inputs into HTML *)
						   
let html_of_word : Jsoniq_syntax.word -> Html.t = function
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> "\"" ^ String.escaped s ^ "\""
  | `Var v -> v
  | `ContextItem -> "_"
  | `ContextEnv -> "*"
  | `Order Jsoniq.ASC -> "ASC"
  | `Order Jsoniq.DESC -> "DESC"
  | `Func name -> name
  | `TheFocus -> Html.span ~classe:"highlighted" "__"
  | `Ellipsis -> "..."
			    
let html_of_input (input : Jsoniq_syntax.input) : Html.t =
  let t, placeholder =
    match input with
    | `Bool -> "checkbox", None
    | `Int -> "number", Some "0"
    | `Float -> "number", Some "0.0e+0"
    | `String -> "text", None
    | `Ident -> "text", Some "x"
  in
  Html.input ~classe:"input" ?placeholder t

	     
let w_focus =
  new Widget_focus.widget
      ~id:"lis-focus"
      ~html_of_word

let w_suggestions : Jsoniq_suggestions.suggestion Widget_suggestions.widget =
  let dico = new Html.dico "lis-results-focus" in
  new Widget_suggestions.widget
      ~id:"lis-suggestions"
      ~html_of_suggestion:(fun sugg ->
			   Html.syntax ~dico ~html_of_word ~html_of_input
				       (Jsoniq_syntax.syn_transf (fst sugg)))
			     
let w_results : (Jsoniq.var, Jsoniq.item) Widget_table.widget =
  let dico = new Html.dico "lis-results-focus" in
  let html_of_input input =
    failwith "unexpected input in extent" in
  new Widget_table.widget
      ~id:"lis-results"
      ~html_of_column:(fun x ->
		       let classe_opt =
			 if x = Jsoniq_semantics.field_focus
			 then Some "highlighted"
			 else None in
		       None, classe_opt, None, x)
      ~html_of_cell:(fun i ->
		     let d = Jsoniq.unpack i in
		     Html.syntax ~dico ~html_of_word ~html_of_input
				 (Jsoniq_syntax.syn_data d))
      
      
let render_place place k =
  let xml = Jsoniq_syntax.syn_focus place#focus in
  w_focus#set_syntax xml;
  w_focus#on_focus_change
    (fun foc ->
     let p = new Jsoniq_lis.place place#lis foc in
     k ~push_in_history:false p);
  w_focus#on_focus_delete
    (fun () ->
     let foc_opt = Jsoniq_focus.delete place#focus in
     match foc_opt with
     | Some foc ->
	let p = new Jsoniq_lis.place place#lis foc in
	k ~push_in_history:true p
     | None -> ());
  place#eval
    (fun ext ->
     w_results#set_contents
       ext.Jsoniq_semantics.vars
       ext.Jsoniq_semantics.bindings)
    (fun suggestions ->
     w_suggestions#set_suggestions suggestions;
     w_suggestions#on_suggestion_selection
       (fun sugg ->
	let transf, new_foc = sugg in
	let p = new Jsoniq_lis.place place#lis new_foc in
	k ~push_in_history:true p))
    
let _ =
  Webapp.start
    ~make_lis
    ~render_place
