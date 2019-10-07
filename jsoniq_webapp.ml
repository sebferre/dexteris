
module Lis = Jsoniq_lis

(* LIS building *)
let make_lis (args : (string * string) list) = new Lis.lis

(* rendering words and inputs into HTML *)
						   
let html_of_word : Jsoniq_syntax.word -> Html.t = function
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> "\"" ^ Jsutils.escapeHTML s ^ "\""
  | `Var v -> v
  | `ContextItem -> "_"
  | `ContextEnv -> "*"
  | `Order Jsoniq.ASC -> "ASC"
  | `Order Jsoniq.DESC -> "DESC"
  | `Func name -> name
  | `TheFocus -> Html.span ~classe:"highlighted" "__"
  | `Ellipsis -> "..."
			    
let html_info_of_input (input : Jsoniq_syntax.input) : Html.input_info =
  (* exceptions are captured by caller of updates *)
  let input_type, placeholder, input_update =
    match input with
    | `Int input -> "number", "0", (fun value -> input#set (int_of_string value))
    | `Float input -> "number", "0.0e+0", (fun value -> input#set (float_of_string value))
    | `String input -> "text", "", (fun value -> input#set value)
    | `Ident input -> "text", "x", (fun value -> input#set value) in
  Html.({ input_type; placeholder; input_update })

      
(* UI widgets *)
			      
let w_focus =
  new Widget_focus.widget
      ~id:"lis-focus"
      ~html_of_word

let w_suggestions : Jsoniq_suggestions.suggestion Widget_suggestions.widget =
  new Widget_suggestions.widget
      ~id:"lis-suggestions"
      ~html_of_suggestion:(fun ~input_dico sugg ->
			   Html.syntax ~input_dico
				       ~html_of_word ~html_info_of_input
				       (Jsoniq_syntax.syn_transf sugg))
			     
let w_results : (Jsoniq.var, Jsoniq.item) Widget_table.widget =
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
		     Html.syntax ~html_of_word
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
	match place#activate sugg with
	| Some p -> k ~push_in_history:true p
	| None -> assert false))
    
let _ =
  Webapp.start
    ~make_lis
    ~render_place