
open Jsoniq
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
    | `Int input ->
       "number", "0",
       new Html.input_update
	   (fun input_elt ->
	    Jsutils.integer_of_input input_elt
	    |> Option.iter input#set)
    | `Float input ->
       "number", "0.0e+0",
       new Html.input_update
	   (fun input_elt ->
	    Jsutils.float_of_input input_elt
	    |> Option.iter input#set)
    | `String input ->
       "text", "",
       new Html.input_update
	   (fun input_elt ->
	    Jsutils.string_of_input input_elt
	    |> input#set)
    | `Ident input ->
       "text", "x",
       new Html.input_update
	   (fun input_elt ->
	    Jsutils.string_of_input input_elt
	    |> input#set)
    | `FileData input ->
       "file", "",
       new Html.input_update
	   (fun input_elt ->
	    Jsutils.file_string_of_input
	      input_elt
	      (fun (filename,contents) ->
	       let str = Yojson.Basic.stream_from_string ~fname:filename contents in
	       let data = Seq.from_stream str in
	       input#set (filename, data)))
  in
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
      

let suggestions_cols = ["col-md-3 col-xs-1";
			"col-md-4 col-xs-1";
			"col-md-5 col-xs-1"]
      
let render_place place k =
  let xml = Jsoniq_syntax.syn_focus place#focus in
  w_focus#set_syntax xml;
  w_focus#on_focus_change
    (fun foc ->
     let p = new Jsoniq_lis.place place#lis foc in
     k ~push_in_history:false p);
  w_focus#on_focus_up
    (fun () ->
     match Jsoniq_focus.focus_up place#focus with
     | Some foc ->
	let p = new Jsoniq_lis.place place#lis foc in
	k ~push_in_history:false p
     | None -> ());
  w_focus#on_focus_delete
    (fun () ->
     match Jsoniq_focus.delete place#focus with
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
     w_suggestions#set_suggestions suggestions_cols suggestions;
     w_suggestions#on_suggestion_selection
       (fun sugg ->
	match place#activate sugg with
	| Some p -> k ~push_in_history:true p
	| None -> assert false))

let error_message : exn -> string = function
  | Failure msg -> msg
  | Jsoniq.TODO -> "some feature is not yet implemented"
  | Jsoniq.TypeError msg -> "Type error: " ^ msg
  | Jsoniq.Unbound x -> "Unbound variable: " ^ x
  | Jsoniq.Undefined msg -> "Undefined " ^ msg
  | Yojson.Json_error msg -> "Syntax error in JSON file: " ^ msg
  | exn -> Printexc.to_string exn

let _ =
  Webapp.start
    ~make_lis
    ~render_place
    ~error_message
