
open Jsoniq
module Lis = Jsoniq_lis

(* LIS building *)
let make_lis (args : (string * string) list) = new Lis.lis

(* rendering words and inputs into HTML *)

let html_of_var v =
  if v = "" then "?"
  else if v = "$" then "this"
  else if v = "#$" then "rank of this"
  else if v.[0] = '#' then "rank of " ^ String.sub v 1 (String.length v - 1)
  else if v = "$key" then "the key"
  else if v = "$value" then "the value"
  else v
						   
let html_of_word : Jsoniq_syntax.word -> Html.t = function
  | `Bool b -> Html.span ~classe:(if b then "word-true" else "word-false") (string_of_bool b)
  | `Int i -> Html.span ~classe:"word-number" (string_of_int i)
  | `Float f -> Html.span ~classe:"word-number" (string_of_float f)
  | `String s -> Html.span ~classe:"word-string" (Jsutils.escapeHTML s)
  | `Filename fname -> Html.span ~classe:"word-filename" (Jsutils.escapeHTML ("<" ^ fname ^ ">"))
  | `Var v -> Html.span ~classe:"word-var" (html_of_var v)
  | `ContextItem -> "_"
  | `ContextEnv -> "*"
  | `Order o -> string_of_order o
  | `Func name -> Html.span ~classe:"word-function" name
  | `TheFocus -> Html.span ~classe:"highlighted" "__"
  | `Ellipsis -> "..."
			    
let html_info_of_input (input : Jsoniq_syntax.input) : Html.input_info =
  (* exceptions are captured by caller of updates *)
  match input with
  | `Int input ->
     Html.int_info
       (fun i k -> input#set i; k ())
  | `Float input ->
     Html.float_info
       (fun f k -> input#set f; k ())
  | `String input ->
     Html.string_info
       (fun s k ->
	let open Js_of_ocaml in
	let s = Regexp.global_replace (Regexp.regexp_string "\\n") s "\n" in
	let s = Regexp.global_replace (Regexp.regexp_string "\\r") s "\r" in
	let s = Regexp.global_replace (Regexp.regexp_string "\\t") s "\t" in
	let s = Regexp.global_replace (Regexp.regexp_string "\\\\") s "\\" in
	input#set s; k ())
  | `Ident input ->
     Html.string_info
       (fun id k -> input#set id; k ())
  | `FuncSig input ->
     Html.string_info
       (fun s k ->
	let open Js_of_ocaml in
	let ls = Regexp.split (Regexp.regexp "[(), ]+") s in
	let ls = List.filter ((<>) "") ls in
	match ls with
	| name::args -> input#set (name, args); k ()
	| [] -> failwith "format expected: func(arg1,arg2,...)")
  | `Select (values, input) ->
     Html.selectElt_info
       values
       (fun x k -> input#set x; k ())
  | `FileString input ->
     Html.fileElt_info
       (fun fname_contents k -> input#set fname_contents; k ())

let html_of_suggestion ~input_dico sugg =
  Html.syntax ~input_dico
    ~html_of_word ~html_info_of_input
    (Jsoniq_syntax.syn_transf Lis.library sugg)


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
				       (Jsoniq_syntax.syn_transf Lis.library sugg))

let w_commandline : Jsoniq_suggestions.suggestion Widget_commandline.widget =
  new Widget_commandline.widget
    ~id:"lis-commandline"
    ~html_of_suggestion
    ~score_of_suggestion:(Jsoniq_command.score_of_suggestion Lis.library)
    ~command_of_suggestion:(Jsoniq_command.command_of_suggestion Lis.library)
			     
let w_results : (Jsoniq.var, Jsoniq.data) Widget_table.widget =
  new Widget_table.widget
      ~id:"lis-results"
      ~html_of_column:(fun x ->
		       let classe_opt =
			 if x = Jsoniq_semantics.field_focus
			 then Some "highlighted"
			 else None in
		       None, classe_opt, None, html_of_var x)
      ~html_of_cell:(fun d ->
		     match Jsoniq_files.get_data_file_opt d with
		     | Some (mime,contents) ->
			let n = String.length contents in
			let contents_excerpt =
			  if n > 100
			  then String.sub contents 0 100 ^ "..."
			  else contents in
			Html.a
			  (Jsutils.make_data_url mime contents)
			  mime
			^ ("&nbsp;(" ^ string_of_int n ^ " chars)")
			^ Html.pre contents_excerpt
		     | None ->
			let xml = Jsoniq_syntax.syn_data ~limit:20 d in
			let html = Html.syntax ~html_of_word xml in
			html)
      

let suggestions_cols = ["col-md-4 col-xs-12";
			"col-md-4 col-xs-12";
			"col-md-4 col-xs-12"]
      
let render_place place k =
  Jsutils.firebug "XML of place";
  let xml = Jsoniq_syntax.syn_focus Lis.library place#focus in
  Jsutils.firebug "focus#set_syntax";
  w_focus#set_syntax xml;
  w_focus#on_focus_change
    (fun foc ->
     let p = new Jsoniq_lis.place place#lis foc in
     k ~push_in_history:false p);
  w_focus#on_focus_up
    (fun () ->
     match Jsoniq_focus.focus_up place#focus with
     | Some (foc,_) ->
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
  Jsutils.firebug "place#eval";
  place#eval
    (fun ext ->
     Jsutils.firebug "ext computed";
     let limit = 20 in (* TODO: add widget for that *)
     let visible_vars =
       List.filter (* filtering out position vars for legibility *)
	 (fun x -> not (Jsoniq.is_var_position x))
	 ext.Jsoniq_semantics.vars in
     let l_bindings, _ = Seq.take limit ext.Jsoniq_semantics.bindings in
     Jsutils.firebug "results#set_contents";
     w_results#set_contents visible_vars l_bindings)
    (fun suggestions ->
     Jsutils.firebug "suggestions#set_suggestions";
     w_suggestions#set_suggestions suggestions_cols suggestions;
     w_commandline#set_suggestions suggestions;
     let suggestion_handler =
       (fun sugg ->
         w_commandline#selected_suggestion sugg;
	 match place#activate sugg with
	 | Some p -> k ~push_in_history:true p
	 | None -> assert false) in
     w_suggestions#on_suggestion_selection suggestion_handler;
     w_commandline#on_suggestion_selection suggestion_handler)

let error_message : exn -> string = function
  | Failure msg -> msg
  | Jsoniq.TODO -> "some feature is not yet implemented"
  | Jsoniq.TypeError msg -> "Type error: " ^ msg
  | Jsoniq.Unbound x -> "Unbound variable: " ^ x
  | Jsoniq.Undefined msg -> "Undefined " ^ msg
  | Jsoniq_functions.Unknown_function name -> "Unknown function: " ^ name
  | Yojson.Json_error msg -> "Syntax error in JSON file: " ^ msg
  | exn -> "Unexpected error: " ^ Printexc.to_string exn

let _ =
  Webapp.start
    ~make_lis
    ~render_place
    ~error_message
