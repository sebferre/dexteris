
module Lis = Jsoniq_lis

(* LIS building *)
let make_lis (args : (string * string) list) = new Lis.lis

(* rendering words and inputs into HTML *)
						   
let html_of_func (func : Jsoniq.func) : Html.t =
  let open Jsoniq in
  match func with
  | EQ -> "="
  | NE -> "!="
  | LE -> "<="
  | LT -> "<"
  | GE -> ">="
  | GT -> ">"
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | IDiv -> "div"
  | Mod -> "mod"
  | Neg -> "-"
  | StringConcat -> "concat"
  | Substring -> "substr"
  | Range -> "range"
  | Sum -> "sum"
  | Avg -> "avg"
  | Defined (name,arity) -> name
						   
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
  | `Func func -> html_of_func func
			    
						   
    
let w_focus =
  new Widget_focus.widget
      ~id:"lis-focus"
      ~html_of_word
      
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
     | None -> ())
    
let _ =
  Webapp.start
    ~make_lis
    ~render_place
