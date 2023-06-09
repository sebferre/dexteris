
open Syntax
open Jsoniq
open Jsoniq_focus

(* syntax definition *)
type word = [`Bool of bool | `Int of int | `Float of float | `String of string | `Filename of string | `Var of var | `Func of string | `ContextItem | `ContextEnv | `Order of order | `TheFocus | `Ellipsis ]
type input = [ `Int of int Focus.input
             | `IntOption of int option Focus.input 
	     | `Float of float Focus.input
	     | `String of string Focus.input
	     | `Ident of string Focus.input
	     | `FuncSig of (string * string list) Focus.input
	     | `Select of string list * string Focus.input
	     | `FileString of string option (* accept *) * (string * string) Focus.input ]

type syn = (word,input,focus) xml

let show_whitechars : string -> string =
  let re_space = Str.regexp_string " " in
  let re_newline = Str.regexp_string "\n" in
  fun s ->
  s
  |> Str.global_replace re_space "␣"
  |> Str.global_replace re_newline "¶"


let rec syn_list ~limit (f : 'a -> syn) (l : 'a list) : syn list =
  match l with
  | [] -> []
  | x::r ->
     if limit=0
     then [ [Kwd "..."] ]
     else f x :: syn_list ~limit:(limit-1) f r
			      
let rec syn_item ?(string_preprocess : string -> string = fun s -> s) : item -> syn = function
  | `Bool b -> [Word (`Bool b)]
  | `Int i -> [Word (`Int i)]
  | `Intlit s -> [Word (`String s)]
  | `Float f -> [Word (`Float f)]
  | `String s ->
     let open Js_of_ocaml in
     let n = (Js.string s)##.length in
     if n > 200
     then
       let s_short = Js.to_string ((Js.string s)##substring 0 200) in
       [Word (`String (string_preprocess s_short)); Kwd ("... (" ^ string_of_int n ^ " chars)")]
     else [Word (`String (string_preprocess s))]
  | `Null -> [Kwd "null"]
  | `Assoc pairs ->
     [Quote ("{",
	     [Enum (", ",
		    syn_list ~limit:20
		      (fun (k,i) -> Kwd k :: Kwd ":" :: syn_item ~string_preprocess i)
		      pairs)],
	     "}")]
  | `List li ->
     [Quote ("[",
	     [Enum (", ",
		    syn_list ~limit:10 (syn_item ~string_preprocess) li)],
	     "]")]
  | `Tuple li ->
     [Quote ("(",
	     [Enum (", ",
		    syn_list ~limit:10 (syn_item ~string_preprocess) li)],
	     ")")]
  | `Variant (c, i_opt) ->
     [Quote ("<",
             Kwd c ::
               (match i_opt with
                | None -> []
                | Some i -> Kwd ":" :: syn_item ~string_preprocess i),
             ">")]

let seq_sep = "; "
       
let syn_data ~(limit : int) (d : data) : syn =
  assert (limit > 1);
  let li, r_opt = Seq.take limit d in
  let lxml_rest =
    match r_opt with
    | None -> []
    | Some _ -> [[Kwd "..."]] in
  match li with
  | [] -> []
  | [i] -> syn_item i
  | (`List _ | `Assoc _)::_ ->
     [Block (List.fold_right
	       (fun i lxml -> syn_item i :: lxml)
	       li lxml_rest)]
  | _ ->
     [Enum (seq_sep,
	    List.fold_right
	      (fun i lxml -> syn_item i :: lxml)
	      li lxml_rest)]

let syn_args lxml =
  [Quote ("(", [Enum (", ", lxml)], ")")]
let syn_func func lxml =
  Word (`Func func) :: syn_args lxml
let syn_pair xml1 xml2 : syn =
  xml1 @ Kwd ":" :: xml2
let syn_order_raw xml1 xml2 : syn =
  xml1 @ xml2
let syn_order xml1 o : syn =
  syn_order_raw xml1 [Word (`Order o)]

let rec label_of_var x =
  if x.[0] = '#'
  then "rank of " ^ label_of_var (String.sub x 1 (String.length x - 1))
  else
    match x with
    | "" -> "?"
    | "$" -> "this"
    | "$key" -> "the key"
    | "$value" -> "the value"
    | _ -> x
  
let syn_Var x : syn = [Word (`Var (label_of_var x))]
let syn_binder : binder -> syn = function
  | Var x -> syn_Var x
  | Fields -> [Kwd "*"]
		
let syn_Call library name lxml : syn =
  try library#syntax name lxml
  with _ -> syn_func name lxml
let syn_Flower xml : syn =
  [Indent xml]
(* [Kwd "collect"; Indent xml] *) (* no needed, focus skipped *)
let syn_Concat lxml : syn =
  [Enum (seq_sep, lxml)]
let syn_Exists xmlx xml1 xml2 : syn =
  Kwd "some" :: xmlx @ Kwd "in" :: xml1 @ [Indent (Kwd "satisfies" :: xml2)]
let syn_ForAll xmlx xml1 xml2 : syn =
  Kwd "every" :: xmlx @ Kwd "in" :: xml1 @ [Indent (Kwd "satisfies" :: xml2)]
let syn_If xml1 xml2 xml3 : syn =
  [Block [Kwd "if" :: xml1;
	  Kwd "then" :: [Indent xml2];
	  Kwd "else" :: [Indent xml3]]]
let syn_Or lxml : syn =
  [Coord ([Kwd "or"], lxml)]
let syn_And lxml : syn =
  [Coord ([Kwd "and"], lxml)]
let syn_Not xml : syn =
  Kwd "not" :: xml
let syn_Map xml1 xml2 : syn =
  xml1 @ Kwd "!" :: xml2
let syn_Pred xml1 xml2 : syn =
  xml1 @ Quote ("[ ", xml2, " ]") :: []
let syn_Dot xml1 xml2 : syn =
  xml1 @ Kwd "." :: xml2
let syn_ArrayLookup xml1 xml2 : syn =
  xml1 @ Quote ("[[", xml2, "]]") :: []
let syn_ArrayUnboxing xml1 : syn =
  xml1 @ Kwd "[]" :: []
let syn_ContextItem : syn =
  [Word `ContextItem]
let syn_ContextEnv : syn =
  [Word `ContextEnv]
let syn_EObject xml_pairs : syn =
  match xml_pairs with
  | [] -> [Kwd "{ }"]
  | [xml_pair] -> [Quote ("{ ", xml_pair, " }")]
  | _ -> [Quote ("{", [Indent [Block xml_pairs]], "}")]
let syn_EnvObject : syn =
  [Kwd "{ * }"]
let syn_Objectify xml1 : syn =
  [Quote ("{| ", xml1, " |}")]
let syn_Arrayify xml1 : syn =
  [Quote ("[", xml1, "]")]
let syn_DefFunc xml_func args xml0 xml1 xml2 : syn =
  let xml_args = List.map syn_Var args in
(*  let xml_inputs = 
    if inputs = []
    then []
    else
      [Block
	 (List.map
	    (fun input ->
	     Kwd "input" :: syn_args (List.map (fun d -> syn_data ~limit:10 d) input))
	    inputs)] in *)
  [Block [Kwd "def" :: xml_func @ syn_args xml_args @
	    Kwd "=" :: Indent (Kwd "input" :: Indent xml0 :: xml1) :: [];
	  xml2]]

let syn_Return xml1 : syn =
  Kwd "return" :: xml1
let syn_For xmlx xml1 opt xml2 : syn =
  Kwd "for" :: (if opt then [Kwd "optional"] else []) @ xmlx @ Kwd "in" :: xml1 @
    [Indent xml2]
let syn_Let xmlx xml1 xml2 : syn =
  [Block [Kwd "let" :: xmlx @ Kwd ":=" :: xml1;
	  xml2]]
let syn_Count xmlx xml1 : syn =
  [Block [Kwd "count" :: xmlx;
	  xml1]]
let syn_Where xml1 xml2 : syn =
  [Block [Kwd "where" :: xml1;
	  xml2]]
let syn_GroupBy_raw xml1 xml2 : syn =
  [Block [Kwd "group" :: Kwd "by" :: xml1;
	  xml2]] 
let syn_GroupBy lx xml2 : syn =
  syn_GroupBy_raw
    [Enum (", ", List.map syn_Var lx)]
    xml2
let syn_Hide_raw xml1 xml2 : syn =
  [Block [Kwd "hide" :: xml1;
	  xml2]]
let syn_Hide lx xml2 : syn =
  syn_Hide_raw
    [Enum (", ", List.map syn_Var lx)]
    xml2
let syn_Slice xml_offset xml_limit xml1 : syn =
  [Block [Kwd "offset" :: xml_offset @ Kwd "limit" :: xml_limit;
	  xml1]]
let syn_OrderBy xml_orders xml2 : syn =
  [Block [Kwd "order" :: Kwd "by" :: Enum (", ", xml_orders) :: [];
	  xml2]]
let syn_FConcat lxml : syn = syn_Concat lxml
let syn_FIf xml1 xml2 xml3 : syn = syn_If xml1 xml2 xml3

let syn_susp xml : syn = [Suspended xml]

let syn_Focus (foc : focus) xml : syn =
  match foc with
  | AtExpr (Flower _, _) -> xml (* no focus on Flower, invalid focus *)
  | AtFlower (Return _, _) -> xml (* no focus on Return, invalid focus *)
  | _ -> [Focus (foc, xml)]
                       
class type library =
  object
    method syntax : string -> syn list -> syn
  end
			   
(* DERIVED *)
let rec syn_focus (library : #library) (foc : focus) : syn =
  match foc with
  | AtExpr (e,ctx) -> syn_expr_ctx library e ctx [Highlight (syn_expr library e ctx); ControlCurrentFocus]
  | AtFlower (f,ctx) -> syn_flower_ctx library f ctx [Highlight (syn_flower library f ctx); ControlCurrentFocus]
and syn_expr library e ctx : syn =
  let xml =
    match e with
    | S s -> [Word (`String (show_whitechars s))]
    | Item i -> syn_item ~string_preprocess:show_whitechars i
    | FileString (fname,contents) ->
       [Kwd "file"; Word (`Filename fname)]
    | Empty -> [Kwd "()"]
    | Concat le ->
       syn_Concat
	 (List.map
	    (fun (e,ll_rr) -> syn_expr library e (ConcatX (ll_rr,ctx)))
	    (Focus.focus_list_of_list le))
    | Flower f ->
       syn_Flower (syn_flower library f (Flower1 ctx))
    | Exists (x,e1,e2) ->
       syn_Exists (syn_Var x)
		  (syn_expr library e1 (Exists1 (x,ctx,e2)))
		  (syn_expr library e2 (Exists2 (x,e1,ctx)))
    | ForAll (x,e1,e2) ->
       syn_ForAll (syn_Var x)
		  (syn_expr library e1 (ForAll1 (x,ctx,e2)))
		  (syn_expr library e2 (ForAll2 (x,e1,ctx)))
    | If (e1,e2,e3) ->
       syn_If (syn_expr library e1 (If1 (ctx,e2,e3)))
	      (syn_expr library e2 (If2 (e1,ctx,e3)))
	      (syn_expr library e3 (If3 (e1,e2,ctx)))
    | Or le ->
       syn_Or
	 (List.map
	    (fun (e,ll_rr) -> syn_expr library e (OrX (ll_rr,ctx)))
	    (Focus.focus_list_of_list le))
    | And le ->
       syn_And
	 (List.map
	    (fun (e,ll_rr) -> syn_expr library e (AndX (ll_rr,ctx)))
	    (Focus.focus_list_of_list le))
    | Not e ->
       syn_Not (syn_expr library e (Not1 ctx))
    | Call (func,le) ->
       syn_Call library
	 func
	 (List.map
	    (fun (e,ll_rr) -> syn_expr library e (CallX (func,ll_rr,ctx)))
	    (Focus.focus_list_of_list le))
    | Map (e1,e2) ->
       syn_Map (syn_expr library e1 (Map1 (ctx,e2)))
	       (syn_expr library e2 (Map2 (e1,ctx)))
    | Pred (e1,e2) ->
       syn_Pred (syn_expr library e1 (Pred1 (ctx,e2)))
		(syn_expr library e2 (Pred2 (e1,ctx)))
    | Dot (e1,e2) ->
       syn_Dot (syn_expr library e1 (Dot1 (ctx,e2)))
	       (syn_expr library e2 (Dot2 (e1,ctx)))
    | ArrayLookup (e1,e2) ->
       syn_ArrayLookup (syn_expr library e1 (ArrayLookup1 (ctx,e2)))
		       (syn_expr library e2 (ArrayLookup2 (e1,ctx)))
    | ArrayUnboxing e1 ->
       syn_ArrayUnboxing (syn_expr library e1 (ArrayUnboxing1 ctx))
    | Var x -> syn_Var x
    | ContextItem -> syn_ContextItem
    | ContextEnv -> syn_ContextEnv
    | EObject pairs ->
       syn_EObject
	 (List.map
	    (fun ((e1,e2),ll_rr) ->
	     syn_pair
	       (syn_expr library e1 (EObjectX1 (ll_rr,ctx,e2)))
	       (syn_expr library e2 (EObjectX2 (ll_rr,e1,ctx))))
	    (Focus.focus_list_of_list pairs))
    | EnvObject -> syn_EnvObject
    | Objectify e1 ->
       syn_Objectify (syn_expr library e1 (Objectify1 ctx))
    | Arrayify e1 ->
       syn_Arrayify (syn_expr library e1 (Arrayify1 ctx))
    | Let (br,e1,e2) ->
       syn_Let (syn_binder br)
	       (syn_expr library e1 (Let1 (br,ctx,e2)))
	       (syn_expr library e2 (Let2 (br,e1,ctx)))
    | DefFunc (name,args,e0,e1,e2) ->
       syn_DefFunc [Word (`Func name)] args
		   (syn_expr library e0 (DefFunc0 (name,args,ctx,e1,e2)))
		   (syn_expr library e1 (DefFunc1 (name,args,e0,ctx,e2)))
		   (syn_expr library e2 (DefFunc2 (name,args,e0,e1,ctx)))
  in
  syn_Focus (AtExpr (e,ctx)) xml
and syn_flower library f ctx : syn =
  let xml =
    match f with
    | Return e1 ->
       syn_Return (syn_expr library e1 (Return1 ctx))
    | For (br,e1,opt,f1) ->
       syn_For (syn_binder br)
	       (syn_expr library e1 (For1 (br,ctx,opt,f1)))
	       opt
	       (syn_flower library f1 (For2 (br,e1,opt,ctx)))
    | FLet (br,e1,f1) ->
       syn_Let (syn_binder br)
	       (syn_expr library e1 (FLet1 (br,ctx,f1)))
	       (syn_flower library f1 (FLet2 (br,e1,ctx)))
    | Count (x,f1) ->
       syn_Count (syn_Var x)
		 (syn_flower library f1 (Count1 (x,ctx)))
    | Where (e1,f1) ->
       syn_Where (syn_expr library e1 (Where1 (ctx,f1)))
		 (syn_flower library f1 (Where2 (e1,ctx)))
    | GroupBy (lx,f1) ->
       syn_GroupBy lx
		   (syn_flower library f1 (GroupBy1 (lx,ctx)))
    | Hide (lx,f1) ->
       syn_Hide lx
		   (syn_flower library f1 (Hide1 (lx,ctx)))
    | Slice (o,l,f1) ->
       syn_Slice [Word (`Int o)] [match l with None -> Kwd "none" | Some l -> Word (`Int l)]
		 (syn_flower library f1 (Slice1 (o,l,ctx)))
    | OrderBy (leo,f1) ->
       syn_OrderBy (List.map
		      (fun ((e,o),ll_rr) ->
		       syn_order
			 (syn_expr library e (OrderBy1X (ll_rr,ctx,o,f1)))
			 o)
		      (Focus.focus_list_of_list leo))
		   (syn_flower library f1 (OrderBy2 (leo,ctx)))
    | FConcat lf ->
       syn_FConcat (List.map
		      (fun (f,ll_rr) -> syn_flower library f (FConcatX (ll_rr,ctx)))
		      (Focus.focus_list_of_list lf))
    | FIf (f1,f2,f3) ->
       syn_FIf (syn_flower library f1 (FIf1 (ctx,f2,f3)))
	       (syn_flower library f2 (FIf2 (f1,ctx,f3)))
	       (syn_flower library f3 (FIf3 (f1,f2,ctx)))
  in
  syn_Focus (AtFlower (f,ctx)) xml
and syn_expr_ctx library e ctx (xml_e : syn) : syn =
  let xml_e = syn_Focus (AtExpr (e,ctx)) xml_e in
  match ctx with
  | Root -> xml_e
  | ConcatX (ll_rr,ctx) ->
     syn_expr_ctx library
       (Concat (Focus.list_of_ctx e ll_rr)) ctx
       (syn_Concat
	  (Syntax.xml_list_focus
	     (fun (e1,ll_rr1) -> syn_susp (syn_expr library e1 (ConcatX (ll_rr1,ctx))))
	     (e,ll_rr) xml_e))
  | Exists1 (x,ctx,e2) ->
     syn_expr_ctx library
       (Exists (x,e,e2)) ctx
       (syn_Exists (syn_Var x) xml_e
		   (syn_susp (syn_expr library e2 (Exists2 (x,e,ctx)))))
  | Exists2 (x,e1,ctx) ->
     syn_expr_ctx library
       (Exists (x,e1,e)) ctx
       (syn_Exists (syn_Var x)
		   (syn_expr library e1 (Exists1 (x,ctx,e)))
		   xml_e)
  | ForAll1 (x,ctx,e2) ->
     syn_expr_ctx library
       (ForAll (x,e,e2)) ctx
       (syn_ForAll (syn_Var x) xml_e
		   (syn_susp (syn_expr library e2 (ForAll2 (x,e,ctx)))))
  | ForAll2 (x,e1,ctx) ->
     syn_expr_ctx library
       (ForAll (x,e1,e)) ctx
       (syn_ForAll (syn_Var x)
		   (syn_expr library e1 (ForAll1 (x,ctx,e)))
		   xml_e)
  | If1 (ctx,e2,e3) ->
     syn_expr_ctx library
       (If (e,e2,e3)) ctx
       (syn_If xml_e
	       (syn_susp (syn_expr library e2 (If2 (e,ctx,e3))))
	       (syn_susp (syn_expr library e3 (If3 (e,e2,ctx)))))
  | If2 (e1,ctx,e3) ->
     syn_expr_ctx library
       (If (e1,e,e3)) ctx
       (syn_If (syn_expr library e1 (If1 (ctx,e,e3)))
	       xml_e
	       (syn_susp (syn_expr library e3 (If3 (e1,e,ctx)))))
  | If3 (e1,e2,ctx) ->
     syn_expr_ctx library
       (If (e1,e2,e)) ctx
       (syn_If (syn_expr library e1 (If1 (ctx,e2,e)))
	       (syn_susp (syn_expr library e2 (If2 (e1,ctx,e))))
	       xml_e)
  | OrX (ll_rr,ctx) ->
     syn_expr_ctx library
       (Or (Focus.list_of_ctx e ll_rr)) ctx
       (syn_Or
	  (Syntax.xml_list_focus
	     (fun (e1,ll_rr1) -> syn_susp (syn_expr library e1 (OrX (ll_rr1,ctx))))
	     (e,ll_rr) xml_e))
  | AndX (ll_rr,ctx) ->
     syn_expr_ctx library
       (And (Focus.list_of_ctx e ll_rr)) ctx
       (syn_And
	  (Syntax.xml_list_focus
	     (fun (e1,ll_rr1) -> syn_susp (syn_expr library e1 (AndX (ll_rr1,ctx))))
	     (e,ll_rr) xml_e))
  | Not1 ctx ->
     syn_expr_ctx library
       (Not e) ctx
       (syn_Not xml_e) (* suspend Not *)
  | CallX (func,ll_rr,ctx) ->
     syn_expr_ctx library
       (Call (func, Focus.list_of_ctx e ll_rr)) ctx
       (syn_Call library
	  func
	  (Syntax.xml_list_focus
	     (fun (e1,ll_rr1) -> syn_susp (syn_expr library e1 (CallX (func,ll_rr1,ctx))))
	     (e,ll_rr) xml_e))
  | Map1 (ctx,e2) ->
     syn_expr_ctx library
       (Map (e,e2)) ctx
       (syn_Map xml_e
		(syn_susp (syn_expr library e2 (Map2 (e,ctx)))))
  | Map2 (e1,ctx) ->
     syn_expr_ctx library
       (Map (e1,e)) ctx
       (syn_Map (syn_expr library e1 (Map1 (ctx,e)))
		xml_e)
  | Pred1 (ctx,e2) ->
     syn_expr_ctx library
       (Pred (e,e2)) ctx
       (syn_Pred xml_e
		 (syn_susp (syn_expr library e2 (Pred2 (e,ctx)))))
  | Pred2 (e1,ctx) ->
     syn_expr_ctx library
       (Pred (e1,e)) ctx
       (syn_Pred (syn_expr library e1 (Pred1 (ctx,e)))
		 xml_e)
  | Dot1 (ctx,e2) ->
     syn_expr_ctx library
       (Dot (e,e2)) ctx
       (syn_Dot xml_e
		(syn_susp (syn_expr library e2 (Dot2 (e,ctx)))))
  | Dot2 (e1,ctx) ->
     syn_expr_ctx library
       (Dot (e1,e)) ctx
       (syn_Dot (syn_susp (syn_expr library e1 (Dot1 (ctx,e))))
		xml_e)
  | ArrayLookup1 (ctx,e2) ->
     syn_expr_ctx library
       (ArrayLookup (e,e2)) ctx
       (syn_ArrayLookup xml_e
			(syn_susp (syn_expr library e2 (ArrayLookup2 (e,ctx)))))
  | ArrayLookup2 (e1,ctx) ->
     syn_expr_ctx library
       (ArrayLookup (e1,e)) ctx
       (syn_ArrayLookup (syn_susp (syn_expr library e1 (ArrayLookup1 (ctx,e))))
			xml_e)
  | ArrayUnboxing1 ctx ->
     syn_expr_ctx library
       (ArrayUnboxing e) ctx
       (syn_ArrayUnboxing xml_e)
  | EObjectX1 (ll_rr,ctx,e2) ->
     syn_expr_ctx library
       (EObject (Focus.list_of_ctx (e,e2) ll_rr)) ctx
       (syn_EObject
	  (Syntax.xml_list_focus
	     (fun ((e1,e2), ll_rr) ->
	      syn_susp (syn_pair
			  (syn_expr library e1 (EObjectX1 (ll_rr,ctx,e2)))
			  (syn_expr library e2 (EObjectX2 (ll_rr,e1,ctx)))))
	     ((e,e2), ll_rr)
	     (syn_pair xml_e
		       (syn_expr library e2 (EObjectX2 (ll_rr,e,ctx))))))
  | EObjectX2 (ll_rr,e1,ctx) ->
     syn_expr_ctx library
       (EObject (Focus.list_of_ctx (e1,e) ll_rr)) ctx
       (syn_EObject
	  (Syntax.xml_list_focus
	     (fun ((e1,e2), ll_rr) ->
	      syn_susp (syn_pair
			  (syn_expr library e1 (EObjectX1 (ll_rr,ctx,e2)))
			  (syn_expr library e2 (EObjectX2 (ll_rr,e1,ctx)))))
	     ((e1,e), ll_rr)
	     (syn_pair (syn_expr library e1 (EObjectX1 (ll_rr,ctx,e)))
		       xml_e)))
  | Objectify1 ctx ->
     syn_expr_ctx library
       (Objectify e) ctx
       (syn_Objectify xml_e)
  | Arrayify1 ctx ->
     syn_expr_ctx library
       (Arrayify e) ctx
       (syn_Arrayify xml_e)
  | Let1 (br,ctx,e2) ->
     syn_expr_ctx library
       (Let (br,e,e2)) ctx
       (syn_Let (syn_binder br)
		xml_e
		(syn_susp (syn_expr library e2 (Let2 (br,e,ctx)))))
  | Let2 (br,e1,ctx) ->
     syn_expr_ctx library
       (Let (br,e1,e)) ctx
       (syn_Let (syn_binder br)
		(syn_expr library e1 (Let1 (br,ctx,e)))
		xml_e)
  | DefFunc0 (name,args,ctx,e1,e2) ->
     syn_expr_ctx library
       (DefFunc (name,args,e,e1,e2)) ctx
       (syn_DefFunc [Word (`Func name)] args
		    xml_e
		    (syn_susp (syn_expr library e1 (DefFunc1 (name,args,e,ctx,e2))))
		    (syn_susp (syn_expr library e2 (DefFunc2 (name,args,e,e1,ctx)))))
  | DefFunc1 (name,args,e0,ctx,e2) ->
     syn_expr_ctx library
       (DefFunc (name,args,e0,e,e2)) ctx
       (syn_DefFunc [Word (`Func name)] args
		    (syn_expr library e0 (DefFunc0 (name,args,ctx,e,e2)))
		    xml_e
		    (syn_susp (syn_expr library e2 (DefFunc2 (name,args,e0,e,ctx)))))
  | DefFunc2 (name,args,e0,e1,ctx) ->
     syn_expr_ctx library
       (DefFunc (name,args,e0,e1,e)) ctx
       (syn_DefFunc [Word (`Func name)] args
		    (syn_expr library e0 (DefFunc0 (name,args,ctx,e1,e)))
		    (syn_expr library e1 (DefFunc1 (name,args,e0,ctx,e)))
		    xml_e)
  | Return1 ctx ->
     syn_flower_ctx library
       (Return e) ctx
       (syn_Return xml_e)
  | For1 (br,ctx,opt,f) ->
     syn_flower_ctx library
       (For (br,e,opt,f)) ctx
       (syn_For (syn_binder br)
		xml_e
		opt
		(syn_susp (syn_flower library f (For2 (br,e,opt,ctx)))))
  | FLet1 (br,ctx,f) ->
     syn_flower_ctx library
       (FLet (br,e,f)) ctx
       (syn_Let (syn_binder br)
		xml_e
		(syn_susp (syn_flower library f (FLet2 (br,e,ctx)))))
  | Where1 (ctx,f) ->
     syn_flower_ctx library
       (Where (e,f)) ctx
       (syn_Where xml_e
		  (syn_susp (syn_flower library f (Where2 (e,ctx)))))
  | OrderBy1X (ll_rr,ctx,o,f) ->
     syn_flower_ctx library
       (OrderBy (Focus.list_of_ctx (e,o) ll_rr, f)) ctx
       (syn_OrderBy (Syntax.xml_list_focus
		       (fun ((e,o), ll_rr) ->
			syn_susp
			  (syn_order
			     (syn_expr library e (OrderBy1X (ll_rr,ctx,o,f)))
			     o))
		       ((e,o), ll_rr)
		       (syn_order xml_e o))
		    (syn_susp (syn_flower library f (OrderBy2 (Focus.list_of_ctx (e,o) ll_rr, ctx)))))
and syn_flower_ctx library f ctx (xml_f : syn) : syn =
  let xml_f = syn_Focus (AtFlower (f,ctx)) xml_f in
  match ctx with
  | Flower1 Root -> xml_f (* special case for root flower *)
  | Flower1 ctx ->
     syn_expr_ctx library
       (Flower f) ctx
       (syn_Flower xml_f)
  | For2 (br,e1,opt,ctx) ->
     syn_flower_ctx library
       (For (br,e1,opt,f)) ctx
       (syn_For (syn_binder br)
		(syn_expr library e1 (For1 (br,ctx,opt,f)))
		opt
		xml_f)
  | FLet2 (br,e1,ctx) ->
     syn_flower_ctx library
       (FLet (br,e1,f)) ctx
       (syn_Let (syn_binder br)
		(syn_expr library e1 (FLet1 (br,ctx,f)))
		xml_f)
  | Count1 (x,ctx) ->
     syn_flower_ctx library
       (Count (x,f)) ctx
       (syn_Count (syn_Var x) xml_f)
  | Where2 (e1,ctx) ->
     syn_flower_ctx library
       (Where (e1,f)) ctx
       (syn_Where (syn_expr library e1 (Where1 (ctx,f)))
		  xml_f)
  | GroupBy1 (lx,ctx) ->
     syn_flower_ctx library
       (GroupBy (lx,f)) ctx
       (syn_GroupBy lx xml_f)
  | Hide1 (lx,ctx) ->
     syn_flower_ctx library
       (Hide (lx,f)) ctx
       (syn_Hide lx xml_f)
  | Slice1 (o,l,ctx) ->
     syn_flower_ctx library
       (Slice (o,l,f)) ctx
       (syn_Slice [Word (`Int o)] [match l with None -> Kwd "none" | Some l -> Word (`Int l)] xml_f)
  | OrderBy2 (leo,ctx) ->
     syn_flower_ctx library
       (OrderBy (leo,f)) ctx
       (syn_OrderBy (List.map
		       (fun ((e,o),ll_rr) ->
			syn_order (syn_expr library e (OrderBy1X (ll_rr,ctx,o,f)))
				  o)
		       (Focus.focus_list_of_list leo))
		    xml_f)
  | FConcatX (ll_rr,ctx) ->
     syn_flower_ctx library
       (FConcat (Focus.list_of_ctx f ll_rr)) ctx
       (syn_FConcat
	  (Syntax.xml_list_focus
	     (fun (f1,ll_rr1) -> syn_susp (syn_flower library f1 (FConcatX (ll_rr1,ctx))))
	     (f,ll_rr) xml_f))
  | FIf1 (ctx,f2,f3) ->
     syn_flower_ctx library
       (FIf (f,f2,f3)) ctx
       (syn_FIf xml_f
	       (syn_susp (syn_flower library f2 (FIf2 (f,ctx,f3))))
	       (syn_susp (syn_flower library f3 (FIf3 (f,f2,ctx)))))
  | FIf2 (f1,ctx,f3) ->
     syn_flower_ctx library
       (FIf (f1,f,f3)) ctx
       (syn_FIf (syn_susp (syn_flower library f1 (FIf1 (ctx,f,f3))))
	       xml_f
	       (syn_susp (syn_flower library f3 (FIf3 (f1,f,ctx)))))
  | FIf3 (f1,f2,ctx) ->
     syn_flower_ctx library
       (FIf (f1,f2,f)) ctx
       (syn_FIf (syn_susp (syn_flower library f1 (FIf1 (ctx,f2,f))))
		(syn_susp (syn_flower library f2 (FIf2 (f1,ctx,f))))
		xml_f)

let the_focus = Word `TheFocus
let ellipsis = Word `Ellipsis
let syn_this = syn_Var var_context
       
let syn_transf (library : #library) : transf -> syn = function
  | FocusUp -> [Kwd "(focus up)"]
  | FocusRight -> [Kwd "(focus right)"]
  | Delete -> [Kwd "(delete focus)"]
  | InsertBool false -> [Word (`Bool false)]
  | InsertBool true -> [Word (`Bool true)]
  | InputInt i -> [Kwd "an"; Kwd "integer"; Input (`Int i)]
  | InputRange (i1,i2) -> Kwd "a" :: Kwd "range" :: Kwd "from " :: library#syntax "range" [[Input (`Int i1)]; [Input (`Int i2)]]
  | InputFloat i -> [Kwd "a"; Kwd "float"; Input (`Float i)]
  | InputString i -> [Kwd "a"; Kwd "string"; Input (`String i)]
  | InputFileString i -> [Kwd "a"; Kwd "file"; Kwd "contents"; Input (`FileString (None, i))]
  | InputFileTable i -> [Kwd "tabular data (csv)"; Input (`FileString (Some ".csv", i))]
  | InsertNull -> [Kwd "null"]
  | InsertConcat1 -> syn_Concat [[the_focus]; [ellipsis]]
  | InsertConcat2 -> syn_Concat [[ellipsis]; [the_focus]]
  | InsertExists1 in_x -> syn_Exists [Input (`Ident in_x)] [the_focus] [ellipsis]
  | InsertForAll1 in_x -> syn_ForAll [Input (`Ident in_x)] [the_focus] [ellipsis]
  | InsertExists2 in_x -> syn_Exists [Input (`Ident in_x)] [ellipsis] [the_focus]
  | InsertForAll2 in_x -> syn_ForAll [Input (`Ident in_x)] [ellipsis] [the_focus]
  | InsertIf1 -> syn_If [the_focus] [ellipsis] [ellipsis]
  | InsertIf2 -> syn_If [ellipsis] [the_focus] [ellipsis]
  | InsertIf3 -> syn_If [ellipsis] [ellipsis] [the_focus]
  | InsertOr -> syn_Or [[the_focus]; [ellipsis]]
  | InsertAnd -> syn_And [[the_focus]; [ellipsis]]
  | InsertNot -> syn_Not [the_focus]
  | InsertFunc (func,arity,pos) ->
     syn_Call library func
       (if arity = 0
        then []
	else Focus.make_list (pos-1) [ellipsis] @ [the_focus] :: Focus.make_list (arity-pos) [ellipsis])
  | InsertMap -> syn_Map [the_focus] [ellipsis]
  | InsertPred -> syn_Pred [the_focus] [ellipsis]
  | InsertDot -> syn_Dot [the_focus] [ellipsis]
  | InsertField k -> syn_Dot [the_focus] [Word (`String (show_whitechars k))]
  | InsertArrayLookup in_i_opt -> syn_ArrayLookup [the_focus] [Input (`IntOption in_i_opt)] (* [ellipsis] *)
  | InsertArrayUnboxing -> syn_ArrayUnboxing [the_focus]
  | InsertVar x -> syn_Var x
  | InsertContextItem -> syn_ContextItem
  | InsertContextEnv -> syn_ContextEnv
  | InsertObject i -> syn_EObject [syn_pair [Input (`String i)] [the_focus]]
  | InsertObjectField i -> syn_pair [Input (`String i)] [ellipsis]
  | InsertEnvObject -> syn_EnvObject
  | InsertArray -> syn_Arrayify [ellipsis]
  | InsertObjectify -> syn_Objectify [the_focus]
  | InsertArrayify -> syn_Arrayify [the_focus]
  | InsertDefFunc1 in_sig ->
     [Block [[Kwd "def"; Input (`FuncSig in_sig); Kwd "="; the_focus];
	     [ellipsis]]]
  (*syn_DefFunc [Input (`Ident in_name)] [] [ellipsis] [the_focus] [ellipsis]*)
  | InsertDefFunc2 in_sig ->
     [Block [[Kwd "def"; Input (`FuncSig in_sig); Kwd "="; ellipsis];
	     [the_focus]]]
  (*syn_DefFunc [Input (`Ident in_sig)] [] [ellipsis] [ellipsis] [the_focus]*)
  | InsertArg in_x -> [Kwd "add"; Kwd "argument"; Input (`Ident in_x)]
  | InsertForVar1 (in_x,in_opt) -> syn_For [Input (`Ident in_x)] [the_focus] false [ellipsis] (* TODO: optional *)
  | InsertForVar2 (in_x,in_opt) -> syn_For [Input (`Ident in_x)] [ellipsis] false [the_focus] (* TODO: optional *)
  | InsertForFields1 (in_opt) -> syn_For [Kwd "*"] [the_focus] false [ellipsis] (* TODO: optional *)
  | InsertLetVar1 in_x -> syn_Let [Input (`Ident in_x)] [the_focus] [ellipsis]
  | InsertLetVar2 in_x -> syn_Let [Input (`Ident in_x)] [ellipsis] [the_focus]
  | InsertLetFields1 -> syn_Let [Kwd "*"] [the_focus] [ellipsis]
  | InsertCount1 in_x -> syn_Count [Input (`Ident in_x)] [the_focus]
  | InsertWhere1 -> syn_Where [the_focus] [ellipsis]
  | InsertWhere2 -> syn_Where [ellipsis] [the_focus]
  | InsertGroupBy (lx, in_x) -> syn_GroupBy_raw [Input (`Select (lx, in_x))] [the_focus]
  | InsertHide (lx, in_x) -> syn_Hide_raw [Input (`Select (lx, in_x))] [the_focus]
  | InsertSlice (in_offset,in_limit) -> syn_Slice [Input (`Int in_offset)] [Input (`Int in_limit)] [the_focus]
  | InsertOrderBy1 in_o -> syn_OrderBy [syn_order_raw [the_focus] [Input (`Select (order_strings, in_o))]] [ellipsis]
  | InsertOrderBy2 in_o -> syn_OrderBy [syn_order_raw [ellipsis] [Input (`Select (order_strings, in_o))]] [the_focus]
				    
	   
