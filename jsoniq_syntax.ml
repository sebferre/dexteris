
open Syntax
open Jsoniq
open Jsoniq_focus

type word = [`Bool of bool | `Int of int | `Float of float | `String of string | `Var of var | `Func of string | `ContextItem | `ContextEnv | `Order of order | `TheFocus | `Ellipsis ]
type input = [ `Int of int Focus.input
	     | `Float of float Focus.input
	     | `String of string Focus.input
	     | `Ident of string Focus.input
	     | `FileData of (string * data) Focus.input ]
type syn = (word,input,focus) xml

let rec syn_list ~limit (f : 'a -> syn) (l : 'a list) : syn list =
  match l with
  | [] -> []
  | x::r ->
     if limit=0
     then [ [Kwd "..."] ]
     else f x :: syn_list ~limit:(limit-1) f r
			      
let rec syn_item : item -> syn = function
  | `Bool b -> [Word (`Bool b)]
  | `Int i -> [Word (`Int i)]
  | `Float f -> [Word (`Float f)]
  | `String s -> [Word (`String s)]
  | `Null -> [Kwd "null"]
  | `Assoc pairs ->
     [Quote ("{",
	     [Enum (", ",
		    syn_list ~limit:20
		      (fun (k,i) -> Word (`String k) :: Kwd ":" :: syn_item i)
		      pairs)],
	     "}")]
  | `List li ->
     [Quote ("[",
	     [Enum (", ",
		    syn_list ~limit:10 syn_item li)],
	     "]")]

let seq_sep = "; "
       
let syn_data ~(limit : int) (d : data) : syn =
  assert (limit > 1);
  let li, r_opt = Seq.take limit d in
  let xml_rest =
    match r_opt with
    | None -> []
    | Some _ -> [Kwd "..."] in
  match li with
  | [] -> [Kwd "()"]
  | [i] -> syn_item i
  | (`List _ | `Assoc _)::_ ->
     [Block (List.fold_right
	       (fun i lxml -> syn_item i :: lxml)
	       li [xml_rest])]
  | _ ->
     [Enum (seq_sep,
	    List.fold_right
	      (fun i lxml -> syn_item i :: lxml)
	      li [xml_rest])]
			      
let syn_args lxml =
  [Quote ("(", [Enum (", ", lxml)], ")")]
let syn_pair xml1 xml2 : syn =
  xml1 @ Kwd ":" :: xml2
let syn_order xml1 o : syn =
  xml1 @ [Word (`Order o)]

let syn_Call func lxml =
  let classic name lxml = Word (`Func name) :: syn_args lxml in
  match func, lxml with
  | EQ, [xml1; xml2] -> xml1 @ Kwd "==" :: xml2
  | NE, [xml1; xml2] -> xml1 @ Kwd "!=" :: xml2
  | LE, [xml1; xml2] -> xml1 @ Kwd "<=" :: xml2
  | LT, [xml1; xml2] -> xml1 @ Kwd "<" :: xml2
  | GE, [xml1; xml2] -> xml1 @ Kwd ">=" :: xml2
  | GT, [xml1; xml2] -> xml1 @ Kwd ">" :: xml2
  | Plus, [xml1; xml2] -> xml1 @ Kwd "+" :: xml2
  | Minus, [xml1; xml2] -> xml1 @ Kwd "-" :: xml2
  | Times, [xml1; xml2] -> xml1 @ Kwd "*" :: xml2
  | Div, [xml1; xml2] -> xml1 @ Kwd "/" :: xml2
  | IDiv, [xml1; xml2] -> xml1 @ Kwd "div" :: xml2
  | Mod, [xml1; xml2] -> xml1 @ Kwd "mod" :: xml2
  | Neg, [xml1] -> Kwd "-" :: xml1
  | StringConcat, [_] -> classic "concat" lxml
  | Substring, [_;_;_] -> classic "substr" lxml
  | ArrayLength, [_] -> classic "length" lxml
  | ObjectKeys, [_] -> classic "keys" lxml
  | Range, [xml1; xml2] -> xml1 @ Kwd "to" :: xml2
  | Count, [_] -> classic "count" lxml
  | Sum, [_] -> classic "sum" lxml
  | Avg, [_] -> classic "avg" lxml
  | Defined (name,arity), lxml -> classic name lxml
  | _ -> failwith "syn_Call: invalid number of arguments or unexpected function"
			       
let syn_Flower xml : syn =
  [Kwd "collect"; Indent xml]
let syn_Concat lxml : syn =
  [Enum (seq_sep, lxml)]
let syn_Exists xmlx xml1 xml2 : syn =
  Kwd "some" :: xmlx @ Kwd "in" :: xml1 @ [Indent (Kwd "satisfies" :: xml2)]
let syn_ForAll xmlx xml1 xml2 : syn =
  Kwd "every" :: xmlx @ Kwd "in" :: xml1 @ [Indent (Kwd "satisfies" :: xml2)]
let syn_If xml1 xml2 xml3 : syn =
  [Block [Kwd "if" :: xml1;
	  Kwd "then" :: xml2;
	  Kwd "else" :: xml3]]
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
let syn_Var x : syn =
  [Word (`Var x)]
let syn_ContextItem : syn =
  [Word `ContextItem]
let syn_ContextEnv : syn =
  [Word `ContextEnv]
let syn_EObject xml_pairs : syn =
  match xml_pairs with
  | [] -> [Kwd "{ }"]
  | [xml_pair] -> [Quote ("{ ", xml_pair, " }")]
  | _ -> [Quote ("{", [Indent [Block xml_pairs]], "}")]
let syn_Objectify xml1 : syn =
  [Quote ("{| ", xml1, " |}")]
let syn_Arrayify xml1 : syn =
  [Quote ("[", xml1, "]")]
let syn_DefFunc xml_func args xml1 xml2 : syn =
  [Block [Kwd "def" :: xml_func @
	    syn_args (List.map (fun x -> [Word (`Var x)]) args) @ Kwd "=" :: Indent xml1 :: [];
	  xml2]]

let syn_Return xml1 : syn =
  Kwd "return" :: xml1
let syn_For xmlx xml1 opt xml2 : syn =
  Kwd "for" :: (if opt then [Kwd "optional"] else []) @ xmlx @ Kwd "in" :: xml1 @
    [Indent xml2]
let syn_Let xmlx xml1 xml2 : syn =
  [Block [Kwd "let" :: xmlx @ Kwd "=" :: xml1;
	  xml2]]
let syn_Where xml1 xml2 : syn =
  [Block [Kwd "where" :: xml1;
	  xml2]]
let syn_GroupBy lx xml1 : syn =
  [Block [Kwd "group" :: Kwd "by" :: Enum (", ", List.map (fun x -> [Word (`Var x)]) lx) :: [];
	  xml1]]
let syn_Project lx xml1 : syn =
  [Block [Kwd "project" :: Kwd "on" :: Enum (", ", List.map (fun x -> [Word (`Var x)]) lx) :: [];
	  xml1]]
let syn_Slice xml_offset xml_limit xml1 : syn =
  [Block [Kwd "offset" :: xml_offset @ Kwd "limit" :: xml_limit;
	  xml1]]
let syn_OrderBy xml_orders xml2 : syn =
  [Block [Kwd "order" :: Kwd "by" :: Enum (", ", xml_orders) :: [];
	  xml2]]
let syn_FConcat lxml : syn = syn_Concat lxml
let syn_FIf xml1 xml2 xml3 : syn = syn_If xml1 xml2 xml3

let syn_susp xml : syn = [Suspended xml]
					  
(* DERIVED *)			      
let rec syn_focus (foc : focus) : syn =
  match foc with
  | AtExpr (e,ctx) -> syn_expr_ctx e ctx [Highlight (syn_expr e ctx); ControlCurrentFocus]
  | AtFlower (f,ctx) -> syn_flower_ctx f ctx [Highlight (syn_flower f ctx); ControlCurrentFocus]
and syn_expr e ctx : syn =
  let xml =
    match e with
    | S s -> [Word (`String s)]
    | Item i -> syn_item i
    | Empty -> [Kwd "()"]
    | Concat le ->
       syn_Concat
	 (List.map
	    (fun (e,ll_rr) -> syn_expr e (ConcatX (ll_rr,ctx)))
	    (Focus.ctx_of_list le))
    | Flower f ->
       syn_Flower (syn_flower f (Flower1 ctx))
    | Exists (x,e1,e2) ->
       syn_Exists [Word (`Var x)]
		  (syn_expr e1 (Exists1 (x,ctx,e2)))
		  (syn_expr e2 (Exists2 (x,e1,ctx)))
    | ForAll (x,e1,e2) ->
       syn_ForAll [Word (`Var x)]
		  (syn_expr e1 (ForAll1 (x,ctx,e2)))
		  (syn_expr e2 (ForAll2 (x,e1,ctx)))
    | If (e1,e2,e3) ->
       syn_If (syn_expr e1 (If1 (ctx,e2,e3)))
	      (syn_expr e2 (If2 (e1,ctx,e3)))
	      (syn_expr e3 (If3 (e1,e2,ctx)))
    | Or le ->
       syn_Or
	 (List.map
	    (fun (e,ll_rr) -> syn_expr e (OrX (ll_rr,ctx)))
	    (Focus.ctx_of_list le))
    | And le ->
       syn_And
	 (List.map
	    (fun (e,ll_rr) -> syn_expr e (AndX (ll_rr,ctx)))
	    (Focus.ctx_of_list le))
    | Not e ->
       syn_Not (syn_expr e (Not1 ctx))
    | Call (func,le) ->
       syn_Call func
		(List.map
		   (fun (e,ll_rr) -> syn_expr e (CallX (func,ll_rr,ctx)))
		   (Focus.ctx_of_list le))
    | Map (e1,e2) ->
       syn_Map (syn_expr e1 (Map1 (ctx,e2)))
	       (syn_expr e2 (Map2 (e1,ctx)))
    | Pred (e1,e2) ->
       syn_Pred (syn_expr e1 (Pred1 (ctx,e2)))
		(syn_expr e2 (Pred2 (e1,ctx)))
    | Dot (e1,e2) ->
       syn_Dot (syn_expr e1 (Dot1 (ctx,e2)))
	       (syn_expr e2 (Dot2 (e1,ctx)))
    | ArrayLookup (e1,e2) ->
       syn_ArrayLookup (syn_expr e1 (ArrayLookup1 (ctx,e2)))
		       (syn_expr e2 (ArrayLookup2 (e1,ctx)))
    | ArrayUnboxing e1 ->
       syn_ArrayUnboxing (syn_expr e1 (ArrayUnboxing1 ctx))
    | Var x -> syn_Var x
    | ContextItem -> syn_ContextItem
    | ContextEnv -> syn_ContextEnv
    | EObject pairs ->
       syn_EObject
	 (List.map
	    (fun ((e1,e2),ll_rr) ->
	     syn_pair
	       (syn_expr e1 (EObjectX1 (ll_rr,ctx,e2)))
	       (syn_expr e2 (EObjectX2 (ll_rr,e1,ctx))))
	    (Focus.ctx_of_list pairs))
    | Objectify e1 ->
       syn_Objectify (syn_expr e1 (Objectify1 ctx))
    | Arrayify e1 ->
       syn_Arrayify (syn_expr e1 (Arrayify1 ctx))
    | Let (x,e1,e2) ->
       syn_Let [Word (`Var x)]
		  (syn_expr e1 (Let1 (x,ctx,e2)))
		  (syn_expr e2 (Let2 (x,e1,ctx)))
    | DefFunc (name,args,e1,e2) ->
       syn_DefFunc [Word (`Func name)] args
		   (syn_expr e1 (DefFunc1 (name,args,ctx,e2)))
		   (syn_expr e2 (DefFunc2 (name,args,e1,ctx)))
  in
  [Focus (AtExpr (e,ctx), xml)]
and syn_flower f ctx : syn =
  let xml =
    match f with
    | Return e1 ->
       syn_Return (syn_expr e1 (Return1 ctx))
    | FileData (filename,d,f1) ->
       syn_For [Kwd "fields"]
	       [Kwd "file"; Word (`String filename)]
	       false
	       (syn_flower f1 (FileData2 (filename,d,ctx)))
    | For (x,e1,opt,f1) ->
       syn_For [Word (`Var x)]
	       (syn_expr e1 (For1 (x,ctx,opt,f1)))
	       opt
	       (syn_flower f1 (For2 (x,e1,opt,ctx)))
    | FLet (x,e1,f1) ->
       syn_Let [Word (`Var x)]
	       (syn_expr e1 (FLet1 (x,ctx,f1)))
	       (syn_flower f1 (FLet2 (x,e1,ctx)))
    | Where (e1,f1) ->
       syn_Where (syn_expr e1 (Where1 (ctx,f1)))
		 (syn_flower f1 (Where2 (e1,ctx)))
    | GroupBy (lx,f1) ->
       syn_GroupBy lx
		   (syn_flower f1 (GroupBy1 (lx,ctx)))
    | Project (lx,f1) ->
       syn_Project lx
		   (syn_flower f1 (Project1 (lx,ctx)))
    | Slice (o,l,f1) ->
       syn_Slice [Word (`Int o)] [match l with None -> Kwd "none" | Some l -> Word (`Int l)]
		 (syn_flower f1 (Slice1 (o,l,ctx)))
    | OrderBy (leo,f1) ->
       syn_OrderBy (List.map
		      (fun ((e,o),ll_rr) ->
		       syn_order
			 (syn_expr e (OrderBy1X (ll_rr,ctx,o,f1)))
			 o)
		      (Focus.ctx_of_list leo))
		   (syn_flower f1 (OrderBy2 (leo,ctx)))
    | FConcat lf ->
       syn_FConcat (List.map
		      (fun (f,ll_rr) -> syn_flower f (FConcatX (ll_rr,ctx)))
		      (Focus.ctx_of_list lf))
    | FIf (f1,f2,f3) ->
       syn_FIf (syn_flower f1 (FIf1 (ctx,f2,f3)))
	       (syn_flower f2 (FIf2 (f1,ctx,f3)))
	       (syn_flower f3 (FIf3 (f1,f2,ctx)))
  in
  [Focus (AtFlower (f,ctx), xml)]
and syn_expr_ctx e ctx (xml_e : syn) : syn =
  let xml_e = [Focus (AtExpr (e,ctx), xml_e)] in
  match ctx with
  | Root -> xml_e
  | ConcatX (ll_rr,ctx) ->
     syn_expr_ctx
       (Concat (Focus.list_of_ctx e ll_rr)) ctx
       (syn_Concat
	  (Syntax.xml_list_ctx
	     (fun e1 ll_rr1 -> syn_susp (syn_expr e1 (ConcatX (ll_rr1,ctx))))
	     e ll_rr xml_e))
  | Exists1 (x,ctx,e2) ->
     syn_expr_ctx
       (Exists (x,e,e2)) ctx
       (syn_Exists [Word (`Var x)] xml_e
		   (syn_susp (syn_expr e2 (Exists2 (x,e,ctx)))))
  | Exists2 (x,e1,ctx) ->
     syn_expr_ctx
       (Exists (x,e1,e)) ctx
       (syn_Exists [Word (`Var x)]
		   (syn_expr e1 (Exists1 (x,ctx,e)))
		   xml_e)
  | ForAll1 (x,ctx,e2) ->
     syn_expr_ctx
       (ForAll (x,e,e2)) ctx
       (syn_ForAll [Word (`Var x)] xml_e
		   (syn_susp (syn_expr e2 (ForAll2 (x,e,ctx)))))
  | ForAll2 (x,e1,ctx) ->
     syn_expr_ctx
       (ForAll (x,e1,e)) ctx
       (syn_ForAll [Word (`Var x)]
		   (syn_expr e1 (ForAll1 (x,ctx,e)))
		   xml_e)
  | If1 (ctx,e2,e3) ->
     syn_expr_ctx
       (If (e,e2,e3)) ctx
       (syn_If xml_e
	       (syn_susp (syn_expr e2 (If2 (e,ctx,e3))))
	       (syn_susp (syn_expr e3 (If3 (e,e2,ctx)))))
  | If2 (e1,ctx,e3) ->
     syn_expr_ctx
       (If (e1,e,e3)) ctx
       (syn_If (syn_susp (syn_expr e1 (If1 (ctx,e,e3))))
	       xml_e
	       (syn_susp (syn_expr e3 (If3 (e1,e,ctx)))))
  | If3 (e1,e2,ctx) ->
     syn_expr_ctx
       (If (e1,e2,e)) ctx
       (syn_If (syn_susp (syn_expr e1 (If1 (ctx,e2,e))))
	       (syn_susp (syn_expr e2 (If2 (e1,ctx,e))))
	       xml_e)
  | OrX (ll_rr,ctx) ->
     syn_expr_ctx
       (Or (Focus.list_of_ctx e ll_rr)) ctx
       (syn_Or
	  (Syntax.xml_list_ctx
	     (fun e1 ll_rr1 -> syn_susp (syn_expr e1 (OrX (ll_rr1,ctx))))
	     e ll_rr xml_e))
  | AndX (ll_rr,ctx) ->
     syn_expr_ctx
       (And (Focus.list_of_ctx e ll_rr)) ctx
       (syn_And
	  (Syntax.xml_list_ctx
	     (fun e1 ll_rr1 -> syn_susp (syn_expr e1 (AndX (ll_rr1,ctx))))
	     e ll_rr xml_e))
  | Not1 ctx ->
     syn_expr_ctx
       (Not e) ctx
       (syn_Not xml_e) (* suspend Not *)
  | CallX (func,ll_rr,ctx) ->
     syn_expr_ctx
       (Call (func, Focus.list_of_ctx e ll_rr)) ctx
       (syn_Call func
		 (Syntax.xml_list_ctx
		    (fun e1 ll_rr1 -> syn_susp (syn_expr e1 (CallX (func,ll_rr1,ctx))))
		    e ll_rr xml_e))
  | Map1 (ctx,e2) ->
     syn_expr_ctx
       (Map (e,e2)) ctx
       (syn_Map xml_e
		(syn_susp (syn_expr e2 (Map2 (e,ctx)))))
  | Map2 (e1,ctx) ->
     syn_expr_ctx
       (Map (e1,e)) ctx
       (syn_Map (syn_expr e1 (Map1 (ctx,e)))
		xml_e)
  | Pred1 (ctx,e2) ->
     syn_expr_ctx
       (Pred (e,e2)) ctx
       (syn_Pred xml_e
		 (syn_susp (syn_expr e2 (Pred2 (e,ctx)))))
  | Pred2 (e1,ctx) ->
     syn_expr_ctx
       (Pred (e1,e)) ctx
       (syn_Pred (syn_expr e1 (Pred1 (ctx,e)))
		 xml_e)
  | Dot1 (ctx,e2) ->
     syn_expr_ctx
       (Dot (e,e2)) ctx
       (syn_Dot xml_e
		(syn_susp (syn_expr e2 (Dot2 (e,ctx)))))
  | Dot2 (e1,ctx) ->
     syn_expr_ctx
       (Dot (e1,e)) ctx
       (syn_Dot (syn_susp (syn_expr e1 (Dot1 (ctx,e))))
		xml_e)
  | ArrayLookup1 (ctx,e2) ->
     syn_expr_ctx
       (ArrayLookup (e,e2)) ctx
       (syn_ArrayLookup xml_e
			(syn_susp (syn_expr e2 (ArrayLookup2 (e,ctx)))))
  | ArrayLookup2 (e1,ctx) ->
     syn_expr_ctx
       (ArrayLookup (e1,e)) ctx
       (syn_ArrayLookup (syn_susp (syn_expr e1 (ArrayLookup1 (ctx,e))))
			xml_e)
  | ArrayUnboxing1 ctx ->
     syn_expr_ctx
       (ArrayUnboxing e) ctx
       (syn_ArrayUnboxing xml_e)
  | EObjectX1 (ll_rr,ctx,e2) ->
     syn_expr_ctx
       (EObject (Focus.list_of_ctx (e,e2) ll_rr)) ctx
       (syn_EObject
	  (Syntax.xml_list_ctx
	     (fun (e1,e2) ll_rr ->
	      syn_susp (syn_pair
			  (syn_expr e1 (EObjectX1 (ll_rr,ctx,e2)))
			  (syn_expr e2 (EObjectX2 (ll_rr,e1,ctx)))))
	     (e,e2) ll_rr
	     (syn_pair xml_e
		       (syn_expr e2 (EObjectX2 (ll_rr,e,ctx))))))
  | EObjectX2 (ll_rr,e1,ctx) ->
     syn_expr_ctx
       (EObject (Focus.list_of_ctx (e1,e) ll_rr)) ctx
       (syn_EObject
	  (Syntax.xml_list_ctx
	     (fun (e1,e2) ll_rr ->
	      syn_susp (syn_pair
			  (syn_expr e1 (EObjectX1 (ll_rr,ctx,e2)))
			  (syn_expr e2 (EObjectX2 (ll_rr,e1,ctx)))))
	     (e1,e) ll_rr
	     (syn_pair (syn_expr e1 (EObjectX1 (ll_rr,ctx,e)))
		       xml_e)))
  | Objectify1 ctx ->
     syn_expr_ctx
       (Objectify e) ctx
       (syn_Objectify xml_e)
  | Arrayify1 ctx ->
     syn_expr_ctx
       (Arrayify e) ctx
       (syn_Arrayify xml_e)
  | Let1 (x,ctx,e2) ->
     syn_expr_ctx
       (Let (x,e,e2)) ctx
       (syn_Let [Word (`Var x)]
		   xml_e
		   (syn_susp (syn_expr e2 (Let2 (x,e,ctx)))))
  | Let2 (x,e1,ctx) ->
     syn_expr_ctx
       (Let (x,e1,e)) ctx
       (syn_Let [Word (`Var x)]
		   (syn_expr e1 (Let1 (x,ctx,e)))
		   xml_e)
  | DefFunc1 (name,args,ctx,e2) ->
     syn_expr_ctx
       (DefFunc (name,args,e,e2)) ctx
       (syn_DefFunc [Word (`Func name)] args
		    xml_e
		    (syn_susp (syn_expr e2 (DefFunc2 (name,args,e,ctx)))))
  | DefFunc2 (name,args,e1,ctx) ->
     syn_expr_ctx
       (DefFunc (name,args,e1,e)) ctx
       (syn_DefFunc [Word (`Func name)] args
		    (syn_expr e1 (DefFunc1 (name,args,ctx,e)))
		    xml_e)
  | Return1 ctx ->
     syn_flower_ctx
       (Return e) ctx
       (syn_Return xml_e)
  | For1 (x,ctx,opt,f) ->
     syn_flower_ctx
       (For (x,e,opt,f)) ctx
       (syn_For [Word (`Var x)]
		xml_e
		opt
		(syn_susp (syn_flower f (For2 (x,e,opt,ctx)))))
  | FLet1 (x,ctx,f) ->
     syn_flower_ctx
       (FLet (x,e,f)) ctx
       (syn_Let [Word (`Var x)]
		xml_e
		(syn_susp (syn_flower f (FLet2 (x,e,ctx)))))
  | Where1 (ctx,f) ->
     syn_flower_ctx
       (Where (e,f)) ctx
       (syn_Where xml_e
		  (syn_susp (syn_flower f (Where2 (e,ctx)))))
  | OrderBy1X (ll_rr,ctx,o,f) ->
     syn_flower_ctx
       (OrderBy (Focus.list_of_ctx (e,o) ll_rr, f)) ctx
       (syn_OrderBy (Syntax.xml_list_ctx
		       (fun (e,o) ll_rr ->
			syn_susp
			  (syn_order
			     (syn_expr e (OrderBy1X (ll_rr,ctx,o,f)))
			     o))
		       (e,o) ll_rr
		       (syn_order xml_e o))
		    (syn_susp (syn_flower f (OrderBy2 (Focus.list_of_ctx (e,o) ll_rr, ctx)))))
and syn_flower_ctx f ctx (xml_f : syn) : syn =
  let xml_f = [Focus (AtFlower (f,ctx), xml_f)] in
  match ctx with
  | Flower1 ctx ->
     syn_expr_ctx
       (Flower f) ctx
       (syn_Flower xml_f)
  | FileData2 (filename,d,ctx) ->
     syn_flower_ctx
       (FileData (filename,d,f)) ctx
       (syn_For [Kwd "fields"]
		[Kwd "file"; Word (`String filename)]
		false
		xml_f)
  | For2 (x,e1,opt,ctx) ->
     syn_flower_ctx
       (For (x,e1,opt,f)) ctx
       (syn_For [Word (`Var x)]
		(syn_expr e1 (For1 (x,ctx,opt,f)))
		opt
		xml_f)
  | FLet2 (x,e1,ctx) ->
     syn_flower_ctx
       (FLet (x,e1,f)) ctx
       (syn_Let [Word (`Var x)]
		(syn_expr e1 (FLet1 (x,ctx,f)))
		xml_f)
  | Where2 (e1,ctx) ->
     syn_flower_ctx
       (Where (e1,f)) ctx
       (syn_Where (syn_expr e1 (Where1 (ctx,f)))
		  xml_f)
  | GroupBy1 (lx,ctx) ->
     syn_flower_ctx
       (GroupBy (lx,f)) ctx
       (syn_GroupBy lx xml_f)
  | Project1 (lx,ctx) ->
     syn_flower_ctx
       (Project (lx,f)) ctx
       (syn_Project lx xml_f)
  | Slice1 (o,l,ctx) ->
     syn_flower_ctx
       (Slice (o,l,f)) ctx
       (syn_Slice [Word (`Int o)] [match l with None -> Kwd "none" | Some l -> Word (`Int l)] xml_f)
  | OrderBy2 (leo,ctx) ->
     syn_flower_ctx
       (OrderBy (leo,f)) ctx
       (syn_OrderBy (List.map
		       (fun ((e,o),ll_rr) ->
			syn_order (syn_expr e (OrderBy1X (ll_rr,ctx,o,f)))
				  o)
		       (Focus.ctx_of_list leo))
		    xml_f)
  | FConcatX (ll_rr,ctx) ->
     syn_flower_ctx
       (FConcat (Focus.list_of_ctx f ll_rr)) ctx
       (syn_FConcat
	  (Syntax.xml_list_ctx
	     (fun f1 ll_rr1 -> syn_susp (syn_flower f1 (FConcatX (ll_rr1,ctx))))
	     f ll_rr xml_f))
  | FIf1 (ctx,f2,f3) ->
     syn_flower_ctx
       (FIf (f,f2,f3)) ctx
       (syn_FIf xml_f
	       (syn_susp (syn_flower f2 (FIf2 (f,ctx,f3))))
	       (syn_susp (syn_flower f3 (FIf3 (f,f2,ctx)))))
  | FIf2 (f1,ctx,f3) ->
     syn_flower_ctx
       (FIf (f1,f,f3)) ctx
       (syn_FIf (syn_susp (syn_flower f1 (FIf1 (ctx,f,f3))))
	       xml_f
	       (syn_susp (syn_flower f3 (FIf3 (f1,f,ctx)))))
  | FIf3 (f1,f2,ctx) ->
     syn_flower_ctx
       (FIf (f1,f2,f)) ctx
       (syn_FIf (syn_susp (syn_flower f1 (FIf1 (ctx,f2,f))))
		(syn_susp (syn_flower f2 (FIf2 (f1,ctx,f))))
	       xml_f)

let the_focus = Word `TheFocus
let ellipsis = Word `Ellipsis
       
let syn_transf : transf -> syn = function
  | FocusUp -> [Kwd "(focus up)"]
  | FocusRight -> [Kwd "(focus right)"]
  | Delete -> [Kwd "(delete focus)"]
  | InsertBool false -> [Kwd "false"]
  | InsertBool true -> [Kwd "true"]
  | InputInt i -> [Kwd "an"; Kwd "integer"; Input (`Int i)]
  | InputRange (i1,i2) -> Kwd "a" :: Kwd "range" :: Kwd "from " :: syn_Call Range [[Input (`Int i1)]; [Input (`Int i2)]]
  | InputFloat i -> [Kwd "a"; Kwd "float"; Input (`Float i)]
  | InputString i -> [Kwd "a"; Kwd "string"; Input (`String i)]
  | InsertNull -> [Kwd "null"]
  | InsertConcat1 -> syn_Concat [[the_focus]; [ellipsis]]
  | InsertConcat2 -> syn_Concat [[ellipsis]; [the_focus]]
  | InsertExists in_x -> syn_Exists [Input (`Ident in_x)] [the_focus] [ellipsis]
  | InsertForAll in_x -> syn_ForAll [Input (`Ident in_x)] [the_focus] [ellipsis]
  | InsertIf1 -> syn_If [the_focus] [ellipsis] [ellipsis]
  | InsertIf2 -> syn_If [ellipsis] [the_focus] [ellipsis]
  | InsertIf3 -> syn_If [ellipsis] [ellipsis] [the_focus]
  | InsertOr -> syn_Or [[the_focus]; [ellipsis]]
  | InsertAnd -> syn_And [[the_focus]; [ellipsis]]
  | InsertNot -> syn_Not [the_focus]
  | InsertFunc func -> syn_Call func (Focus.make_list (arity_of_func func) [ellipsis])
  | InsertMap -> syn_Map [the_focus] [ellipsis]
  | InsertPred -> syn_Pred [the_focus] [ellipsis]
  | InsertDot -> syn_Dot [the_focus] [ellipsis]
  | InsertField k -> syn_Dot [the_focus] [Word (`String k)]
  | InsertArrayLookup -> syn_ArrayLookup [the_focus] [ellipsis]
  | InsertArrayUnboxing -> syn_ArrayUnboxing [the_focus]
  | InsertVar x -> syn_Var x
  | InsertContextItem -> syn_ContextItem
  | InsertContextEnv -> syn_ContextEnv
  | InsertObject -> syn_EObject [syn_pair [ellipsis] [ellipsis]]
  | InsertArray -> syn_Arrayify [ellipsis]
  | InsertObjectify -> syn_Objectify [the_focus]
  | InsertArrayify -> syn_Arrayify [the_focus]
  | InsertDefFunc1 in_name -> syn_DefFunc [Input (`Ident in_name)] [] [the_focus] [ellipsis]
  | InsertDefFunc2 in_name -> syn_DefFunc [Input (`Ident in_name)] [] [ellipsis] [the_focus]
  | InsertArg in_x -> [Kwd "add"; Kwd "argument"; Input (`Ident in_x)]
  | InputFileData i -> syn_For [Kwd "fields"] [Kwd "file"; Input (`FileData i)] false [the_focus]
  | InsertFor1 (in_x,in_opt) -> syn_For [Input (`Ident in_x)] [the_focus] false [ellipsis] (* TODO: optional *)
  | InsertFor2 (in_x,in_opt) -> syn_For [Input (`Ident in_x)] [ellipsis] false [the_focus] (* TODO: optional *)
  | InsertLet1 in_x -> syn_Let [Input (`Ident in_x)] [the_focus] [ellipsis]
  | InsertLet2 in_x -> syn_Let [Input (`Ident in_x)] [ellipsis] [the_focus]
  | InsertWhere1 -> syn_Where [the_focus] [ellipsis]
  | InsertWhere2 -> syn_Where [ellipsis] [the_focus]
  | InsertGroupBy x -> syn_GroupBy [x] [the_focus]
  | InsertProject x -> syn_Project [x] [the_focus]
  | InsertSlice (in_offset,in_limit) -> syn_Slice [Input (`Int in_offset)] [Input (`Int in_limit)] [the_focus]
  | InsertOrderBy1 o -> syn_OrderBy [syn_order [the_focus] o] [ellipsis]
  | InsertOrderBy2 o -> syn_OrderBy [syn_order [ellipsis] o] [the_focus]
				    
	   
