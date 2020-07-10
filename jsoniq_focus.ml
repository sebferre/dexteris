
open Focus
open Jsoniq

(* focus ctx types *)
       
(* DERIVED *)
type expr_ctx =
  | Root
  | ConcatX of expr list_ctx * expr_ctx
  | Exists1 of var * expr_ctx * expr
  | Exists2 of var * expr * expr_ctx
  | ForAll1 of var * expr_ctx * expr
  | ForAll2 of var * expr * expr_ctx
  | If1 of expr_ctx * expr * expr
  | If2 of expr * expr_ctx * expr
  | If3 of expr * expr * expr_ctx
  | OrX of expr list_ctx * expr_ctx
  | AndX of expr list_ctx * expr_ctx
  | Not1 of expr_ctx
  | CallX of string * expr list_ctx * expr_ctx
  | Map1 of expr_ctx * expr
  | Map2 of expr * expr_ctx
  | Pred1 of expr_ctx * expr
  | Pred2 of expr * expr_ctx
  | Dot1 of expr_ctx * expr
  | Dot2 of expr * expr_ctx
  | ArrayLookup1 of expr_ctx * expr
  | ArrayLookup2 of expr * expr_ctx
  | ArrayUnboxing1 of expr_ctx
  | EObjectX1 of (expr * expr) list_ctx * expr_ctx * expr
  | EObjectX2 of (expr * expr) list_ctx * expr * expr_ctx
  | Objectify1 of expr_ctx
  | Arrayify1 of expr_ctx
  | Let1 of binder * expr_ctx * expr
  | Let2 of binder * expr * expr_ctx
  | DefFunc0 of string * var list * expr_ctx * expr * expr
  | DefFunc1 of string * var list * expr * expr_ctx * expr
  | DefFunc2 of string * var list * expr * expr * expr_ctx
  | Return1 of flower_ctx
  | For1 of binder * flower_ctx * bool * flower
  | FLet1 of binder * flower_ctx * flower
  | Where1 of flower_ctx * flower
  | OrderBy1X of (expr * order) list_ctx * flower_ctx * order * flower

 (* DERIVED *)
and flower_ctx =
  | Flower1 of expr_ctx
  | For2 of binder * expr * bool * flower_ctx
  | FLet2 of binder * expr * flower_ctx
  | Count1 of var * flower_ctx
  | Where2 of expr * flower_ctx
  | GroupBy1 of var list * flower_ctx
  | Project1 of var list * flower_ctx
  | Slice1 of int * int option * flower_ctx
  | OrderBy2 of (expr * order) list * flower_ctx
  | FConcatX of flower list_ctx * flower_ctx
  | FIf1 of flower_ctx * flower * flower
  | FIf2 of flower * flower_ctx * flower
  | FIf3 of flower * flower * flower_ctx

(* DERIVED *)
type focus =
  | AtExpr of expr * expr_ctx
  | AtFlower of flower * flower_ctx

(* focus moves *)
			   
(* DERIVED *)
let rec focus_up : focus -> (focus * path) option = function
  | AtExpr (e, Root) -> None
  | AtExpr (e, ctx) -> Some (focus_expr_up e ctx)
  | AtFlower (f, ctx) -> Some (focus_flower_up f ctx)
and focus_expr_up (e : expr) : expr_ctx -> focus * path = function
  | Root -> assert false
  | ConcatX (ll_rr,ctx) -> AtExpr (Concat (list_of_ctx e ll_rr), ctx),
			   down_rights (List.length (fst ll_rr))
  | Exists1 (x,ctx,e2) -> AtExpr (Exists (x,e,e2), ctx), down_rights 0
  | Exists2 (x,e1,ctx) -> AtExpr (Exists (x,e1,e), ctx), down_rights 1
  | ForAll1 (x,ctx,e2) -> AtExpr (ForAll (x,e,e2), ctx), down_rights 0
  | ForAll2 (x,e1,ctx) -> AtExpr (ForAll (x,e1,e), ctx), down_rights 1
  | If1 (ctx,e2,e3) -> AtExpr (If (e,e2,e3), ctx), down_rights 0
  | If2 (e1,ctx,e3) -> AtExpr (If (e1,e,e3), ctx), down_rights 1
  | If3 (e1,e2,ctx) -> AtExpr (If (e1,e2,e), ctx), down_rights 2
  | OrX (ll_rr,ctx) -> AtExpr (Or (list_of_ctx e ll_rr), ctx), down_rights (List.length (fst ll_rr))
  | AndX (ll_rr,ctx) -> AtExpr (And (list_of_ctx e ll_rr), ctx), down_rights (List.length (fst ll_rr))
  | Not1 ctx -> AtExpr (Not e, ctx), down_rights 0
  | CallX (func,ll_rr,ctx) -> AtExpr (Call (func, list_of_ctx e ll_rr), ctx), down_rights (List.length (fst ll_rr))
  | Map1 (ctx,e2) -> AtExpr (Map (e,e2), ctx), down_rights 0
  | Map2 (e1,ctx) -> AtExpr (Map (e1,e), ctx), down_rights 1
  | Pred1 (ctx,e2) -> AtExpr (Pred (e,e2), ctx), down_rights 0
  | Pred2 (e1,ctx) -> AtExpr (Pred (e1,e), ctx), down_rights 1
  | Dot1 (ctx,e2) -> AtExpr (Dot (e,e2), ctx), down_rights 0
  | Dot2 (e1,ctx) -> AtExpr (Dot (e1,e), ctx), down_rights 1
  | ArrayLookup1 (ctx,e2) -> AtExpr (ArrayLookup (e,e2), ctx), down_rights 0
  | ArrayLookup2 (e1,ctx) -> AtExpr (ArrayLookup (e1,e), ctx), down_rights 1
  | ArrayUnboxing1 ctx -> AtExpr (ArrayUnboxing e, ctx), down_rights 0
  | EObjectX1 (ll_rr,ctx,e2) -> AtExpr (EObject (list_of_ctx (e,e2) ll_rr), ctx), down_rights (List.length (fst ll_rr)) @ [DOWN]
  | EObjectX2 (ll_rr,e1,ctx) -> AtExpr (EObject (list_of_ctx (e1,e) ll_rr), ctx), down_rights (List.length (fst ll_rr)) @ [DOWN;RIGHT]
  | Objectify1 ctx -> AtExpr (Objectify e, ctx), down_rights 0
  | Arrayify1 ctx -> AtExpr (Arrayify e, ctx), down_rights 0
  | Let1 (x,ctx,e2) -> AtExpr (Let (x,e,e2), ctx), down_rights 0
  | Let2 (x,e1,ctx) -> AtExpr (Let (x,e1,e), ctx), down_rights 1
  | DefFunc0 (name,args,ctx,e1,e2) -> AtExpr (DefFunc (name,args,e,e1,e2), ctx), down_rights 0
  | DefFunc1 (name,args,e0,ctx,e2) -> AtExpr (DefFunc (name,args,e0,e,e2), ctx), down_rights 1
  | DefFunc2 (name,args,e0,e1,ctx) -> AtExpr (DefFunc (name,args,e0,e1,e), ctx), down_rights 2
  | Return1 ctx -> AtFlower (Return e, ctx), down_rights 0
  | For1 (x,ctx,opt,f) -> AtFlower (For (x,e,opt,f), ctx), down_rights 0
  | FLet1 (x,ctx,f) -> AtFlower (FLet (x,e,f), ctx), down_rights 0
  | Where1 (ctx,f) -> AtFlower (Where (e,f), ctx), down_rights 0
  | OrderBy1X (ll_rr,ctx,o,f) -> AtFlower (OrderBy (list_of_ctx (e,o) ll_rr, f), ctx), down_rights (List.length (fst ll_rr))
and focus_flower_up (f : flower) : flower_ctx -> focus * path = function
  | Flower1 ctx -> AtExpr (Flower f, ctx), down_rights 0
  | For2 (x,e,opt,ctx) -> AtFlower (For (x,e,opt,f), ctx), down_rights 1
  | FLet2 (x,e,ctx) -> AtFlower (FLet (x,e,f), ctx), down_rights 1
  | Count1 (x,ctx) -> AtFlower (Count (x,f), ctx), down_rights 0
  | Where2 (e,ctx) -> AtFlower (Where (e,f), ctx), down_rights 1
  | GroupBy1 (lx,ctx) -> AtFlower (GroupBy (lx,f), ctx), down_rights 0
  | Project1 (lx,ctx) -> AtFlower (Project (lx,f), ctx), down_rights 0
  | Slice1 (offset,limit,ctx) -> AtFlower (Slice (offset,limit,f), ctx), down_rights 0 
  | OrderBy2 (leo,ctx) -> AtFlower (OrderBy (leo,f), ctx), down_rights (List.length leo)
  | FConcatX (ll_rr,ctx) -> AtFlower (FConcat (list_of_ctx f ll_rr), ctx), down_rights (List.length (fst ll_rr))
  | FIf1 (ctx,f2,f3) -> AtFlower (FIf (f,f2,f3), ctx), down_rights 0
  | FIf2 (f1,ctx,f3) -> AtFlower (FIf (f1,f,f3), ctx), down_rights 1
  | FIf3 (f1,f2,ctx) -> AtFlower (FIf (f1,f2,f), ctx), down_rights 2

(* DERIVED *)
let rec focus_right : focus -> focus option = function
  | AtExpr (e, ctx) -> focus_expr_right e ctx
  | AtFlower (f, ctx) -> focus_flower_right f ctx
and focus_expr_right (e : expr) : expr_ctx -> focus option = function
  | Root -> None
  | ConcatX ((ll,e1::rr),ctx) -> Some (AtExpr (e1, ConcatX ((e::ll,rr), ctx)))
  | ConcatX ((_,[]),_) -> None
  | Exists1 (x,ctx,e2) -> Some (AtExpr (e2, Exists2 (x,e,ctx)))
  | Exists2 _ -> None
  | ForAll1 (x,ctx,e2) -> Some (AtExpr (e2, ForAll2 (x,e,ctx)))
  | ForAll2 _ -> None
  | If1 (ctx,e2,e3) -> Some (AtExpr (e2, If2 (e,ctx,e3)))
  | If2 (e1,ctx,e3) -> Some (AtExpr (e3, If3 (e1,e,ctx)))
  | If3 (e1,e2,ctx) -> None
  | OrX ((ll,e1::rr),ctx) -> Some (AtExpr (e1, OrX ((e::ll,rr),ctx)))
  | OrX ((_,[]),_) -> None
  | AndX ((ll,e1::rr),ctx) -> Some (AtExpr (e1, AndX ((e::ll,rr),ctx)))
  | AndX ((_,[]),_) -> None
  | Not1 ctx -> None
  | CallX (func,(ll,e1::rr),ctx) -> Some (AtExpr (e1, CallX (func, (e::ll,rr), ctx)))
  | CallX (_, (_,[]), _) -> None
  | Map1 (ctx,e2) -> Some (AtExpr (e2, Map2 (e,ctx)))
  | Map2 (e1,ctx) -> None
  | Pred1 (ctx,e2) -> Some (AtExpr (e2, Pred2 (e,ctx)))
  | Pred2 (e1,ctx) -> None
  | Dot1 (ctx,e2) -> Some (AtExpr (e2, Dot2 (e,ctx)))
  | Dot2 (e1,ctx) -> None
  | ArrayLookup1 (ctx,e2) -> Some (AtExpr (e2, ArrayLookup2 (e,ctx)))
  | ArrayLookup2 (e1,ctx) -> None
  | ArrayUnboxing1 ctx -> None
  | EObjectX1 (ll_rr,ctx,e2) -> Some (AtExpr (e2, EObjectX2 (ll_rr,e,ctx)))
  | EObjectX2 ((ll,(e3,e4)::rr),e1,ctx) -> Some (AtExpr (e3, EObjectX1 (((e1,e)::ll,rr), ctx, e3)))
  | EObjectX2 ((_,[]),_,_) -> None
  | Objectify1 ctx -> None
  | Arrayify1 ctx -> None
  | Let1 (x,ctx,e2) -> Some (AtExpr (e2, Let2 (x,e,ctx)))
  | Let2 (x,e1,ctx) -> None
  | DefFunc0 (name,args,ctx,e1,e2) -> Some (AtExpr (e1, DefFunc1 (name,args,e,ctx,e2)))
  | DefFunc1 (name,args,e0,ctx,e2) -> Some (AtExpr (e2, DefFunc2 (name,args,e0,e,ctx)))
  | DefFunc2 (name,args,e0,e1,ctx) -> None
  | Return1 ctx -> None
  | For1 (x,ctx,opt,f) -> Some (AtFlower (f, For2 (x,e,opt,ctx)))
  | FLet1 (x,ctx,f) -> Some (AtFlower (f, FLet2 (x,e,ctx)))
  | Where1 (ctx,f) -> Some (AtFlower (f, Where2 (e,ctx)))
  | OrderBy1X ((ll,(e1,o1)::rr),ctx,o,f) -> Some (AtExpr (e1, OrderBy1X (((e,o)::ll,rr),ctx,o1,f)))
  | OrderBy1X ((ll,[]),ctx,o,f) -> Some (AtFlower (f, OrderBy2 (Focus.list_of_ctx (e,o) (ll,[]), ctx)))
and focus_flower_right (f : flower) : flower_ctx -> focus option = function
  | Flower1 ctx -> None
  | For2 (x,e,opt,ctx) -> None
  | FLet2 (x,e,ctx) -> None
  | Count1 (x,ctx) -> None
  | Where2 (e,ctx) -> None
  | GroupBy1 (lx,ctx) -> None
  | Project1 (lx,ctx) -> None
  | Slice1 (offset,limit,ctx) -> None
  | OrderBy2 (leo,ctx) -> None
  | FConcatX ((ll,f1::rr),ctx) -> Some (AtFlower (f1, FConcatX ((f::ll,rr),ctx)))
  | FConcatX ((_,[]),_) -> None
  | FIf1 (ctx,f2,f3) -> Some (AtFlower (f2, FIf2 (f,ctx,f3)))
  | FIf2 (f1,ctx,f3) -> Some (AtFlower (f3, FIf3 (f1,f,ctx)))
  | FIf3 (f1,f2,ctx) -> None

let rec focus_next_Empty (foc : focus) : focus option =
  match foc with
  | AtExpr (Empty, ctx) -> Some foc
  | _ ->
     match focus_right foc with
     | Some foc_right -> focus_next_Empty foc_right
     | None ->
	match focus_up foc with
	| Some (foc_up,_) -> focus_next_Empty foc_up
	| None -> None
	  

(* conversions between focus and (expr,path) *)
			  
(* DERIVED *)
let rec focus_of_path_expr (ctx : expr_ctx) : path * expr -> focus = function
  | [], e -> AtExpr (e,ctx)
  | DOWN::path, Concat le ->
     let path, ll_rr, x = list_focus_of_path_list path le in
     focus_of_path_expr (ConcatX (ll_rr,ctx)) (path,x)
  | DOWN::path, Flower f -> focus_of_path_flower (Flower1 ctx) (path,f)
  | DOWN::RIGHT::path, Exists (x,e1,e2) -> focus_of_path_expr (Exists2 (x,e1,ctx)) (path,e2)
  | DOWN::path, Exists (x,e1,e2) -> focus_of_path_expr (Exists1 (x,ctx,e1)) (path,e1)
  | DOWN::RIGHT::path, ForAll (x,e1,e2) -> focus_of_path_expr (ForAll2 (x,e1,ctx)) (path,e2)
  | DOWN::path, ForAll (x,e1,e2) -> focus_of_path_expr (ForAll1 (x,ctx,e1)) (path,e1)
  | DOWN::RIGHT::RIGHT::path, If (e1,e2,e3) -> focus_of_path_expr (If3 (e1,e2,ctx)) (path,e3)
  | DOWN::RIGHT::path, If (e1,e2,e3) -> focus_of_path_expr (If2 (e1,ctx,e3)) (path,e2)
  | DOWN::path, If (e1,e2,e3) -> focus_of_path_expr (If1 (ctx,e2,e3)) (path,e1)
  | DOWN::path, Or le ->
     let path, ll_rr, x = list_focus_of_path_list path le in
     focus_of_path_expr (OrX (ll_rr,ctx)) (path,x)
  | DOWN::path, And le ->
     let path, ll_rr, x = list_focus_of_path_list path le in
     focus_of_path_expr (AndX (ll_rr,ctx)) (path,x)
  | DOWN::path, Not e1 -> focus_of_path_expr (Not1 ctx) (path,e1)
  | DOWN::path, Call (f,le) ->
     let path, ll_rr, x = list_focus_of_path_list path le in
     focus_of_path_expr (CallX (f,ll_rr,ctx)) (path,x)
  | DOWN::RIGHT::path, Map (e1,e2) -> focus_of_path_expr (Map2 (e1,ctx)) (path,e2)
  | DOWN::path, Map (e1,e2) -> focus_of_path_expr (Map1 (ctx,e2)) (path,e1)
  | DOWN::RIGHT::path, Pred (e1,e2) -> focus_of_path_expr (Pred2 (e1,ctx)) (path,e2)
  | DOWN::path, Pred (e1,e2) -> focus_of_path_expr (Pred1 (ctx,e2)) (path,e1)
  | DOWN::RIGHT::path, Dot (e1,e2) -> focus_of_path_expr (Dot2 (e1,ctx)) (path,e2)
  | DOWN::path, Dot (e1,e2) -> focus_of_path_expr (Dot1 (ctx,e2)) (path,e1)
  | DOWN::RIGHT::path, ArrayLookup (e1,e2) -> focus_of_path_expr (ArrayLookup2 (e1,ctx)) (path,e2)
  | DOWN::path, ArrayLookup (e1,e2) -> focus_of_path_expr (ArrayLookup1 (ctx,e2)) (path,e1)
  | DOWN::path, ArrayUnboxing e1 -> focus_of_path_expr (ArrayUnboxing1 ctx) (path,e1)
  | DOWN::path, EObject pairs ->
     let path, ll_rr, pair = list_focus_of_path_list path pairs in
     ( match path, pair with
       | DOWN::RIGHT::path, (e1,e2) -> focus_of_path_expr (EObjectX2 (ll_rr,e1,ctx)) (path,e2)
       | DOWN::path, (e1,e2) -> focus_of_path_expr (EObjectX1 (ll_rr,ctx,e2)) (path,e1)
       | _ -> raise Invalid_path)
  | DOWN::path, Objectify e1 -> focus_of_path_expr (Objectify1 ctx) (path,e1)
  | DOWN::path, Arrayify e1 -> focus_of_path_expr (Arrayify1 ctx) (path,e1)
  | DOWN::RIGHT::path, Let (x,e1,e2) -> focus_of_path_expr (Let2 (x,e1,ctx)) (path,e2)
  | DOWN::path, Let (x,e1,e2) -> focus_of_path_expr (Let1 (x,ctx,e2)) (path,e1)
  | DOWN::RIGHT::RIGHT::path, DefFunc (f,lx,e0,e1,e2) -> focus_of_path_expr (DefFunc2 (f,lx,e0,e1,ctx)) (path,e2)
  | DOWN::RIGHT::path, DefFunc (f,lx,e0,e1,e2) -> focus_of_path_expr (DefFunc1 (f,lx,e0,ctx,e2)) (path,e1)
  | DOWN::path, DefFunc (f,lx,e0,e1,e2) -> focus_of_path_expr (DefFunc0 (f,lx,ctx,e1,e2)) (path,e0)
  | _ -> raise Invalid_path
and focus_of_path_flower (ctx : flower_ctx) : path * flower -> focus = function
  | [], f -> AtFlower (f,ctx)
  | DOWN::path, Return e1 -> focus_of_path_expr (Return1 ctx) (path,e1)
  | DOWN::RIGHT::path, For (x,e1,b,f) -> focus_of_path_flower (For2 (x,e1,b,ctx)) (path,f)
  | DOWN::path, For (x,e1,b,f) -> focus_of_path_expr (For1 (x,ctx,b,f)) (path,e1)
  | DOWN::RIGHT::path, FLet (x,e1,f) -> focus_of_path_flower (FLet2 (x,e1,ctx)) (path,f)
  | DOWN::path, FLet (x,e1,f) -> focus_of_path_expr (FLet1 (x,ctx,f)) (path,e1)
  | DOWN::path, Count (x,f) -> focus_of_path_flower (Count1 (x,ctx)) (path,f)
  | DOWN::RIGHT::path, Where (e1,f) -> focus_of_path_flower (Where2 (e1,ctx)) (path,f)
  | DOWN::path, Where (e1,f) -> focus_of_path_expr (Where1 (ctx,f)) (path,e1)
  | DOWN::RIGHT::path, OrderBy (leo,f) -> focus_of_path_flower (OrderBy2 (leo,ctx)) (path,f)
  | DOWN::path, OrderBy (leo,f) ->
     let path, ll_rr, (e,o) = list_focus_of_path_list path leo in
     focus_of_path_expr (OrderBy1X (ll_rr,ctx,o,f)) (path,e)
  | DOWN::path, FConcat lf ->
     let path, ll_rr, x = list_focus_of_path_list path lf in
     focus_of_path_flower (FConcatX (ll_rr,ctx)) (path,x)
  | DOWN::RIGHT::RIGHT::path, FIf (e1,e2,e3) -> focus_of_path_flower (FIf3 (e1,e2,ctx)) (path,e3)
  | DOWN::RIGHT::path, FIf (e1,e2,e3) -> focus_of_path_flower (FIf2 (e1,ctx,e3)) (path,e2)
  | DOWN::path, FIf (e1,e2,e3) -> focus_of_path_flower (FIf1 (ctx,e2,e3)) (path,e1)
  | _ -> raise Invalid_path
		
let expr_path_of_focus (foc : focus) : expr * path =
  let rec aux foc path =
    match focus_up foc, foc with
    | None, AtExpr (e,Root) -> e, path
    | None, _ -> assert false
    | Some (foc',path'), _ -> aux foc' (path'@path) in
  aux foc []

let focus_of_expr_path (e, path : expr * path) : focus =
  focus_of_path_expr Root (path,e)

let focus_to_yojson (foc : focus) : Yojson.Safe.t =
  let e, path = expr_path_of_focus foc in
  `Assoc [ "expression", Jsoniq.expr_to_yojson e;
	   "path", Focus.path_to_yojson path ]

let focus_of_yojson (x : Yojson.Safe.t) : (focus,string) Result.result =
  match x with
  | `Assoc ["expression", x_e; "path", x_path] ->
     Result.bind
       (expr_of_yojson x_e)
       (fun e ->
	Result.bind
	  (path_of_yojson x_path)
	  (fun path ->
	   let foc = focus_of_expr_path (e,path) in
	   Result.Ok foc))
  | _ -> Result.Error "Invalid serialization of a focus"
      
(* focus transformations and navigation paths *)
			   
let initial_focus = AtExpr (Empty, Root)


type transf =
  | FocusUp
  | FocusRight
  | Delete
  | InsertBool of bool
  | InputInt of int input
  | InputRange of int input * int input
  | InputFloat of float input
  | InputString of string input
  | InputFileString of (string * string) input
  | InsertNull
  | InsertConcat1
  | InsertConcat2
  | InsertExists of var input
  | InsertForAll of var input
  | InsertIf1 | InsertIf2 | InsertIf3
  | InsertOr
  | InsertAnd
  | InsertNot
  | InsertFunc of string * int (* name, arity *)
  | InsertMap
  | InsertPred
  | InsertDot
  | InsertField of string
  | InsertArrayLookup
  | InsertArrayUnboxing
  | InsertVar of var
  | InsertContextItem
  | InsertContextEnv
  | InsertObject
  | InsertObjectField
  | InsertArray
  | InsertObjectify
  | InsertArrayify
  | InsertDefFunc1 of string input
  | InsertDefFunc2 of string input
  | InsertArg of var input
  | InsertForVar1 of var input * bool input
  | InsertForVar2 of var input * bool input
  | InsertForFields1 of bool input
  | InsertLetVar1 of var input
  | InsertLetVar2 of var input
  | InsertLetFields1
  | InsertCount1 of var input
  | InsertWhere1
  | InsertWhere2
  | InsertGroupBy of var list * var input
  | InsertProject of var list * var input
  | InsertSlice of int input * int input
  | InsertOrderBy1 of string input
  | InsertOrderBy2 of string input

			     			     
(* TODO: avoid implicit focus change in transf application, make it global *)
		       
let rec reaching_list reaching_elt sep l =
  match l with
  | [] -> []
  | [x] -> reaching_elt x @ [FocusUp]
  | x::r -> reaching_elt x @ sep @ reaching_list reaching_elt sep r

let rec reaching_expr : expr -> transf list = function
  | S s -> [InputString (new input s)]
  | Item i -> reaching_item i
  | FileString (fname,contents) -> [InputFileString (new input (fname,contents))]
  | Empty -> [] (* the default value *)
  | Concat le -> reaching_list reaching_expr [InsertConcat1] le (* assuming |le| > 1 *)
  | Flower f -> reaching_flower f
  | Exists (x,e1,e2) -> reaching_expr e1 @ InsertExists (new input x) :: reaching_expr e2 @ [FocusUp]
  | ForAll (x,e1,e2) -> reaching_expr e1 @ InsertForAll (new input x) :: reaching_expr e2 @ [FocusUp]
  | If (e1,e2,e3) -> reaching_expr e1 @ InsertIf1 :: reaching_expr e2 @ FocusRight :: reaching_expr e3 @ [FocusUp]
  | Or le -> reaching_list reaching_expr [InsertOr] le
  | And le -> reaching_list reaching_expr [InsertAnd] le
  | Not e -> reaching_expr e @ [InsertNot]
  | Call (func,le) -> InsertFunc (func, List.length le) :: reaching_list reaching_expr [FocusRight] le
  | Map (e1,e2) -> reaching_expr e1 @ InsertMap :: reaching_expr e2 @ [FocusUp]
  | Pred (e1,e2) -> reaching_expr e1 @ InsertPred :: reaching_expr e2 @ [FocusUp]
  | Dot (e1,e2) -> reaching_expr e1 @ InsertDot :: reaching_expr e2 @ [FocusUp]
  | ArrayLookup (e1,e2) -> reaching_expr e1 @ InsertArrayLookup :: reaching_expr e2 @ [FocusUp]
  | ArrayUnboxing e -> reaching_expr e @ [InsertArrayUnboxing]
  | Var x -> [InsertVar x]
  | ContextItem -> [InsertContextItem]
  | ContextEnv -> [InsertContextEnv]
  | EObject pairs -> InsertObject :: reaching_list reaching_pair [InsertObjectField] pairs
  | Objectify e -> reaching_expr e @ [InsertObjectify]
  | Arrayify e -> reaching_expr e @ [InsertArrayify]
  | Let (Var x,e1,e2) -> reaching_expr e1 @ InsertLetVar1 (new input x) :: reaching_expr e2 @ [FocusUp]
  | Let (Fields,e1,e2) -> reaching_expr e1 @ InsertLetFields1 :: reaching_expr e2 @ [FocusUp]
  | DefFunc (name,args,e0,e1,e2) -> InsertDefFunc1 (new input name) :: List.map (fun x -> InsertArg (new input x)) args @ reaching_expr e0 @ FocusRight :: reaching_expr e1 @ FocusRight :: reaching_expr e2 @ [FocusUp]
and reaching_flower : flower -> transf list = function
  | Return e -> reaching_expr e @ [FocusUp]
  | For (Var x,e,opt,f) -> reaching_expr e @ InsertForVar1 (new input x, new input opt) :: reaching_flower f @ [FocusUp]
  | For (Fields,e,opt,f) -> reaching_expr e @ InsertForFields1 (new input opt) :: reaching_flower f @ [FocusUp]
  | FLet (Var x,e,f) -> reaching_expr e @ InsertLetVar1 (new input x) :: reaching_flower f @ [FocusUp]
  | FLet (Fields,e,f) -> reaching_expr e @ InsertLetFields1 :: reaching_flower f @ [FocusUp]
  | Count (x,f) -> InsertCount1 (new input x) :: reaching_flower f @ [FocusUp]
  | Where (e,f) -> reaching_expr e @ InsertWhere1 :: reaching_flower f @ [FocusUp]
  | GroupBy (lx,f) -> List.map (fun x -> InsertGroupBy (lx, new input x)) lx @ reaching_flower f @ [FocusUp]
  | Project (lx,f) -> List.map (fun x -> InsertProject (lx, new input x)) lx @ reaching_flower f @ [FocusUp]
  | Slice (offset,limit,f) ->
     let l = match limit with None -> 0 | Some l -> l in
     InsertSlice (new input offset, new input l) :: reaching_flower f @ [FocusUp]
  | OrderBy (leo,f) -> List.concat (List.map (fun (e,o) -> reaching_expr e @ [InsertOrderBy1 (new input (string_of_order o))]) leo) @ reaching_flower f @ [FocusUp]
  | FConcat lf -> reaching_list reaching_flower [InsertConcat1] lf
  | FIf (f1,f2,f3) -> reaching_flower f1 @ InsertIf1 :: reaching_flower f2 @ FocusRight :: reaching_flower f3 @ [FocusUp]
and reaching_data (d : data) : transf list =
  reaching_list reaching_item [InsertConcat1] (Seq.to_list d)
and reaching_item : item -> transf list = function
  | `Bool b -> [InsertBool b]
  | `Int n -> [InputInt (new input n)]
  | `Float f -> [InputFloat (new input f)]
  | `String s -> [InputString (new input s)]
  | `Null -> [InsertNull]
  | `Assoc pairs -> InsertObject :: reaching_list reaching_pair [InsertObjectField] (List.map (fun (k,i) -> S k, Item i) pairs)
  | `List li -> InsertArray :: reaching_list reaching_item [InsertConcat1] li
and reaching_pair (e1, e2: expr * expr) : transf list =
  reaching_expr e1 @ FocusRight :: reaching_expr e2

let flower_of_expr = function
  | Flower f -> f
  | e -> Return e
let expr_of_flower = function
  | Return e -> e
  | f -> Flower f
let ctx_flower_of_expr = function
  | Return1 ctx_f -> ctx_f
  | ctx_e -> Flower1 ctx_e
let ctx_expr_of_flower = function
  | Flower1 ctx_e -> ctx_e
  | ctx_f -> Return1 ctx_f
		     
let rec delete (foc : focus) : focus option =
  match foc with
  | AtExpr (Empty,ctx) -> delete_ctx_expr ctx
     (* match focus_up foc with
       | None -> None
       | Some (foc_up,_) -> delete foc_up *)
  (*  | AtExpr (_, ArrayUnboxing1 ctx) -> Some (AtExpr (Empty,ctx)) *)
  | AtExpr (_,ctx) -> Some (AtExpr (Empty, ctx))
  | AtFlower (Return Empty, ctx) -> delete_ctx_flower ctx
     (* match focus_up foc with
       | None -> None
       e| Some (foc_up,_) -> delete foc_up *)
  | AtFlower (f, ctx) -> Some (AtExpr (Empty, ctx_expr_of_flower ctx))
and delete_ctx_expr : expr_ctx -> focus option = function
  | Root -> None
  | ConcatX (ll_rr,ctx) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | [e] -> e
       | le -> Concat le in
     Some (AtExpr (e, ctx))
  | Exists1 (x,ctx,e2) -> Some (AtExpr (e2,ctx))
  | Exists2 (x,e1,ctx) -> Some (AtExpr (e1,ctx))
  | ForAll1 (x,ctx,e2) -> Some (AtExpr (e2,ctx))
  | ForAll2 (x,e1,ctx) -> Some (AtExpr (e1,ctx))
  | If1 (ctx,e2,e3) -> Some (AtExpr (e3, ctx))
  | If2 (e1,ctx,e3) -> Some (AtExpr (e3,ctx))
  | If3 (e1,e2,ctx) -> Some (AtExpr (e2,ctx))
  | OrX (ll_rr,ctx) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | [e] -> e
       | le -> Or le in
     Some (AtExpr (e,ctx))
  | AndX (ll_rr,ctx) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | [e] -> e
       | le -> And le in
     Some (AtExpr (e,ctx))
  | Not1 ctx -> Some (AtExpr (Empty,ctx))
  | CallX (func,ll_rr,ctx) -> Some (AtExpr (Empty,ctx))
  | Map1 (ctx,e2) -> Some (AtExpr (Empty,ctx))
  | Map2 (e1,ctx) -> Some (AtExpr (e1,ctx))
  | Pred1 (ctx,e2) -> Some (AtExpr (Empty,ctx))
  | Pred2 (e1,ctx) -> Some (AtExpr (e1,ctx))
  | Dot1 (ctx,e2) -> Some (AtExpr (Empty,ctx))
  | Dot2 (e1,ctx) -> Some (AtExpr (e1,ctx))
  | ArrayLookup1 (ctx,e2) -> Some (AtExpr (Empty,ctx))
  | ArrayLookup2 (e1,ctx) -> Some (AtExpr (e1,ctx))
  | ArrayUnboxing1 ctx -> Some (AtExpr (Empty,ctx))
  | EObjectX1 (ll_rr,ctx,e2) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | pairs -> EObject pairs in
     Some (AtExpr (e,ctx))
  | EObjectX2 (ll_rr,e1,ctx) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | pairs -> EObject pairs in
     Some (AtExpr (e,ctx))
  | Objectify1 ctx -> Some (AtExpr (Empty,ctx))
  | Arrayify1 ctx -> Some (AtExpr (Empty,ctx))
  | Let1 (br,ctx,e2) -> Some (AtExpr (e2,ctx))
  | Let2 (br,e1,ctx) -> Some (AtExpr (e1,ctx))
  | DefFunc0 (f,lx,ctx,e1,e2) -> Some (AtExpr (e2,ctx))
  | DefFunc1 (f,lx,e0,ctx,e2) -> Some (AtExpr (e2,ctx))
  | DefFunc2 (f,lx,e0,e1,ctx) -> Some (AtExpr (e1,ctx))
  | Return1 ctx -> delete_ctx_flower ctx
  | For1 (x,ctx,opt,f) -> Some (AtFlower (f,ctx))
  | FLet1 (br,ctx,f) -> Some (AtFlower (f,ctx))
  | Where1 (ctx,f) -> Some (AtFlower (f,ctx))
  | OrderBy1X (ll_rr,ctx,o,f1) ->
     let f =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> f1
       | leo -> OrderBy (leo,f1) in
     Some (AtFlower (f,ctx))
and delete_ctx_flower : flower_ctx -> focus option = function
  | Flower1 ctx -> delete_ctx_expr ctx
  | For2 (x,e,opt,ctx) -> Some (AtExpr (e, ctx_expr_of_flower ctx))
  | FLet2 (br,e,ctx) -> Some (AtExpr (e, ctx_expr_of_flower ctx))
  | Count1 (x,ctx) -> Some (AtExpr (Empty, ctx_expr_of_flower ctx))
  | Where2 (e,ctx) -> Some (AtExpr (e, ctx_expr_of_flower ctx))
  | GroupBy1 (lx,ctx) -> Some (AtExpr (Empty, ctx_expr_of_flower ctx))
  | Project1 (lx,ctx) -> Some (AtExpr (Empty, ctx_expr_of_flower ctx))
  | Slice1 (o,l,ctx) -> Some (AtExpr (Empty, ctx_expr_of_flower ctx))
  | OrderBy2 (leo,ctx) -> Some (AtExpr (Empty, ctx_expr_of_flower ctx))
  | FConcatX (ll_rr,ctx) ->
     let f =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Return Empty
       | [f] -> f
       | lf -> FConcat lf in
     Some (AtFlower (f, ctx))
  | FIf1 (ctx,f2,f3) -> Some (AtFlower (f3, ctx))
  | FIf2 (f1,ctx,f3) -> Some (AtFlower (f3,ctx))
  | FIf3 (f1,f2,ctx) -> Some (AtFlower (f2,ctx))

			     
let rec list_switch x = function
  | [] -> [x]
  | x1::lx1 ->
     if x1 = x
     then lx1
     else x1::list_switch x lx1
			      
let rec apply_transf (transf : transf) (foc : focus) : focus option =
  match transf with
  | FocusUp -> Option.map fst (focus_up foc)
  | FocusRight -> focus_right foc
  | Delete -> delete foc
  | _ ->
     let e, ctx_e =
       match foc with
       | AtExpr (e,ctx) -> e, ctx
       | AtFlower (f,ctx) -> expr_of_flower f, ctx_expr_of_flower ctx in
     match apply_transf_expr (transf,e,ctx_e) with
     | None -> None
     | Some (Flower f, Return1 ctx) -> Some (AtFlower (f,ctx))
     | Some (e',ctx') -> Some (AtExpr (e',ctx'))
and apply_transf_expr = function
  | (FocusUp | FocusRight | Delete), _, _ -> assert false
  | InsertBool b, _, ctx -> Some (Item (`Bool b), ctx)
  | InputInt in_n, _, ctx -> Some (Item (`Int in_n#get), ctx)
  | InputRange (in_a,in_b), _, ctx -> Some (Call ("range", [Item (`Int in_a#get); Item (`Int in_b#get)]), ctx)
  | InputFloat in_f, _, ctx -> Some (Item (`Float in_f#get), ctx)
  | InputString in_s, _, ctx -> Some (Item (`String in_s#get), ctx)
  | InputFileString in_file, _, ctx ->
     let filename, contents = in_file#get in
     Some (FileString (filename, contents), ctx)
     
  | InsertNull, _, ctx -> Some (Item `Null, ctx)

  | InsertConcat1, e, EObjectX1 ((ll,rr), ctx, e2) -> Some (Empty, EObjectX1 (((e,e2)::ll,rr), ctx, Empty))
  | InsertConcat1, e, EObjectX2 ((ll,rr), e1, ctx) -> Some (Empty, EObjectX1 (((e1,e)::ll,rr), ctx, Empty))

  | InsertConcat1, Empty, _ -> None
  | InsertConcat1, e, ConcatX ((ll,rr), ctx) -> Some (Empty, ConcatX ((e::ll,rr), ctx))
  | InsertConcat1, Concat le, ctx -> Some (Empty, ConcatX ((List.rev le,[]), ctx))
  | InsertConcat1, e, ctx -> Some (Empty, ConcatX (([e],[]), ctx))
  | InsertConcat2, Empty, _ -> None
  | InsertConcat2, e, ConcatX ((ll,rr), ctx) -> Some (Empty, ConcatX ((ll,e::rr), ctx))
  | InsertConcat2, Concat le, ctx -> Some (Empty, ConcatX (([],le), ctx))
  | InsertConcat2, e, ctx -> Some (Empty, ConcatX (([],[e]), ctx))

  | InsertExists in_x, Empty, ctx -> Some (Empty, Exists1 (in_x#get,ctx,Empty))
  | InsertExists in_x, e, ctx -> Some (Empty, Exists2 (in_x#get,e,ctx))
  | InsertForAll in_x, Empty, ctx -> Some (Empty, ForAll1 (in_x#get,ctx,Empty))
  | InsertForAll in_x, e, ctx -> Some (Empty, ForAll2 (in_x#get,e,ctx))

  | InsertIf1, e, ctx -> Some (Empty, If2 (e,ctx,Empty))
  | InsertIf2, e, ctx -> Some (Empty, If1 (ctx,e,Empty))
  | InsertIf3, e, ctx -> Some (Empty, If1 (ctx,Empty,e))

  | InsertOr, e, OrX ((ll,rr), ctx) -> Some (Empty, OrX ((e::ll,rr), ctx))
  | InsertOr, Or le, ctx -> Some (Empty, OrX ((List.rev le,[]), ctx))
  | InsertOr, e, ctx -> Some (Empty, OrX (([e],[]), ctx))

  | InsertAnd, e, AndX ((ll,rr), ctx) -> Some (Empty, AndX ((e::ll,rr), ctx))
  | InsertAnd, And le, ctx -> Some (Empty, AndX ((List.rev le,[]), ctx))
  | InsertAnd, e, ctx -> Some (Empty, AndX (([e],[]), ctx))

  | InsertNot, Not e, ctx -> Some (e,ctx)
  | InsertNot, e, Not1 ctx -> Some (e,ctx)
  | InsertNot, e, ctx -> Some (Not e, ctx)

  | InsertFunc (func,n), e, ctx ->
     if n = 0 then
       Some (Call (func, []), ctx)
     else if e=Empty then
       Some (e, CallX (func, ([],make_list (n-1) Empty), ctx))
     else if n > 1 then
       Some (Empty, CallX (func, ([e],make_list (n-2) Empty), ctx))
     else Some (Call (func, [e]), ctx)

  | InsertMap, e, ctx -> Some (Empty, Map2 (e, ctx))
  | InsertPred, e, ctx -> Some (Empty, Pred2 (e, ctx))
  | InsertDot, e, ctx -> Some (Empty, Dot2 (e, ctx))
  | InsertField k, Dot (e,_), ctx -> Some (Dot (e, S k), ctx)
  | InsertField k, e, ctx -> Some (Dot (e, S k), ctx)
  | InsertArrayLookup, e, ctx -> Some (Empty, ArrayLookup2 (e, ctx))

  | InsertArrayUnboxing, e, ctx -> Some (ArrayUnboxing e, ctx)

  | InsertVar x, Empty, ctx -> Some (Var x, ctx)
  | InsertVar _, _, _ -> None
  | InsertContextItem, Empty, ctx -> Some (ContextItem, ctx)
  | InsertContextItem, _, _ -> None
  | InsertContextEnv, Empty, ctx -> Some (ContextEnv, ctx)
  | InsertContextEnv, _, _ -> None

  | InsertObject, e, ctx -> Some (Empty, EObjectX1 (([],[]), ctx, e))
  | InsertObjectField, e1, EObjectX1 ((ll,rr), ctx, e2) -> Some (Empty, EObjectX1 (((e1,e2)::ll,rr), ctx, Empty))
  | InsertObjectField, e2, EObjectX2 ((ll,rr), e1, ctx) -> Some (Empty, EObjectX1 (((e1,e2)::ll,rr), ctx, Empty))
  | InsertObjectField, _, _ -> None
  | InsertArray, Empty, ctx -> Some (Empty, Arrayify1 ctx)
  | InsertArray, _, _ -> None
  | InsertObjectify, e, ctx -> Some (Objectify e, ctx)
  | InsertArrayify, e, ctx -> Some (Arrayify e, ctx)

  | InsertDefFunc1 in_name, e, ctx -> Some (Empty, DefFunc0 (in_name#get, [], ctx, e, Empty))
  | InsertDefFunc2 in_name, e, ctx -> Some (Empty, DefFunc0 (in_name#get, [], ctx, Empty, e))
  | InsertArg in_x, DefFunc (name,args,inputs,e1,e2), ctx ->
     let args = args @ [in_x#get] in
     Some (DefFunc (name, args, inputs, e1, e2), ctx)
  | InsertArg in_x, e0, DefFunc0 (name,args,ctx,e1,e2) ->
     let args = args @ [in_x#get] in
     Some (e0, DefFunc0 (name, args, ctx, e1, e2))
  | InsertArg _, _, _ -> None

  | InsertForVar1 (in_x,in_opt), e, ctx -> Some (Empty, Return1 (For2 (Var in_x#get, e, in_opt#get, ctx_flower_of_expr ctx)))
  | InsertForVar2 (in_x,in_opt), e, ctx -> Some (Empty, For1 (Var in_x#get, ctx_flower_of_expr ctx, in_opt#get, flower_of_expr e))
  | InsertForFields1 (in_opt), e, ctx -> Some (Empty, Return1 (For2 (Fields, e, in_opt#get, ctx_flower_of_expr ctx)))

  | InsertLetVar1 in_x, e, Return1 ctx -> Some (Empty, Return1 (FLet2 (Var in_x#get, e, ctx)))
  | InsertLetVar2 in_x, e, Return1 ctx -> Some (Empty, FLet1 (Var in_x#get, ctx, flower_of_expr e))
  | InsertLetFields1, e, Return1 ctx -> Some (Empty, Return1 (FLet2 (Fields, e, ctx)))
  | InsertLetVar1 in_x, e, ctx -> Some (Empty, Let2 (Var in_x#get, e, ctx))
  | InsertLetVar2 in_x, e, ctx -> Some (Empty, Let1 (Var in_x#get, ctx, e))
  | InsertLetFields1, e, ctx -> Some (Empty, Let2 (Fields, e, ctx))

  | InsertCount1 in_x, e, Return1 ctx -> Some (e, Return1 (Count1 (in_x#get, ctx)))
  | InsertCount1 _, _, _ -> None
				     
  (* transformations below should only be suggested in for context *)

  | InsertWhere1, e, ctx -> Some (Empty, Return1 (Where2 (e, ctx_flower_of_expr ctx)))
  | InsertWhere2, e, ctx -> Some (Empty, Where1 (ctx_flower_of_expr ctx, flower_of_expr e))

  | InsertGroupBy (_, in_x), e, Return1 (GroupBy1 (lx,ctx)) -> Some (e, Return1 (GroupBy1 (list_switch in_x#get lx,ctx)))					    
  | InsertGroupBy (_, in_x), Flower (GroupBy (lx,f)), ctx -> Some (Flower (GroupBy (list_switch in_x#get lx,f)), ctx)						 
  | InsertGroupBy (_, in_x), e, ctx -> Some (e, Return1 (GroupBy1 ([in_x#get], ctx_flower_of_expr ctx)))

  | InsertProject (_, in_x), e, Return1 (Project1 (lx,ctx)) -> Some (e, Return1 (Project1 (list_switch in_x#get lx,ctx)))					    
  | InsertProject (_, in_x), Flower (Project (lx,f)), ctx -> Some (Flower (Project (list_switch in_x#get lx,f)), ctx)						 
  | InsertProject (_, in_x), e, ctx -> Some (Flower (Project ([in_x#get], flower_of_expr e)), ctx)
(*  | InsertProject in_lx, e, Return1 (Project1 (_,ctx)) -> Some (e, Return1 (Project1 (in_lx#get, ctx)))
  | InsertProject in_lx, Flower (Project (_,f)), ctx -> Some (Flower (Project (in_lx#get, f)), ctx)						 
  | InsertProject in_lx, e, ctx -> Some (Flower (Project (in_lx#get, flower_of_expr e)), ctx) *)

  | InsertSlice (in_offset,in_limit), e, ctx ->
     let offset = in_offset#get in
     let limit = match in_limit#get with 0 -> None | l -> Some l in
     Some (e, Return1 (Slice1 (offset, limit, ctx_flower_of_expr ctx)))

  | InsertOrderBy1 in_o, e, Return1 (OrderBy2 (leo, ctx)) -> Some (Empty, Return1 (OrderBy2 (leo@[e, order_of_string in_o#get], ctx)))
  | InsertOrderBy1 in_o, e, ctx -> Some (Empty, Return1 (OrderBy2 ([e, order_of_string in_o#get], ctx_flower_of_expr ctx)))
  | InsertOrderBy2 in_o, e, ctx -> Some (Empty, OrderBy1X (([],[]), ctx_flower_of_expr ctx, order_of_string in_o#get, flower_of_expr e))

