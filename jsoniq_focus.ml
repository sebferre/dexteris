
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
  | Hide1 of var list * flower_ctx
  | Slice1 of int * int option * flower_ctx
  | OrderBy2 of (expr * order) list * flower_ctx
  | FConcatX of flower list_ctx * flower_ctx
  | FIf1 of flower_ctx * flower * flower
  | FIf2 of flower * flower_ctx * flower
  | FIf3 of flower * flower * flower_ctx

let flower1 = function
  | Return1 ctx_f -> ctx_f
  | ctx_e -> Flower1 ctx_e
let return1 = function
  | Flower1 ctx_e -> ctx_e
  | ctx_f -> Return1 ctx_f

(* DERIVED *)
type focus =
  | AtExpr of expr * expr_ctx (* USE function [at_expr] to build values *)
  | AtFlower of flower * flower_ctx (* USE functio [at_flower] to build values *)

let rec at_expr (e : expr) (ctx : expr_ctx) : focus =
  match e with
  | Flower f -> at_flower f (flower1 ctx)
  | _ -> AtExpr (e, ctx)
and at_flower (f : flower) (ctx : flower_ctx) : focus =
  match f with
  | Return e -> at_expr e (return1 ctx)
  | _ -> AtFlower (f,ctx)
       
let is_empty_focus = function
  | AtExpr (Empty, _) -> true
  | _ -> false

let rec var_of_focus : focus -> var option = function
  | AtExpr (Var x, _) -> Some x
  | AtExpr (ContextItem, _) -> Some var_context
  | AtExpr (e,ctx) -> var_of_expr_ctx ctx
  | AtFlower (f,ctx) -> var_of_flower_ctx ctx
and var_of_expr_ctx = function
  | Exists1 (x,_,_) -> Some x
  | ForAll1 (x,_,_) -> Some x
  | If2 (_,ctx2,_) -> var_of_expr_ctx ctx2
  | If3 (_,_,ctx2) -> var_of_expr_ctx ctx2
  | Pred1 (ctx2,_) -> var_of_expr_ctx ctx2
  | Let1 (Var x,_,_) -> Some x
  | Let2 (_,_,ctx2) -> var_of_expr_ctx ctx2
  | For1 (Var x,_,_,_) -> Some x
  | FLet1 (Var x,_,_) -> Some x
  | _ -> None
and var_of_flower_ctx = function
  | _ -> None
       
(* focus moves *)

(* DERIVED *)
let rec focus_up : focus -> (focus * path) option = function
  | AtExpr (e, Root) -> None
  | AtExpr (e, ctx) -> Some (focus_expr_up e ctx)
  | AtFlower (f, Flower1 Root) -> None
  | AtFlower (f, ctx) -> Some (focus_flower_up f ctx)
and focus_expr_up (e : expr) : expr_ctx -> focus * path = function
  | Root -> assert false
  | ConcatX (ll_rr,ctx) -> at_expr (Concat (list_of_ctx e ll_rr)) ctx,
			   down_rights (List.length (fst ll_rr))
  | Exists1 (x,ctx,e2) -> at_expr (Exists (x,e,e2)) ctx, down_rights 0
  | Exists2 (x,e1,ctx) -> at_expr (Exists (x,e1,e)) ctx, down_rights 1
  | ForAll1 (x,ctx,e2) -> at_expr (ForAll (x,e,e2)) ctx, down_rights 0
  | ForAll2 (x,e1,ctx) -> at_expr (ForAll (x,e1,e)) ctx, down_rights 1
  | If1 (ctx,e2,e3) -> at_expr (If (e,e2,e3)) ctx, down_rights 0
  | If2 (e1,ctx,e3) -> at_expr (If (e1,e,e3)) ctx, down_rights 1
  | If3 (e1,e2,ctx) -> at_expr (If (e1,e2,e)) ctx, down_rights 2
  | OrX (ll_rr,ctx) -> at_expr (Or (list_of_ctx e ll_rr)) ctx,
                       down_rights (List.length (fst ll_rr))
  | AndX (ll_rr,ctx) -> at_expr (And (list_of_ctx e ll_rr)) ctx,
                        down_rights (List.length (fst ll_rr))
  | Not1 ctx -> at_expr (Not e) ctx, down_rights 0
  | CallX (func,ll_rr,ctx) -> at_expr (Call (func, list_of_ctx e ll_rr)) ctx,
                              down_rights (List.length (fst ll_rr))
  | Map1 (ctx,e2) -> at_expr (Map (e,e2)) ctx, down_rights 0
  | Map2 (e1,ctx) -> at_expr (Map (e1,e)) ctx, down_rights 1
  | Pred1 (ctx,e2) -> at_expr (Pred (e,e2)) ctx, down_rights 0
  | Pred2 (e1,ctx) -> at_expr (Pred (e1,e)) ctx, down_rights 1
  | Dot1 (ctx,e2) -> at_expr (Dot (e,e2)) ctx, down_rights 0
  | Dot2 (e1,ctx) -> at_expr (Dot (e1,e)) ctx, down_rights 1
  | ArrayLookup1 (ctx,e2) -> at_expr (ArrayLookup (e,e2)) ctx, down_rights 0
  | ArrayLookup2 (e1,ctx) -> at_expr (ArrayLookup (e1,e)) ctx, down_rights 1
  | ArrayUnboxing1 ctx -> at_expr (ArrayUnboxing e) ctx, down_rights 0
  | EObjectX1 (ll_rr,ctx,e2) -> at_expr (EObject (list_of_ctx (e,e2) ll_rr)) ctx,
                                DOWN :: path_of_list_ctx ll_rr (path_of_list_ctx ll_rr [])
  | EObjectX2 (ll_rr,e1,ctx) -> at_expr (EObject (list_of_ctx (e1,e) ll_rr)) ctx,
                                DOWN :: path_of_list_ctx ll_rr (path_of_list_ctx ll_rr [RIGHT])
  | Objectify1 ctx -> at_expr (Objectify e) ctx, down_rights 0
  | Arrayify1 ctx -> at_expr (Arrayify e) ctx, down_rights 0
  | Let1 (x,ctx,e2) -> at_expr (Let (x,e,e2)) ctx, down_rights 0
  | Let2 (x,e1,ctx) -> at_expr (Let (x,e1,e)) ctx, down_rights 1
  | DefFunc0 (name,args,ctx,e1,e2) -> at_expr (DefFunc (name,args,e,e1,e2)) ctx, down_rights 0
  | DefFunc1 (name,args,e0,ctx,e2) -> at_expr (DefFunc (name,args,e0,e,e2)) ctx, down_rights 1
  | DefFunc2 (name,args,e0,e1,ctx) -> at_expr (DefFunc (name,args,e0,e1,e)) ctx, down_rights 2
  | Return1 ctx -> focus_flower_up (Return e) ctx (* skipping focus at Return *)
  | For1 (x,ctx,opt,f) -> at_flower (For (x,e,opt,f)) ctx, down_rights 0
  | FLet1 (x,ctx,f) -> at_flower (FLet (x,e,f)) ctx, down_rights 0
  | Where1 (ctx,f) -> at_flower (Where (e,f)) ctx, down_rights 0
  | OrderBy1X (ll_rr,ctx,o,f) -> at_flower (OrderBy (list_of_ctx (e,o) ll_rr, f)) ctx,
                                 DOWN :: path_of_list_ctx ll_rr []
and focus_flower_up (f : flower) : flower_ctx -> focus * path = function
  | Flower1 ctx -> focus_expr_up (Flower f) ctx (* skipping focus at Flower *)
  | For2 (x,e,opt,ctx) -> at_flower (For (x,e,opt,f)) ctx, down_rights 1
  | FLet2 (x,e,ctx) -> at_flower (FLet (x,e,f)) ctx, down_rights 1
  | Count1 (x,ctx) -> at_flower (Count (x,f)) ctx, down_rights 0
  | Where2 (e,ctx) -> at_flower (Where (e,f)) ctx, down_rights 1
  | GroupBy1 (lx,ctx) -> at_flower (GroupBy (lx,f)) ctx, down_rights 0
  | Hide1 (lx,ctx) -> at_flower (Hide (lx,f)) ctx, down_rights 0
  | Slice1 (offset,limit,ctx) -> at_flower (Slice (offset,limit,f)) ctx, down_rights 0 
  | OrderBy2 (leo,ctx) -> at_flower (OrderBy (leo,f)) ctx,
                          DOWN :: path_of_list_ctx (leo,[]) []
  | FConcatX (ll_rr,ctx) -> at_flower (FConcat (list_of_ctx f ll_rr)) ctx,
                            down_rights (List.length (fst ll_rr))
  | FIf1 (ctx,f2,f3) -> at_flower (FIf (f,f2,f3)) ctx, down_rights 0
  | FIf2 (f1,ctx,f3) -> at_flower (FIf (f1,f,f3)) ctx, down_rights 1
  | FIf3 (f1,f2,ctx) -> at_flower (FIf (f1,f2,f)) ctx, down_rights 2

(*
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

(* DERIVED *)
let rec focus_left : focus -> focus option = function
  | AtExpr (e, ctx) -> focus_expr_left e ctx
  | AtFlower (f, ctx) -> focus_flower_left f ctx
and focus_expr_left (e : expr) : expr_ctx -> focus option = function
  | Root -> None
  | ConcatX ((e1::ll,rr),ctx) -> Some (AtExpr (e1, ConcatX ((ll,e::rr), ctx)))
  | ConcatX (([],_),_) -> None
  | Exists1 (x,ctx,e2) -> None
  | Exists2 (x,e1,ctx) -> Some (AtExpr (e1, Exists1 (x,ctx,e)))
  | ForAll1 (x,ctx,e2) -> None
  | ForAll2 (x,e1,ctx) -> Some (AtExpr (e1, ForAll1 (x,ctx,e)))
  | If1 (ctx,e2,e3) -> None
  | If2 (e1,ctx,e3) -> Some (AtExpr (e1, If1 (ctx,e,e3)))
  | If3 (e1,e2,ctx) -> Some (AtExpr (e2, If2 (e1,ctx,e)))
  | OrX ((e1::ll,rr),ctx) -> Some (AtExpr (e1, OrX ((ll,e::rr),ctx)))
  | OrX (([],_),_) -> None
  | AndX ((e1::ll,rr),ctx) -> Some (AtExpr (e1, AndX ((ll,e::rr),ctx)))
  | AndX (([],_),_) -> None
  | Not1 ctx -> None
  | CallX (func,(e1::ll,rr),ctx) -> Some (AtExpr (e1, CallX (func, (ll,e::rr), ctx)))
  | CallX (_, ([],_), _) -> None
  | Map1 (ctx,e2) -> None
  | Map2 (e1,ctx) -> Some (AtExpr (e1, Map1 (ctx,e)))
  | Pred1 (ctx,e2) -> None
  | Pred2 (e1,ctx) -> Some (AtExpr (e1, Pred1 (ctx,e)))
  | Dot1 (ctx,e2) -> None
  | Dot2 (e1,ctx) -> Some (AtExpr (e1, Dot1 (ctx,e)))
  | ArrayLookup1 (ctx,e2) -> None
  | ArrayLookup2 (e1,ctx) -> Some (AtExpr (e1, ArrayLookup1 (ctx,e)))
  | ArrayUnboxing1 ctx -> None
  | EObjectX1 (((e3,e4)::ll,rr),ctx,e2) -> Some (AtExpr (e4, EObjectX2 ((ll,(e,e2)::rr),e3,ctx)))
  | EObjectX1 (([],_),_,_) -> None
  | EObjectX2 (ll_rr,e1,ctx) -> Some (AtExpr (e1, EObjectX1 (ll_rr,ctx,e)))
  | Objectify1 ctx -> None
  | Arrayify1 ctx -> None
  | Let1 (x,ctx,e2) -> None
  | Let2 (x,e1,ctx) -> Some (AtExpr (e1, Let1 (x,ctx,e)))
  | DefFunc0 (name,args,ctx,e1,e2) -> None
  | DefFunc1 (name,args,e0,ctx,e2) -> Some (AtExpr (e0, DefFunc0 (name,args,ctx,e,e2)))
  | DefFunc2 (name,args,e0,e1,ctx) -> Some (AtExpr (e1, DefFunc1 (name,args,e0,ctx,e)))
  | Return1 ctx -> None
  | For1 (x,ctx,opt,f) -> None
  | FLet1 (x,ctx,f) -> None
  | Where1 (ctx,f) -> None
  | OrderBy1X (((e1,o1)::ll,rr),ctx,o,f) -> Some (AtExpr (e1, OrderBy1X ((ll,(e,o)::rr),ctx,o1,f)))
  | OrderBy1X (([],rr),ctx,o,f) -> None
and focus_flower_left (f : flower) : flower_ctx -> focus option = function
  | Flower1 ctx -> None
  | For2 (x,e,opt,ctx) -> Some (AtExpr (e, For1 (x,ctx,opt,f)))
  | FLet2 (x,e,ctx) -> Some (AtExpr (e, FLet1 (x,ctx,f)))
  | Count1 (x,ctx) -> None
  | Where2 (e,ctx) -> None
  | GroupBy1 (lx,ctx) -> None
  | Project1 (lx,ctx) -> None
  | Slice1 (offset,limit,ctx) -> None
  | OrderBy2 (leo,ctx) ->
     ( match List.rev leo with
       | [] -> None
       | (e,o)::ll -> Some (AtExpr (e, OrderBy1X ((ll,[]),ctx,o,f))) )
  | FConcatX ((f1::ll,rr),ctx) -> Some (AtFlower (f1, FConcatX ((ll,f::rr),ctx)))
  | FConcatX (([],_),_) -> None
  | FIf1 (ctx,f2,f3) -> None
  | FIf2 (f1,ctx,f3) -> Some (AtFlower (f1, FIf1 (ctx,f,f3)))
  | FIf3 (f1,f2,ctx) -> Some (AtFlower (f2, FIf2 (f1,ctx,f)))
 *)
                      
let rec focus_next_Empty (foc : focus) : focus option = Some foc
(*  match foc with
  | AtExpr (Empty, ctx) -> Some foc
  | _ ->
     match focus_right foc with
     | Some foc_right -> focus_next_Empty foc_right
     | None ->
	match focus_up foc with
	| Some (foc_up,_) -> focus_next_Empty foc_up
	| None -> None
 *)	  

(* conversions between focus and (expr,path) *)
			  
(* DERIVED *)
let rec focus_of_path_focus path : focus -> focus (* raises Invalid_path *) = function
  | AtExpr (e,ctx) -> focus_of_path_expr ctx (path,e)
  | AtFlower (f,ctx) -> focus_of_path_flower ctx (path,f)
and focus_of_path_expr (ctx : expr_ctx) : path * expr -> focus = function
  | path, Flower f -> focus_of_path_flower (flower1 ctx) (path,f) (* skipping Flower *)
  | [], e -> at_expr e ctx
  | DOWN::path, Concat le ->
     let path, (x, ll_rr) = list_focus_of_path_list path le in
     focus_of_path_expr (ConcatX (ll_rr,ctx)) (path,x)
  | DOWN::RIGHT::path, Exists (x,e1,e2) -> focus_of_path_expr (Exists2 (x,e1,ctx)) (path,e2)
  | DOWN::path, Exists (x,e1,e2) -> focus_of_path_expr (Exists1 (x,ctx,e2)) (path,e1)
  | DOWN::RIGHT::path, ForAll (x,e1,e2) -> focus_of_path_expr (ForAll2 (x,e1,ctx)) (path,e2)
  | DOWN::path, ForAll (x,e1,e2) -> focus_of_path_expr (ForAll1 (x,ctx,e2)) (path,e1)
  | DOWN::RIGHT::RIGHT::path, If (e1,e2,e3) -> focus_of_path_expr (If3 (e1,e2,ctx)) (path,e3)
  | DOWN::RIGHT::path, If (e1,e2,e3) -> focus_of_path_expr (If2 (e1,ctx,e3)) (path,e2)
  | DOWN::path, If (e1,e2,e3) -> focus_of_path_expr (If1 (ctx,e2,e3)) (path,e1)
  | DOWN::path, Or le ->
     let path, (x, ll_rr) = list_focus_of_path_list path le in
     focus_of_path_expr (OrX (ll_rr,ctx)) (path,x)
  | DOWN::path, And le ->
     let path, (x,ll_rr) = list_focus_of_path_list path le in
     focus_of_path_expr (AndX (ll_rr,ctx)) (path,x)
  | DOWN::path, Not e1 -> focus_of_path_expr (Not1 ctx) (path,e1)
  | DOWN::path, Call (f,le) ->
     let path, (x,ll_rr) = list_focus_of_path_list path le in
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
     let rec aux path ll p_rr =
       match path, p_rr with
       | RIGHT::RIGHT::path, (e1,e2)::rr -> aux path ((e1,e2)::ll) rr
       | RIGHT::path, (e1,e2)::rr -> focus_of_path_expr (EObjectX2 ((ll,rr),e1,ctx)) (path,e2)
       | path, (e1,e2)::rr -> focus_of_path_expr (EObjectX1 ((ll,rr),ctx,e2)) (path,e1)
       | path, [] -> raise (Invalid_path path) in
     aux path [] pairs
  | DOWN::path, Objectify e1 -> focus_of_path_expr (Objectify1 ctx) (path,e1)
  | DOWN::path, Arrayify e1 -> focus_of_path_expr (Arrayify1 ctx) (path,e1)
  | DOWN::RIGHT::path, Let (x,e1,e2) -> focus_of_path_expr (Let2 (x,e1,ctx)) (path,e2)
  | DOWN::path, Let (x,e1,e2) -> focus_of_path_expr (Let1 (x,ctx,e2)) (path,e1)
  | DOWN::RIGHT::RIGHT::path, DefFunc (f,lx,e0,e1,e2) -> focus_of_path_expr (DefFunc2 (f,lx,e0,e1,ctx)) (path,e2)
  | DOWN::RIGHT::path, DefFunc (f,lx,e0,e1,e2) -> focus_of_path_expr (DefFunc1 (f,lx,e0,ctx,e2)) (path,e1)
  | DOWN::path, DefFunc (f,lx,e0,e1,e2) -> focus_of_path_expr (DefFunc0 (f,lx,ctx,e1,e2)) (path,e0)
  | path, _ -> raise (Invalid_path path)
and focus_of_path_flower (ctx : flower_ctx) : path * flower -> focus = function
  | path, Return e1 -> focus_of_path_expr (return1 ctx) (path,e1) (* skipping Return *)
  | [], f -> at_flower f ctx
  | DOWN::RIGHT::path, For (x,e1,b,f) -> focus_of_path_flower (For2 (x,e1,b,ctx)) (path,f)
  | DOWN::path, For (x,e1,b,f) -> focus_of_path_expr (For1 (x,ctx,b,f)) (path,e1)
  | DOWN::RIGHT::path, FLet (x,e1,f) -> focus_of_path_flower (FLet2 (x,e1,ctx)) (path,f)
  | DOWN::path, FLet (x,e1,f) -> focus_of_path_expr (FLet1 (x,ctx,f)) (path,e1)
  | DOWN::path, Count (x,f) -> focus_of_path_flower (Count1 (x,ctx)) (path,f)
  | DOWN::RIGHT::path, Where (e1,f) -> focus_of_path_flower (Where2 (e1,ctx)) (path,f)
  | DOWN::path, Where (e1,f) -> focus_of_path_expr (Where1 (ctx,f)) (path,e1)
  | DOWN::path, GroupBy (lx,f) -> focus_of_path_flower (GroupBy1 (lx,ctx)) (path,f)
  | DOWN::path, Hide (lx,f) -> focus_of_path_flower (Hide1 (lx,ctx)) (path,f)
  | DOWN::path, Slice (o,l,f) -> focus_of_path_flower (Slice1 (o,l,ctx)) (path,f)
  | DOWN::path, OrderBy (leo,f) ->
     (try
        let path, ((e,o), ll_rr) = list_focus_of_path_list path leo in
        focus_of_path_expr (OrderBy1X (ll_rr,ctx,o,f)) (path,e)
      with
      | Invalid_path (RIGHT::path) ->
         focus_of_path_flower (OrderBy2 (leo,ctx)) (path,f))
  | DOWN::path, FConcat lf ->
     let path, (x,ll_rr) = list_focus_of_path_list path lf in
     focus_of_path_flower (FConcatX (ll_rr,ctx)) (path,x)
  | DOWN::RIGHT::RIGHT::path, FIf (e1,e2,e3) -> focus_of_path_flower (FIf3 (e1,e2,ctx)) (path,e3)
  | DOWN::RIGHT::path, FIf (e1,e2,e3) -> focus_of_path_flower (FIf2 (e1,ctx,e3)) (path,e2)
  | DOWN::path, FIf (e1,e2,e3) -> focus_of_path_flower (FIf1 (ctx,e2,e3)) (path,e1)
  | path, _ -> raise (Invalid_path path)
		
let expr_path_of_focus (foc : focus) : expr * path =
  let rec aux foc path =
    match focus_up foc, foc with
    | None, AtExpr (e,Root) -> e, path
    | None, _ -> assert false
    | Some (foc',path'), _ -> aux foc' (path'@path) in
  aux foc []

let focus_of_expr_path (e, path : expr * path) : focus =
  focus_of_path_expr Root (path,e)

let focus_down (foc : focus) : focus option =
  try Some (focus_of_path_focus [DOWN] foc)
  with Invalid_path _ -> None
    
let focus_right (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     try Some (focus_of_path_focus (path'@[RIGHT]) foc')
     with Invalid_path _ -> None

let focus_left (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     match List.rev path' with
     | [] -> None
     | DOWN::_ -> None
     | RIGHT::path'' ->
        try Some (focus_of_path_focus path'' foc')
        with Invalid_path _ -> None

let rec focus_succ (foc : focus) : focus option =
  match focus_down foc with
  | Some foc' -> Some foc'
  | None -> focus_succ_aux foc
and focus_succ_aux foc =
  match focus_right foc with
  | Some foc' -> Some foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> focus_succ_aux foc'
     | None -> None

let rec focus_pred (foc : focus) : focus option =
  match focus_left foc with
  | Some foc' -> focus_pred_down_rightmost foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> Some foc'
     | None -> None
and focus_pred_down_rightmost foc =
  match focus_down foc with
  | None -> Some foc
  | Some foc' -> focus_pred_rightmost foc'
and focus_pred_rightmost foc =
  match focus_right foc with
  | Some foc' -> focus_pred_rightmost foc'
  | None -> focus_pred_down_rightmost foc
                           
(* focus (de)serialization *)

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
  | InputFileTable of (string * string) input (* input file as table *)
  | InsertNull
  | InsertConcat1
  | InsertConcat2
  | InsertExists1 of var input
  | InsertExists2 of var input
  | InsertForAll1 of var input
  | InsertForAll2 of var input
  | InsertIf1 | InsertIf2 | InsertIf3
  | InsertOr
  | InsertAnd
  | InsertNot
  | InsertFunc of string * int * int (* name, arity, focus position in [1,arity] *)
  | InsertMap
  | InsertPred
  | InsertDot
  | InsertField of string
  | InsertArrayLookup of int option input
  | InsertArrayUnboxing
  | InsertVar of var
  | InsertContextItem
  | InsertContextEnv
  | InsertObject of string input
  | InsertObjectField of string input
  | InsertEnvObject
  | InsertArray
  | InsertObjectify
  | InsertArrayify
  | InsertDefFunc1 of (string * string list) input
  | InsertDefFunc2 of (string * string list) input
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
  | InsertHide of var list * var input
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
  | Exists (x,e1,e2) -> reaching_expr e1 @ InsertExists1 (new input x) :: reaching_expr e2 @ [FocusUp]
  | ForAll (x,e1,e2) -> reaching_expr e1 @ InsertForAll1 (new input x) :: reaching_expr e2 @ [FocusUp]
  | If (e1,e2,e3) -> reaching_expr e1 @ InsertIf1 :: reaching_expr e2 @ FocusRight :: reaching_expr e3 @ [FocusUp]
  | Or le -> reaching_list reaching_expr [InsertOr] le
  | And le -> reaching_list reaching_expr [InsertAnd] le
  | Not e -> reaching_expr e @ [InsertNot]
  | Call (func,le) -> InsertFunc (func, List.length le, 1) :: reaching_list reaching_expr [FocusRight] le
  | Map (e1,e2) -> reaching_expr e1 @ InsertMap :: Delete :: reaching_expr e2 @ [FocusUp]
  | Pred (e1,e2) -> reaching_expr e1 @ InsertPred :: Delete :: reaching_expr e2 @ [FocusUp]
  | Dot (e1,e2) -> reaching_expr e1 @ InsertDot :: reaching_expr e2 @ [FocusUp]
  | ArrayLookup (e1,e2) -> reaching_expr e1 @ InsertArrayLookup (new input None) :: reaching_expr e2 @ [FocusUp]
  | ArrayUnboxing e -> reaching_expr e @ [InsertArrayUnboxing]
  | Var x -> [InsertVar x]
  | ContextItem -> [InsertContextItem]
  | ContextEnv -> [InsertContextEnv]
  | EObject pairs -> InsertObject (new input "") :: reaching_list reaching_pair [InsertObjectField (new input "")] pairs
  | EnvObject -> [InsertEnvObject]
  | Objectify e -> reaching_expr e @ [InsertObjectify]
  | Arrayify e -> reaching_expr e @ [InsertArrayify]
  | Let (Var x,e1,e2) -> reaching_expr e1 @ InsertLetVar1 (new input x) :: reaching_expr e2 @ [FocusUp]
  | Let (Fields,e1,e2) -> reaching_expr e1 @ InsertLetFields1 :: reaching_expr e2 @ [FocusUp]
  | DefFunc (name,args,e0,e1,e2) ->
     InsertDefFunc1 (new input (name,args)) :: Delete :: reaching_expr e0 @ FocusRight :: reaching_expr e1 @ FocusRight :: reaching_expr e2 @ [FocusUp]
and reaching_flower : flower -> transf list = function
  | Return e -> reaching_expr e @ [FocusUp]
  | For (Var x,e,opt,f) -> reaching_expr e @ InsertForVar1 (new input x, new input opt) :: reaching_flower f @ [FocusUp]
  | For (Fields,e,opt,f) -> reaching_expr e @ InsertForFields1 (new input opt) :: reaching_flower f @ [FocusUp]
  | FLet (Var x,e,f) -> reaching_expr e @ InsertLetVar1 (new input x) :: reaching_flower f @ [FocusUp]
  | FLet (Fields,e,f) -> reaching_expr e @ InsertLetFields1 :: reaching_flower f @ [FocusUp]
  | Count (x,f) -> InsertCount1 (new input x) :: reaching_flower f @ [FocusUp]
  | Where (e,f) -> reaching_expr e @ InsertWhere1 :: reaching_flower f @ [FocusUp]
  | GroupBy (lx,f) -> List.map (fun x -> InsertGroupBy (lx, new input x)) lx @ reaching_flower f @ [FocusUp]
  | Hide (lx,f) -> List.map (fun x -> InsertHide ([], new input x)) lx @ reaching_flower f @ [FocusUp]
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
  | `Intlit s -> [InputString (new input s)] (* bof *)
  | `Float f -> [InputFloat (new input f)]
  | `String s -> [InputString (new input s)]
  | `Null -> [InsertNull]
  | `Assoc pairs -> InsertObject (new input "") :: reaching_list reaching_pair [InsertObjectField (new input "")] (List.map (fun (k,i) -> S k, Item i) pairs)
  | `List li -> InsertArray :: reaching_list reaching_item [InsertConcat1] li
  | `Tuple li -> reaching_item (`List li) (* Yojson encoding *)
  | `Variant (c, None) -> reaching_item (`String c) (* Yojson encoding *)
  | `Variant (c, Some i) -> reaching_item (`List [`String c; i]) (* Yojson encoding *)
and reaching_pair (e1, e2: expr * expr) : transf list =
  reaching_expr e1 @ FocusRight :: reaching_expr e2


let rec delete (foc : focus) : focus option =
  match foc with
  | AtExpr (Empty,ctx) -> delete_ctx_expr ctx
     (* match focus_up foc with
       | None -> None
       | Some (foc_up,_) -> delete foc_up *)
  (*  | AtExpr (_, ArrayUnboxing1 ctx) -> Some (AtExpr (Empty,ctx)) *)
  | AtExpr (_,ctx) -> Some (at_expr Empty ctx)
  | AtFlower (Return Empty, ctx) -> delete_ctx_flower ctx
     (* match focus_up foc with
       | None -> None
       e| Some (foc_up,_) -> delete foc_up *)
  | AtFlower (f, ctx) -> Some (at_expr Empty (return1 ctx))
(*Some (AtFlower (delete_flower f, ctx))*)
and delete_ctx_expr : expr_ctx -> focus option = function
  | Root -> None
  | ConcatX (ll_rr,ctx) ->
     let e = concat (Focus.list_of_ctx_none ll_rr) in
     Some (at_expr e ctx)
  | Exists1 (x,ctx,e2) -> Some (at_expr e2 ctx)
  | Exists2 (x,e1,ctx) -> Some (at_expr e1 ctx)
  | ForAll1 (x,ctx,e2) -> Some (at_expr e2 ctx)
  | ForAll2 (x,e1,ctx) -> Some (at_expr e1 ctx)
  | If1 (ctx,e2,e3) -> Some (at_expr e3 ctx)
  | If2 (e1,ctx,e3) -> Some (at_expr e3 ctx)
  | If3 (e1,e2,ctx) -> Some (at_expr e2 ctx)
  | OrX (ll_rr,ctx) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | [e] -> e
       | le -> Or le in
     Some (at_expr e ctx)
  | AndX (ll_rr,ctx) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | [e] -> e
       | le -> And le in
     Some (at_expr e ctx)
  | Not1 ctx -> Some (at_expr Empty ctx)
  | CallX (func,ll_rr,ctx) -> Some (at_expr Empty ctx)
  | Map1 (ctx,e2) -> Some (at_expr Empty ctx)
  | Map2 (e1,ctx) -> Some (at_expr e1 ctx)
  | Pred1 (ctx,e2) -> Some (at_expr Empty ctx)
  | Pred2 (e1,ctx) -> Some (at_expr e1 ctx)
  | Dot1 (ctx,e2) -> Some (at_expr Empty ctx)
  | Dot2 (e1,ctx) -> Some (at_expr e1 ctx)
  | ArrayLookup1 (ctx,e2) -> Some (at_expr Empty ctx)
  | ArrayLookup2 (e1,ctx) -> Some (at_expr e1 ctx)
  | ArrayUnboxing1 ctx -> Some (at_expr Empty ctx)
  | EObjectX1 (ll_rr,ctx,e2) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | pairs -> EObject pairs in
     Some (at_expr e ctx)
  | EObjectX2 (ll_rr,e1,ctx) ->
     let e =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> Empty
       | pairs -> EObject pairs in
     Some (at_expr e ctx)
  | Objectify1 ctx -> Some (at_expr Empty ctx)
  | Arrayify1 ctx -> Some (at_expr Empty ctx)
  | Let1 (br,ctx,e2) -> Some (at_expr e2 ctx)
  | Let2 (br,e1,ctx) -> Some (at_expr e1 ctx)
  | DefFunc0 (f,lx,ctx,e1,e2) -> Some (at_expr e2 ctx)
  | DefFunc1 (f,lx,e0,ctx,e2) -> Some (at_expr e2 ctx)
  | DefFunc2 (f,lx,e0,e1,ctx) -> Some (at_expr e1 ctx)
  | Return1 ctx -> delete_ctx_flower ctx (* skipping Return *)
  | For1 (x,ctx,opt,f) -> Some (at_flower f ctx)
  | FLet1 (br,ctx,f) -> Some (at_flower f ctx)
  | Where1 (ctx,f) -> Some (at_flower f ctx)
  | OrderBy1X (ll_rr,ctx,o,f1) ->
     let f =
       match Focus.list_of_ctx_none ll_rr with
       | [] -> f1
       | leo -> OrderBy (leo,f1) in
     Some (at_flower f ctx)
and delete_ctx_flower : flower_ctx -> focus option = function
  | Flower1 ctx -> delete_ctx_expr ctx (* skipping Flower *)
  | For2 (x,e,opt,ctx) -> Some (at_expr e (return1 ctx))
  | FLet2 (br,e,ctx) -> Some (at_expr e (return1 ctx))
  | Count1 (x,ctx) -> Some (at_expr Empty (return1 ctx))
  | Where2 (e,ctx) -> Some (at_expr e (return1 ctx))
  | GroupBy1 (lx,ctx) -> Some (at_expr Empty (return1 ctx))
  | Hide1 (lx,ctx) -> Some (at_expr Empty (return1 ctx))
  | Slice1 (o,l,ctx) -> Some (at_expr Empty (return1 ctx))
  | OrderBy2 (leo,ctx) -> Some (at_expr Empty (return1 ctx))
  | FConcatX (ll_rr,ctx) ->
     let f = fconcat (Focus.list_of_ctx_none ll_rr) in
     Some (at_flower f ctx)
  | FIf1 (ctx,f2,f3) -> Some (at_flower f3 ctx)
  | FIf2 (f1,ctx,f3) -> Some (at_flower f3 ctx)
  | FIf3 (f1,f2,ctx) -> Some (at_flower f2 ctx)

let rec delete_constr (foc : focus) : focus option =
  match foc with
  | AtExpr (e,ctx) ->
     (match delete_constr_expr e with
      | None -> None
      | Some (Flower f') -> Some (at_flower f' (flower1 ctx))
      | Some e' -> Some (at_expr e' ctx))
  | AtFlower (f,ctx) ->
     (match delete_constr_flower f with
      | None -> None
      | Some (Return e') -> Some (at_expr e' (return1 ctx))
      | Some f' -> Some (at_flower f' ctx))
and delete_constr_expr : expr -> expr option = function
  | S _ | Item _ | FileString _ -> Some Empty
  | Empty -> None
  | Concat _ -> None
  | Flower f ->
     (match delete_constr_flower f with
      | None -> None
      | Some (Return e') -> Some e'
      | Some f' -> Some (flower f'))
  | Exists (x,e1,e2) -> Some (Concat [e1; e2])
  | ForAll (x,e1,e2) -> Some (Concat [e1; e2])
  | If (e1,e2,e3) -> Some (Concat [e1; e2; e3])
  | Or le -> Some (concat le)
  | And le -> Some (concat le)
  | Not e1 -> Some e1
  | Call (f,le) -> Some (concat le)
  | Map (e1,e2) -> Some e1
  | Pred (e1,e2) -> Some e1
  | Dot (e1,e2) -> Some e1
  | ArrayLookup (e1,e2) -> Some e1
  | ArrayUnboxing e1 -> Some e1
  | Var _ -> Some Empty
  | ContextItem | ContextEnv -> Some Empty
  | EObject lkv -> Some (concat (List.map snd lkv))
  | EnvObject -> Some Empty
  | Objectify e1 -> Some e1
  | Arrayify e1 -> Some e1
  | Let (br,e1,e2) -> Some (Concat [e1; e2])
  | DefFunc (f,args,input,body,e) -> Some e
and delete_constr_flower : flower -> flower option = function
  | Return e ->
     (match delete_constr_expr e with
      | None -> None
      | Some (Flower f') -> Some f'
      | Some e' -> Some (return e'))
  | For (br,e,opt,f1) -> Some (return (Concat [e; flower f1]))
  | FLet (br,e,f1) -> Some (return (Concat [e; flower f1]))
  | Count (x,f1) -> Some f1
  | Where (e,f1) -> Some (return (Concat [e; flower f1]))
  | GroupBy (lx,f1) -> Some f1
  | Hide (lx,f1) -> Some f1
  | Slice (offset,limit,f1) -> Some f1
  | OrderBy (leo,f1) -> Some (return (Concat (List.map fst leo @ [flower f1])))
  | FConcat _ -> None
  | FIf (f1,f2,f3) -> Some (FConcat [f1; f2; f3])
                      
			     
let rec list_switch x = function
  | [] -> [x]
  | x1::lx1 ->
     if x1 = x
     then lx1
     else x1::list_switch x lx1

let init_func_input args =
  let e = EObject (args |> List.map (fun arg -> (S arg, Var arg))) in
  let f =
    List.fold_right
      (fun arg f -> For (Var arg, Item `Null, false, f))
      args (return e) in
  flower f

let rec extend_func_input arg = function
  | EObject l ->
     EObject (l @ [S arg, Var arg])
  | Flower f -> flower (extend_func_input_flower arg f)
  | e -> e
and extend_func_input_flower arg = function
  | For (Var x, e, opt, f) ->
     For (Var x, e, opt, extend_func_input_flower arg f)
  | Return e ->
     For (Var arg, Item `Null, false, return (extend_func_input arg e))
  | f -> f
  
    
let rec apply_transf (transf : transf) (foc : focus) : focus option =
  match transf with
  | FocusUp -> Option.map fst (focus_up foc)
  | FocusRight -> focus_right foc
  | Delete -> delete foc
  | _ ->
     let e, ctx_e =
       match foc with
       | AtExpr (e,ctx) -> e, ctx
       | AtFlower (f,ctx) -> flower f, return1 ctx in
     match apply_transf_expr (transf,e,ctx_e) with
     | None -> None
     | Some (e',ctx') -> Some (at_expr e' ctx')
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
  | InputFileTable in_file, _, ctx ->
     let filename, contents = in_file#get in
     if filename = "" || Filename.check_suffix filename ".csv" then
       Some (EnvObject, return1 (For2 (Fields, Call ("parseCSV", [FileString (filename, contents)]), false, flower1 (CallX ("printCSV", ([], []), ctx)))))
     else None

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

  | InsertExists1 in_x, Empty, ctx -> Some (Empty, Exists1 (in_x#get,ctx,Empty))
  | InsertExists1 in_x, e, ctx -> Some (Empty, Exists2 (in_x#get,e,ctx))
  | InsertExists2 in_x, Empty, ctx -> None
  | InsertExists2 in_x, e, ctx -> Some (Empty, Exists1 (in_x#get,ctx,e))
  | InsertForAll1 in_x, Empty, ctx -> Some (Empty, ForAll1 (in_x#get,ctx,Empty))
  | InsertForAll1 in_x, e, ctx -> Some (Empty, ForAll2 (in_x#get,e,ctx))
  | InsertForAll2 in_x, Empty, ctx -> None
  | InsertForAll2 in_x, e, ctx -> Some (Empty, ForAll1 (in_x#get,ctx,e))

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

  | InsertFunc (func,arity,pos), e, ctx ->
     if arity = 0 then
       Some (Call (func, []), ctx)
     else if e=Empty then (* focus on pos-th arg *)
       Some (e, CallX (func,
                       (make_list (pos-1) Empty,
                        make_list (arity-pos) Empty),
                       ctx))
     else if arity = 1 then (* focus on function call *)
       Some (Call (func, [e]), ctx)
     else if pos = 1 then (* focus on 2nd arg *)
       Some (Empty, CallX (func, ([e], make_list (arity-2) Empty), ctx))
     else (* focus on 1st arg, keeping [e] as pos-th arg *)
       Some (Empty, CallX (func,
                           ([],
                            make_list (pos-2) Empty @ e :: make_list (arity-pos) Empty),
                           ctx))
    
  | InsertMap, e, ctx -> Some (Var var_context, Map2 (e, ctx))
  | InsertPred, e, ctx -> Some (Var var_context, Pred2 (e, ctx))
  | InsertDot, e, ctx -> Some (Empty, Dot2 (e, ctx))
  | InsertField k, e, Dot1 (ctx,_) -> Some (Dot (e, S k), ctx)
  | InsertField k, e, ctx -> Some (Dot (e, S k), ctx)
  | InsertArrayLookup in_i_opt, e, ctx ->
     (match in_i_opt#get with
      | None -> Some (Empty, ArrayLookup2 (e, ctx))
      | Some i -> Some (ArrayLookup (e, Item (`Int i)), ctx))
  | InsertArrayUnboxing, e, ctx -> Some (ArrayUnboxing e, ctx)

  | InsertVar x, Empty, ctx -> Some (Var x, ctx)
  | InsertVar _, _, _ -> None
  | InsertContextItem, Empty, ctx -> Some (ContextItem, ctx)
  | InsertContextItem, _, _ -> None
  | InsertContextEnv, Empty, ctx -> Some (ContextEnv, ctx)
  | InsertContextEnv, _, _ -> None

  | InsertObject in_field, e, ctx ->
     let field = in_field#get in
     if field = ""
     then Some (Empty, EObjectX1 (([],[]), ctx, e))
     else Some (e, EObjectX2 (([],[]), S field, ctx))
  | InsertObjectField in_field, e1, EObjectX1 ((ll,rr), ctx, e2) ->
     let field = in_field#get in
     if field = ""
     then Some (Empty, EObjectX1 (((e1,e2)::ll,rr), ctx, Empty))
     else Some (Empty, EObjectX2 (((e1,e2)::ll,rr), S field, ctx))
  | InsertObjectField in_field, e2, EObjectX2 ((ll,rr), e1, ctx) ->
     let field = in_field#get in
     if field = ""
     then Some (Empty, EObjectX1 (((e1,e2)::ll,rr), ctx, Empty))
     else Some (Empty, EObjectX2 (((e1,e2)::ll,rr), S field, ctx))
  | InsertObjectField _, _, _ -> None
  | InsertEnvObject, _, ctx -> Some (EnvObject, ctx)
  | InsertArray, Empty, ctx -> Some (Empty, Arrayify1 ctx)
  | InsertArray, _, _ -> None
  | InsertObjectify, e, ctx -> Some (Objectify e, ctx)
  | InsertArrayify, e, ctx -> Some (Arrayify e, ctx)

  | InsertDefFunc1 in_sig, e, ctx ->
     let name, args = in_sig#get in
     let e0 = init_func_input args in
     Some (e0, DefFunc0 (name, args, ctx, e, Empty))
  | InsertDefFunc2 in_sig, e, ctx ->
     let name, args = in_sig#get in
     let e0 = init_func_input args in
     Some (e0, DefFunc0 (name, args, ctx, Empty, e))
  | InsertArg in_x, DefFunc (name,args,e0,e1,e2), ctx ->
     let arg = in_x#get in
     let args = args @ [arg] in
     let e0 = extend_func_input arg e0 in
     Some (DefFunc (name, args, e0, e1, e2), ctx)
  | InsertArg in_x, e0, DefFunc0 (name,args,ctx,e1,e2) ->
     let arg = in_x#get in
     let args = args @ [arg] in
     let e0 = extend_func_input arg e0 in
     Some (e0, DefFunc0 (name, args, ctx, e1, e2))
  | InsertArg _, _, _ -> None

  | InsertForVar1 (in_x,in_opt), e, ctx -> Some (Empty, return1 (For2 (Var in_x#get, e, in_opt#get, flower1 ctx)))
  | InsertForVar2 (in_x,in_opt), e, ctx -> Some (Empty, For1 (Var in_x#get, flower1 ctx, in_opt#get, return e))
  | InsertForFields1 (in_opt), e, ctx -> Some (Empty, return1 (For2 (Fields, e, in_opt#get, flower1 ctx)))

  | InsertLetVar1 in_x, e, Return1 ctx -> Some (Empty, return1 (FLet2 (Var in_x#get, e, ctx)))
  | InsertLetVar2 in_x, e, Return1 ctx -> Some (Empty, FLet1 (Var in_x#get, ctx, return e))
  | InsertLetFields1, e, Return1 ctx -> Some (Empty, return1 (FLet2 (Fields, e, ctx)))
  | InsertLetVar1 in_x, e, ctx -> Some (Empty, Let2 (Var in_x#get, e, ctx))
  | InsertLetVar2 in_x, e, ctx -> Some (Empty, Let1 (Var in_x#get, ctx, e))
  | InsertLetFields1, e, ctx -> Some (Empty, Let2 (Fields, e, ctx))

  | InsertCount1 in_x, e, Return1 ctx -> Some (e, return1 (Count1 (in_x#get, ctx)))
  | InsertCount1 _, _, _ -> None
				     
  (* transformations below should only be suggested in for context *)

  | InsertWhere1, e, ctx -> Some (Empty, return1 (Where2 (e, flower1 ctx)))
  | InsertWhere2, e, ctx -> Some (Empty, Where1 (flower1 ctx, return e))

  | InsertGroupBy (_, in_x), e, Return1 (GroupBy1 (lx,ctx)) -> Some (e, return1 (GroupBy1 (list_switch in_x#get lx,ctx)))					    
  | InsertGroupBy (_, in_x), Flower (GroupBy (lx,f)), ctx -> Some (flower (GroupBy (list_switch in_x#get lx,f)), ctx)						 
  | InsertGroupBy (_, in_x), e, ctx -> Some (e, return1 (GroupBy1 ([in_x#get], flower1 ctx)))

  | InsertHide (_, in_x), e, Return1 (Hide1 (lx,ctx)) ->
     (match list_switch in_x#get lx with
      | [] -> Some (e, return1 ctx)
      | new_lx -> Some (e, return1 (Hide1 (new_lx, ctx))))
  | InsertHide (_, in_x), Flower (Hide (lx,f)), ctx ->
     (match list_switch in_x#get lx with
      | [] -> Some (flower f, ctx)
      | new_lx -> Some (flower (Hide (new_lx, f)), ctx))
  | InsertHide (_, in_x), e, ctx ->
     Some (e, return1 (Hide1 ([in_x#get], flower1 ctx)))
  (*     Some (flower (Hide ([in_x#get], return e)), ctx) *)
(*  | InsertProject in_lx, e, return1 (Project1 (_,ctx)) -> Some (e, return1 (Project1 (in_lx#get, ctx)))
  | InsertProject in_lx, Flower (Project (_,f)), ctx -> Some (flower (Project (in_lx#get, f)), ctx)						 
  | InsertProject in_lx, e, ctx -> Some (flower (Project (in_lx#get, return e)), ctx) *)

  | InsertSlice (in_offset,in_limit), e, ctx ->
     let offset = in_offset#get in
     let limit = match in_limit#get with 0 -> None | l -> Some l in
     Some (e, return1 (Slice1 (offset, limit, flower1 ctx)))

  | InsertOrderBy1 in_o, e, Return1 (OrderBy2 (leo, ctx)) -> Some (Empty, return1 (OrderBy2 (leo@[e, order_of_string in_o#get], ctx)))
  | InsertOrderBy1 in_o, e, ctx -> Some (Empty, return1 (OrderBy2 ([e, order_of_string in_o#get], flower1 ctx)))
  | InsertOrderBy2 in_o, e, ctx -> Some (Empty, OrderBy1X (([],[]), flower1 ctx, order_of_string in_o#get, return e))


let prefill_transf (focus : focus) (t : transf) : transf =
  let e =
    match focus with
    | AtExpr (e,ctx) -> e
    | AtFlower (f,ctx) -> flower f
  in
  match t with
  | InputInt _ ->
     (match e with
      | Item (`Int i) -> InputInt (new input i)
      | Item (`Float f) -> InputInt (new input (int_of_float f))
      | _ -> t)
  | InputRange (_, _) ->
     (match e with
      | Call ("range", [Item (`Int a); Item (`Int b)]) -> InputRange (new input a, new input b)
      | Item (`Int i) -> InputRange (new input i, new input i)
      | _ -> t)
  | InputFloat _ ->
     (match e with
      | Item (`Float f) -> InputFloat (new input f)
      | Item (`Int i) -> InputFloat (new input (float_of_int i))
      | _ -> t)
  | InputString _ ->
     (match e with
      | S s -> InputString (new input s)
      | Item (`String s) -> InputString (new input s)
      | _ -> t)
  | _ -> t
