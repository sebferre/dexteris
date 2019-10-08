
open Focus
open Jsoniq

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
  | CallX of func * expr list_ctx * expr_ctx
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
  | DefVar1 of var * expr_ctx * expr
  | DefVar2 of var * expr * expr_ctx
  | DefFunc1 of string * var list * expr_ctx * expr
  | DefFunc2 of string * var list * expr * expr_ctx
  | Return1 of flower_ctx
  | For1 of var * flower_ctx * bool * flower
  | ForObject1 of flower_ctx * bool * flower
  | Let1 of var * flower_ctx * flower
  | Where1 of flower_ctx * flower
  | OrderBy1X of (expr * order) list_ctx * flower_ctx * order * flower

 (* DERIVED *)
and flower_ctx =
  | Flower1 of expr_ctx
  | For2 of var * expr * bool * flower_ctx
  | ForObject2 of expr * bool * flower_ctx
  | Let2 of var * expr * flower_ctx
  | Where2 of expr * flower_ctx
  | GroupBy1 of var list * flower_ctx
  | OrderBy2 of (expr * order) list * flower_ctx
  | FConcatX of flower list_ctx * flower_ctx
  | FIf1 of flower_ctx * flower * flower
  | FIf2 of flower * flower_ctx * flower
  | FIf3 of flower * flower * flower_ctx

(* DERIVED *)
type focus =
  | AtExpr of expr * expr_ctx
  | AtFlower of flower * flower_ctx

(* DERIVED *)
let rec focus_up : focus -> focus option = function
  | AtExpr (e, Root) -> None
  | AtExpr (e, ctx) -> Some (focus_expr_up e ctx)
  | AtFlower (f, ctx) -> Some (focus_flower_up f ctx)
and focus_expr_up (e : expr) : expr_ctx -> focus = function
  | Root -> assert false
  | ConcatX (ll_rr,ctx) -> AtExpr (Concat (list_of_ctx e ll_rr), ctx)
  | Exists1 (x,ctx,e2) -> AtExpr (Exists (x,e,e2), ctx)
  | Exists2 (x,e1,ctx) -> AtExpr (Exists (x,e1,e), ctx)
  | ForAll1 (x,ctx,e2) -> AtExpr (ForAll (x,e,e2), ctx)
  | ForAll2 (x,e1,ctx) -> AtExpr (ForAll (x,e1,e), ctx)
  | If1 (ctx,e2,e3) -> AtExpr (If (e,e2,e3), ctx)
  | If2 (e1,ctx,e3) -> AtExpr (If (e1,e,e3), ctx)
  | If3 (e1,e2,ctx) -> AtExpr (If (e1,e2,e), ctx)
  | OrX (ll_rr,ctx) -> AtExpr (Or (list_of_ctx e ll_rr), ctx)
  | AndX (ll_rr,ctx) -> AtExpr (And (list_of_ctx e ll_rr), ctx)
  | Not1 ctx -> AtExpr (Not e, ctx)
  | CallX (func,ll_rr,ctx) -> AtExpr (Call (func, list_of_ctx e ll_rr), ctx)
  | Map1 (ctx,e2) -> AtExpr (Map (e,e2), ctx)
  | Map2 (e1,ctx) -> AtExpr (Map (e1,e), ctx)
  | Pred1 (ctx,e2) -> AtExpr (Pred (e,e2), ctx)
  | Pred2 (e1,ctx) -> AtExpr (Pred (e1,e), ctx)
  | Dot1 (ctx,e2) -> AtExpr (Dot (e,e2), ctx)
  | Dot2 (e1,ctx) -> AtExpr (Dot (e1,e), ctx)
  | ArrayLookup1 (ctx,e2) -> AtExpr (ArrayLookup (e,e2), ctx)
  | ArrayLookup2 (e1,ctx) -> AtExpr (ArrayLookup (e1,e), ctx)
  | ArrayUnboxing1 ctx -> AtExpr (ArrayUnboxing e, ctx)
  | EObjectX1 (ll_rr,ctx,e2) -> AtExpr (EObject (list_of_ctx (e,e2) ll_rr), ctx)
  | EObjectX2 (ll_rr,e1,ctx) -> AtExpr (EObject (list_of_ctx (e1,e) ll_rr), ctx)
  | Objectify1 ctx -> AtExpr (Objectify e, ctx)
  | Arrayify1 ctx -> AtExpr (Arrayify e, ctx)
  | DefVar1 (x,ctx,e2) -> AtExpr (DefVar (x,e,e2), ctx)
  | DefVar2 (x,e1,ctx) -> AtExpr (DefVar (x,e1,e), ctx)
  | DefFunc1 (name,args,ctx,e2) -> AtExpr (DefFunc (name,args,e,e2), ctx)
  | DefFunc2 (name,args,e1,ctx) -> AtExpr (DefFunc (name,args,e1,e), ctx)
  | Return1 ctx -> AtFlower (Return e, ctx)
  | For1 (x,ctx,opt,f) -> AtFlower (For (x,e,opt,f), ctx)
  | ForObject1 (ctx,opt,f) -> AtFlower (ForObject (e,opt,f), ctx)
  | Let1 (x,ctx,f) -> AtFlower (Let (x,e,f), ctx)
  | Where1 (ctx,f) -> AtFlower (Where (e,f), ctx)
  | OrderBy1X (ll_rr,ctx,o,f) -> AtFlower (OrderBy (list_of_ctx (e,o) ll_rr, f), ctx)
and focus_flower_up (f : flower) : flower_ctx -> focus = function
  | Flower1 ctx -> AtExpr (Flower f, ctx)
  | For2 (x,e,opt,ctx) -> AtFlower (For (x,e,opt,f), ctx)
  | ForObject2 (e,opt,ctx) -> AtFlower (ForObject (e,opt,f), ctx)
  | Let2 (x,e,ctx) -> AtFlower (Let (x,e,f), ctx)
  | Where2 (e,ctx) -> AtFlower (Where (e,f), ctx)
  | GroupBy1 (lx,ctx) -> AtFlower (GroupBy (lx,f), ctx)
  | OrderBy2 (leo,ctx) -> AtFlower (OrderBy (leo,f), ctx)
  | FConcatX (ll_rr,ctx) -> AtFlower (FConcat (list_of_ctx f ll_rr), ctx)
  | FIf1 (ctx,f2,f3) -> AtFlower (FIf (f,f2,f3), ctx)
  | FIf2 (f1,ctx,f3) -> AtFlower (FIf (f1,f,f3), ctx)
  | FIf3 (f1,f2,ctx) -> AtFlower (FIf (f1,f2,f), ctx)

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
  | DefVar1 (x,ctx,e2) -> Some (AtExpr (e2, DefVar2 (x,e,ctx)))
  | DefVar2 (x,e1,ctx) -> None
  | DefFunc1 (name,args,ctx,e2) -> Some (AtExpr (e2, DefFunc2 (name,args,e,ctx)))
  | DefFunc2 (name,args,e1,ctx) -> None
  | Return1 ctx -> None
  | For1 (x,ctx,opt,f) -> Some (AtFlower (f, For2 (x,e,opt,ctx)))
  | ForObject1 (ctx,opt,f) -> Some (AtFlower (f, ForObject2 (e,opt,ctx)))
  | Let1 (x,ctx,f) -> Some (AtFlower (f, Let2 (x,e,ctx)))
  | Where1 (ctx,f) -> Some (AtFlower (f, Where2 (e,ctx)))
  | OrderBy1X ((ll,(e1,o1)::rr),ctx,o,f) -> Some (AtExpr (e1, OrderBy1X (((e,o)::ll,rr),ctx,o1,f)))
  | OrderBy1X ((ll,[]),ctx,o,f) -> Some (AtFlower (f, OrderBy2 (List.rev ((e,o)::ll), ctx)))
and focus_flower_right (f : flower) : flower_ctx -> focus option = function
  | Flower1 ctx -> None
  | For2 (x,e,opt,ctx) -> None
  | ForObject2 (e,opt,ctx) -> None
  | Let2 (x,e,ctx) -> None
  | Where2 (e,ctx) -> None
  | GroupBy1 (lx,ctx) -> None
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
     | Some foc -> focus_next_Empty foc
     | None ->
	match focus_up foc with
	| Some foc -> focus_next_Empty foc
	| None -> None
			  

let rec delete (foc : focus) : focus option =
  match foc with
  | AtExpr (Empty,ctx) ->
     ( match focus_up foc with
       | None -> None
       | Some foc_up -> delete foc_up )
  | AtExpr (_,ctx) -> Some (AtExpr (Empty, ctx))
  | AtFlower (Return Empty, ctx) ->
     ( match focus_up foc with
       | None -> None
       | Some foc_up -> delete foc_up )
  | AtFlower (f, ctx) -> Some (AtExpr (Empty, Return1 ctx))
			  
(* focus transformations and navigation paths *)
			   
let initial_focus = AtExpr (Empty, Root)


type transf =
  | FocusUp
  | FocusRight
  | Delete
  | InsertBool of bool
  | InputInt of int input
  | InputFloat of float input
  | InputString of string input
  | InsertNull
  | InsertConcat
  | InsertExists of var input
  | InsertForAll of var input
  | InsertIf1 | InsertIf2 | InsertIf3
  | InsertOr
  | InsertAnd
  | InsertNot
  | InsertFunc of func
  | InsertMap
  | InsertPred
  | InsertDot
  | InsertArrayLookup
  | InsertArrayUnboxing
  | InsertVar of var
  | InsertContextItem
  | InsertContextEnv
  | InsertObject
  | InsertArray
  | InsertObjectify
  | InsertArrayify
  | InsertDefVar of var input
  | InsertDefFunc of string input
  | InsertArg of var input
  | InsertFor of var input * bool input
  | InsertForObject of bool input
  | InsertLet of var input
  | InsertWhere
  | InsertGroupBy of var
  | InsertOrderBy of order

(* TODO: avoid implicit focus change in transf application, make it global *)
		       
let rec reaching_list reaching_elt sep l =
  match l with
  | [] -> []
  | [x] -> reaching_elt x @ [FocusUp]
  | x::r -> reaching_elt x @ sep @ reaching_list reaching_elt sep r

let rec reaching_expr : expr -> transf list = function
  | S s -> [InputString (new input s)]
  | Item i -> reaching_item i
  | Empty -> [] (* the default value *)
  | Data d -> reaching_data d
  | Concat le -> reaching_list reaching_expr [InsertConcat] le (* assuming |le| > 1 *)
  | Flower f -> reaching_flower f
  | Exists (x,e1,e2) -> reaching_expr e1 @ InsertExists (new input x) :: reaching_expr e2 @ [FocusUp]
  | ForAll (x,e1,e2) -> reaching_expr e1 @ InsertForAll (new input x) :: reaching_expr e2 @ [FocusUp]
  | If (e1,e2,e3) -> reaching_expr e1 @ InsertIf1 :: reaching_expr e2 @ FocusRight :: reaching_expr e3 @ [FocusUp]
  | Or le -> reaching_list reaching_expr [InsertOr] le
  | And le -> reaching_list reaching_expr [InsertAnd] le
  | Not e -> reaching_expr e @ [InsertNot]
  | Call (func,le) -> InsertFunc func :: reaching_list reaching_expr [FocusRight] le
  | Map (e1,e2) -> reaching_expr e1 @ InsertMap :: reaching_expr e2 @ [FocusUp]
  | Pred (e1,e2) -> reaching_expr e1 @ InsertPred :: reaching_expr e2 @ [FocusUp]
  | Dot (e1,e2) -> reaching_expr e1 @ InsertDot :: reaching_expr e2 @ [FocusUp]
  | ArrayLookup (e1,e2) -> reaching_expr e1 @ InsertArrayLookup :: reaching_expr e2 @ [FocusUp]
  | ArrayUnboxing e -> reaching_expr e @ [InsertArrayUnboxing]
  | Var x -> [InsertVar x]
  | ContextItem -> [InsertContextItem]
  | ContextEnv -> [InsertContextEnv]
  | EObject pairs -> InsertObject :: reaching_list reaching_pair [InsertConcat] pairs
  | Objectify e -> reaching_expr e @ [InsertObjectify]
  | Arrayify e -> reaching_expr e @ [InsertArrayify]
  | DefVar (x,e1,e2) -> reaching_expr e1 @ InsertDefVar (new input x) :: reaching_expr e2 @ [FocusUp]
  | DefFunc (name,args,e1,e2) -> InsertDefFunc (new input name) :: List.map (fun x -> InsertArg (new input x)) args @ reaching_expr e1 @ FocusRight :: reaching_expr e2 @ [FocusUp]
and reaching_flower : flower -> transf list = function
  | Return e -> reaching_expr e @ [FocusUp]
  | For (x,e,opt,f) -> reaching_expr e @ InsertFor (new input x, new input opt) :: reaching_flower f @ [FocusUp]
  | ForObject (e,opt,f) -> reaching_expr e @ InsertForObject (new input opt) :: reaching_flower f @ [FocusUp]
  | Let (x,e,f) -> reaching_expr e @ InsertLet (new input x) :: reaching_flower f @ [FocusUp]
  | Where (e,f) -> reaching_expr e @ InsertWhere :: reaching_flower f @ [FocusUp]
  | GroupBy (lx,f) -> List.map (fun x -> InsertGroupBy x) lx @ reaching_flower f @ [FocusUp]
  | OrderBy (leo,f) -> List.concat (List.map (fun (e,o) -> reaching_expr e @ [InsertOrderBy o]) leo) @ reaching_flower f @ [FocusUp]
  | FConcat lf -> reaching_list reaching_flower [InsertConcat] lf
  | FIf (f1,f2,f3) -> reaching_flower f1 @ InsertIf1 :: reaching_flower f2 @ FocusRight :: reaching_flower f3 @ [FocusUp]
and reaching_data (d : data) : transf list =
  reaching_list reaching_item [InsertConcat] (Seq.to_list d)
and reaching_item : item -> transf list = function
  | Bool b -> [InsertBool b]
  | Int n -> [InputInt (new input n)]
  | Float f -> [InputFloat (new input f)]
  | String s -> [InputString (new input s)]
  | Null -> [InsertNull]
  | Object pairs -> InsertObject :: reaching_list reaching_pair [InsertConcat] (List.map (fun (k,i) -> S k, Item i) pairs)
  | Array li -> InsertArray :: reaching_list reaching_item [InsertConcat] li
and reaching_pair (e1, e2: expr * expr) : transf list =
  reaching_expr e1 @ FocusRight :: reaching_expr e2


let ctx_flower_of_expr = function
  | Return1 ctx_f -> ctx_f
  | ctx_e -> Flower1 ctx_e
						 
let rec apply_transf (transf : transf) (foc : focus) : focus option =
  match transf with
  | FocusUp -> focus_up foc
  | FocusRight -> focus_right foc
  | Delete -> delete foc
  | _ ->
     match foc with
     | AtExpr (e,ctx) -> apply_transf_expr (transf, e, ctx)
     | AtFlower (f,ctx) -> apply_transf_flower (transf, f, ctx)
and apply_transf_expr = function
  | InsertBool b, _, ctx -> Some (AtExpr (Item (Bool b), ctx))
  | InputInt in_n, _, ctx -> Some (AtExpr (Item (Int in_n#get), ctx))
  | InputFloat in_f, _, ctx -> Some (AtExpr (Item (Float in_f#get), ctx))
  | InputString in_s, _, ctx -> Some (AtExpr (Item (String in_s#get), ctx))
  | InsertNull, _, ctx -> Some (AtExpr (Item Null, ctx))

  | InsertConcat, e, EObjectX1 ((ll,rr), ctx, e2) -> Some (AtExpr (Empty, EObjectX1 (((e,e2)::ll,rr), ctx, Empty)))
  | InsertConcat, e, EObjectX2 ((ll,rr), e1, ctx) -> Some (AtExpr (Empty, EObjectX1 (((e1,e)::ll,rr), ctx, Empty)))

  | InsertConcat, Empty, _ -> None
  | InsertConcat, e, ConcatX ((ll,rr), ctx) -> Some (AtExpr (Empty, ConcatX ((e::ll,rr), ctx)))
  | InsertConcat, Concat le, ctx -> Some (AtExpr (Empty, ConcatX ((List.rev le,[]), ctx)))
  | InsertConcat, e, ctx -> Some (AtExpr (Empty, ConcatX (([e],[]), ctx)))

  | InsertExists in_x, Empty, ctx -> Some (AtExpr (Empty, Exists1 (in_x#get,ctx,Empty)))
  | InsertExists in_x, e, ctx -> Some (AtExpr (Empty, Exists2 (in_x#get,e,ctx)))
  | InsertForAll in_x, Empty, ctx -> Some (AtExpr (Empty, ForAll1 (in_x#get,ctx,Empty)))
  | InsertForAll in_x, e, ctx -> Some (AtExpr (Empty, ForAll2 (in_x#get,e,ctx)))

  | InsertIf1, e, ctx -> Some (AtExpr (Empty, If2 (e,ctx,Empty)))
  | InsertIf2, e, ctx -> Some (AtExpr (Empty, If1 (ctx,e,Empty)))
  | InsertIf3, e, ctx -> Some (AtExpr (Empty, If1 (ctx,Empty,e)))

  | InsertOr, e, OrX ((ll,rr), ctx) -> Some (AtExpr (Empty, OrX ((e::ll,rr), ctx)))
  | InsertOr, Or le, ctx -> Some (AtExpr (Empty, OrX ((List.rev le,[]), ctx)))
  | InsertOr, e, ctx -> Some (AtExpr (Empty, OrX (([e],[]), ctx)))

  | InsertAnd, e, AndX ((ll,rr), ctx) -> Some (AtExpr (Empty, AndX ((e::ll,rr), ctx)))
  | InsertAnd, And le, ctx -> Some (AtExpr (Empty, AndX ((List.rev le,[]), ctx)))
  | InsertAnd, e, ctx -> Some (AtExpr (Empty, AndX (([e],[]), ctx)))

  | InsertNot, Not e, ctx -> Some (AtExpr (e,ctx))
  | InsertNot, e, Not1 ctx -> Some (AtExpr (e,ctx))
  | InsertNot, e, ctx -> Some (AtExpr (Not e, ctx))

  | InsertFunc func, e, ctx ->
     ( match arity_of_func func with
       | 0 -> Some (AtExpr (Call (func, []), ctx))
       | n -> Some (AtExpr (e, CallX (func, ([],make_list (n-1) Empty), ctx))) )

  | InsertMap, e, ctx -> Some (AtExpr (Empty, Map2 (e, ctx)))
  | InsertPred, e, ctx -> Some (AtExpr (Empty, Pred2 (e, ctx)))
  | InsertDot, e, ctx -> Some (AtExpr (Empty, Dot2 (e, ctx)))
  | InsertArrayLookup, e, ctx -> Some (AtExpr (Empty, ArrayLookup2 (e, ctx)))

  | InsertArrayUnboxing, e, ctx -> Some (AtExpr (ArrayUnboxing e, ctx))

  | InsertVar x, Empty, ctx -> Some (AtExpr (Var x, ctx))
  | InsertContextItem, Empty, ctx -> Some (AtExpr (ContextItem, ctx))
  | InsertContextEnv, Empty, ctx -> Some (AtExpr (ContextEnv, ctx))

  | InsertObject, Empty, ctx -> Some (AtExpr (Empty, EObjectX1 (([],[]), ctx, Empty)))
  | InsertArray, Empty, ctx -> Some (AtExpr (Empty, Arrayify1 ctx))
  | InsertObjectify, e, ctx -> Some (AtExpr (Objectify e, ctx))
  | InsertArrayify, e, ctx -> Some (AtExpr (Arrayify e, ctx))

  | InsertDefVar in_x, e, ctx -> Some (AtExpr (Empty, DefVar2 (in_x#get, e, ctx)))
  | InsertDefFunc in_name, Empty, ctx -> Some (AtExpr (Empty, DefFunc1 (in_name#get, [], ctx, Empty)))
  | InsertArg in_x, DefFunc (name,args,e1,e2), ctx -> Some (AtExpr (DefFunc (name, args@[in_x#get], e1, e2), ctx))
  | InsertArg in_x, e1, DefFunc1 (name,args,ctx,e2) -> Some (AtExpr (e1, DefFunc1 (name, args@[in_x#get], ctx, e2)))

  | InsertFor (in_x,in_opt), e, ctx -> Some (AtExpr (Empty, Return1 (For2 (in_x#get,e,in_opt#get, ctx_flower_of_expr ctx))))
  | InsertForObject in_opt, e, ctx -> Some (AtExpr (Empty, Return1 (ForObject2 (e,in_opt#get, ctx_flower_of_expr ctx))))
  | InsertLet in_x, e, ctx -> Some (AtExpr (Empty, Return1 (Let2 (in_x#get,e, ctx_flower_of_expr ctx))))

  | InsertWhere, e, Return1 ctx -> Some (AtExpr (Empty, Return1 (Where2 (e, ctx))))
  | InsertGroupBy x, e, Return1 (GroupBy1 (lx,ctx)) -> Some (AtExpr (e, Return1 (GroupBy1 (lx@[x],ctx))))
  | InsertGroupBy x, e, Return1 ctx -> Some (AtExpr (e, Return1 (GroupBy1 ([x],ctx))))
  | InsertOrderBy o, e, Return1 (OrderBy2 (leo, ctx)) -> Some (AtExpr (Empty, Return1 (OrderBy2 (leo@[e,o], ctx))))
  | InsertOrderBy o, e, Return1 ctx -> Some (AtExpr (Empty, Return1 (OrderBy2 ([e,o], ctx))))
    
  | transf, Flower f, ctx -> apply_transf_flower (transf, f, Flower1 ctx)
				
  | _ -> None
and apply_transf_flower = function
  | InsertFor (in_x,in_opt), f, ctx -> Some (AtExpr (Empty, For1 (in_x#get, ctx, in_opt#get, f)))
  | InsertForObject in_opt, f, ctx -> Some (AtExpr (Empty, ForObject1 (ctx, in_opt#get, f)))
  | InsertLet in_x, f, ctx -> Some (AtExpr (Empty, Let1 (in_x#get, ctx, f)))

  | _, _, Flower1 _ -> None
  | InsertWhere, f, ctx -> Some (AtExpr (Empty, Where1 (ctx, f)))
  | InsertGroupBy x, GroupBy (lx,f), ctx -> Some (AtFlower (GroupBy (lx@[x],f), ctx))
  | InsertGroupBy x, f, GroupBy1 (lx, ctx) -> Some (AtFlower (f, GroupBy1 (lx@[x], ctx)))
  | InsertGroupBy x, f, ctx -> Some (AtFlower (GroupBy ([x],f), ctx))
				
  | transf, Return e, ctx -> apply_transf_expr (transf, e, Return1 ctx)
  | _ -> None
