
open Jsoniq
open Jsoniq_focus

type typ = [`Bool | `Int | `Float | `String | `Object | `Array]

module TypSet = Set.Make(struct type t = typ let compare = Stdlib.compare end)

let list_all_typs = [`Bool; `Int; `Float; `String; `Object; `Array]
let all_typs : TypSet.t = TypSet.of_list list_all_typs
       
(*
type 'a sem = { expr : 'a;
		funcs : (string * var list) list; (* defined functions in scope *)
		env : var list; (* variables in scope *)
		typs : TypSet.t; (* allowed types at focus *)
	      }

let sem_bind_in x e1 sem =
  { sem with env = x::sem.env; expr = flower (For (x,e1,false, return sem.expr)) }
let sem_map f sem =
  { sem with expr = f sem.expr }
let sem_var x f sem =
  { sem with env = x::sem.env; expr = f sem.expr }
let sem_func name args f sem =
  { sem with funcs = (name,args)::sem.funcs; expr = f sem.expr }
let sem_to_flower sem =
  { sem with expr = return sem.expr }
let sem_to_expr sem =
  { sem with expr = flower sem.expr }
 *)
		  
let field_focus = "@focus"

class annot =
  object
    val mutable funcs : (string * var list) list = [] (* defined functions in scope *)
    val mutable typs : TypSet.t = all_typs (* allowed types at focus *)

    method funcs = funcs
    method typs = typs

    method add_func (name : string) (args : var list) : unit =
      funcs <- (name,args)::funcs

    val mutable typ_to_be_defined = true
    method any_typ : unit =
      typ_to_be_defined <- false
    method only_typs (lt : typ list) : unit =
      if typ_to_be_defined then (
	typs <- TypSet.filter (fun t -> List.mem t lt) typs;
	typ_to_be_defined <- false
      )
    method allows_any_type : bool = not typ_to_be_defined

    val mutable focus_var : var = field_focus
    method set_focus_var (x : var) = focus_var <- x
    method focus_var : var = focus_var
  end

type sem = { annot : annot; expr : expr }

let expr_bind_in (x : var) e1 e = flower (For (Var x,e1,false, return e))

(*let expr_for_inputs args inputs e =
  let inputs =
    if inputs = [] (* need to bind arguments as variables *)
    then [ List.map (fun _ -> Seq.return `Null) args ]
    else inputs in
  let item_of_data d =
    d
    |> Seq.case
	 ~nil:(fun () -> `Null)
	 ~cons:(fun i next ->
		next
		|> Seq.case
		     ~nil:(fun () -> i)
		     ~cons:(fun _ _ -> `List (Seq.to_list d))) in
  let e1_inputs =
    Concat
      (List.map
	 (fun input ->
	  Item
	    (`Assoc
	      (List.map2
		 (fun arg d -> (arg, item_of_data d))
		 args input)))
	 inputs) in
  flower (For (Fields, e1_inputs, false, return e))
 *)

	 
let rec sem_focus (foc : focus) : sem =
  let annot = new annot in
  let focus_var =
    match var_of_focus foc with
    | Some x -> x
    | None -> field_focus in
  annot#set_focus_var focus_var;
  match foc with
  | AtExpr (e,ctx) ->
     let e' = Let (Var focus_var, e, ContextEnv) in
     sem_expr_ctx annot e e' ctx
  | AtFlower (f,ctx) ->
     let f' = return (Let (Var focus_var, flower f, ContextEnv)) in
     sem_flower_ctx annot f f' ctx
and sem_expr_ctx annot e e' : expr_ctx -> sem = function
  | Root ->
     annot#any_typ;
     { annot; expr=e' }
  | ConcatX (ll_rr,ctx) ->
     let e = Concat (Focus.list_of_ctx e ll_rr) in
     sem_expr_ctx annot e e' ctx
  | Exists1 (x,ctx,e2) ->
     annot#any_typ;
     let e = Exists (x,e,e2) in
     sem_expr_ctx annot e e' ctx
  | Exists2 (x,e1,ctx) ->
     (*annot#only_typs [`Bool];*)
     let e = Exists (x,e1,e) in
     sem_expr_ctx annot e (expr_bind_in x e1 e') ctx
  | ForAll1 (x,ctx,e2) ->
     annot#any_typ;
     let e = ForAll (x,e,e2) in
     sem_expr_ctx annot e e' ctx
  | ForAll2 (x,e1,ctx) ->
     (*annot#only_typs [`Bool];*)
     let e = ForAll (x,e1,e) in
     sem_expr_ctx annot e (expr_bind_in x e1 e') ctx
  | If1 (ctx,e2,e3) ->
     annot#only_typs [`Bool];
     let e = If (e,e2,e3) in
     sem_expr_ctx annot e e' ctx
  | If2 (e1,ctx,e3) ->
     let e = If (e1,e,e3) in
     sem_expr_ctx annot e (If (e1,e',Empty)) ctx
  | If3 (e1,e2,ctx) ->
     let e = If (e1,e2,e) in
     sem_expr_ctx annot e (If (e1,Empty,e')) ctx
  | OrX (ll_rr,ctx) ->
     annot#only_typs [`Bool];
     let e = Or (Focus.list_of_ctx e ll_rr) in
     sem_expr_ctx annot e e' ctx
  | AndX (ll_rr,ctx) ->
     annot#only_typs [`Bool];
     let e = And (Focus.list_of_ctx e ll_rr) in
     sem_expr_ctx annot e e' ctx
  | Not1 ctx ->
     annot#only_typs [`Bool];
     let e = Not e in
     sem_expr_ctx annot e e' ctx
  | CallX (func,ll_rr,ctx) ->
     (* TODO: type constraints *)
     annot#any_typ;
     let e = Call (func, Focus.list_of_ctx e ll_rr) in
     sem_expr_ctx annot e e' ctx
  | Map1 (ctx,e2) ->
     annot#any_typ;
     let e = Map (e,e2) in
     sem_expr_ctx annot e e' ctx
  | Map2 (e1,ctx) ->
     let e = Map (e1,e) in
     sem_expr_ctx annot e (expr_bind_in var_context e1 e') ctx
  | Pred1 (ctx,e2) ->
     let e = Pred (e,e2) in
     sem_expr_ctx annot e e' ctx
  | Pred2 (e1,ctx) ->
     (*annot#only_typs [`Bool];*) (* annoying constraint when building *)
     let e = Pred (e1,e) in
     sem_expr_ctx annot e (expr_bind_in var_context e1 e') ctx
  | Dot1 (ctx,e2) ->
     annot#only_typs [`Object];
     let e = Dot (e,e2) in
     sem_expr_ctx annot e e' ctx
  | Dot2 (e1,ctx) ->
     annot#only_typs [`String];
     let e = Dot (e1,e) in
     sem_expr_ctx annot e e' ctx
  | ArrayLookup1 (ctx,e2) ->
     annot#only_typs [`Array];
     let e = ArrayLookup (e,e2) in
     sem_expr_ctx annot e e' ctx
  | ArrayLookup2 (e1,ctx) ->
     annot#only_typs [`Int];
     let e = ArrayLookup (e1,e) in
     sem_expr_ctx annot e e' ctx
  | ArrayUnboxing1 ctx ->
     annot#only_typs [`Array];
     let e = ArrayUnboxing e in
     sem_expr_ctx annot e e' ctx
  | EObjectX1 (ll_rr,ctx,e2) ->
     annot#only_typs [`String];
     let e = EObject (Focus.list_of_ctx (e,e2) ll_rr) in
     sem_expr_ctx annot e e' ctx
  | EObjectX2 (ll_rr,e1,ctx) ->
     annot#any_typ;
     let e = EObject (Focus.list_of_ctx (e1,e) ll_rr) in
     sem_expr_ctx annot e e' ctx
  | Objectify1 ctx ->
     annot#only_typs [`Object];
     let e = Objectify e in
     sem_expr_ctx annot e e' ctx
  | Arrayify1 ctx ->
     annot#any_typ;
     let e = Arrayify e in
     sem_expr_ctx annot e e' ctx
  | Let1 (x,ctx,e2) ->
     annot#any_typ;
     let e = Let (x,e,e2) in
     sem_expr_ctx annot e e' ctx
  | Let2 (x,e1,ctx) ->
     let e = Let (x,e1,e) in
     sem_expr_ctx annot e (Let (x,e1,e')) ctx
  | DefFunc0 (name,args,ctx,e1,e2) ->
     let e = DefFunc (name,args,e,e1,e2) in
     sem_expr_ctx annot e e' ctx
  | DefFunc1 (name,args,e0,ctx,e2) ->
     annot#add_func name args;
     let e1 = e in
     let e = DefFunc (name,args,e0,e,e2) in
     sem_expr_ctx annot e (DefFunc (name,args,e0,e1, flower (For (Fields, e0, false, return e')))) ctx
  | DefFunc2 (name,args,e0,e1,ctx) ->
     annot#add_func name args;
     let e = DefFunc (name,args,e0,e1,e) in
     sem_expr_ctx annot e (DefFunc (name,args,e0,e1,e')) ctx
  | Return1 ctx ->
     let f = return e in
     sem_flower_ctx annot f (return e') ctx
  | For1 (br,ctx,opt,f) ->
     annot#any_typ;
     let f = For (br,e,opt,f) in
     sem_flower_ctx annot f (return e') ctx
  | FLet1 (br,ctx,f) ->
     annot#any_typ;
     let f = FLet (br,e,f) in
     sem_flower_ctx annot f (return e') ctx
  | Where1 (ctx,f) ->
     annot#only_typs [`Bool];
     let f = Where (e,f) in
     sem_flower_ctx annot f (return e') ctx
  | OrderBy1X (ll_rr,ctx,o,f) ->
     annot#only_typs [`Bool; `Int; `Float; `String];
     let f = OrderBy (Focus.list_of_ctx (e,o) ll_rr, f) in
     sem_flower_ctx annot f (return e') ctx
and sem_flower_ctx annot f f' : flower_ctx -> sem = function
  | Flower1 ctx ->
     let e = flower f in
     sem_expr_ctx annot e (flower f') ctx
  | For2 (br,e1,opt,ctx) ->
     let f = For (br,e1,opt,f) in
     sem_flower_ctx annot f (For (br,e1,opt,f')) ctx
  | FLet2 (br,e1,ctx) ->
     let f = FLet (br,e1,f) in
     sem_flower_ctx annot f (FLet (br,e1,f')) ctx
  | Count1 (x,ctx) ->
     let f = Count (x,f) in
     sem_flower_ctx annot f (Count (x,f')) ctx
  | Where2 (e1,ctx) ->
     let f = Where (e1,f) in
     sem_flower_ctx annot f (Where (e1,f')) ctx
  | GroupBy1 (lx,ctx) ->
     let f = GroupBy (lx,f) in
     sem_flower_ctx annot f (GroupBy (lx,f')) ctx
  | Hide1 (lx,ctx) ->
     let f = Hide (lx,f) in
     sem_flower_ctx annot f (Hide (lx,f')) ctx
  | Slice1 (o,l,ctx) ->
     let f = Slice (o,l,f) in
     sem_flower_ctx annot f (Slice (o,l,f')) ctx
  | OrderBy2 (leo,ctx) ->
     let f = OrderBy (leo,f) in
     sem_flower_ctx annot f (OrderBy (leo,f')) ctx
  | FConcatX (ll_rr,ctx) ->
     let f = FConcat (Focus.list_of_ctx f ll_rr) in
     sem_flower_ctx annot f f' ctx
  | FIf1 (ctx,f2,f3) ->
     annot#only_typs [`Bool];
     let f = FIf (f,f2,f3) in
     sem_flower_ctx annot f f' ctx
  | FIf2 (f1,ctx,f3) ->
     let f = FIf (f1,f,f3) in
     sem_flower_ctx annot f (FIf (f1,f',return Empty)) ctx
  | FIf3 (f1,f2,ctx) ->
     let f = FIf (f1,f2,f) in
     sem_flower_ctx annot f (FIf (f1,return Empty,f')) ctx

type extent = { vars : var list; focus_var : var; bindings : env Seq.t }

let extent (library : #Jsoniq.library) (sem : sem) : extent =
  let focus_var = sem.annot#focus_var in
  let res = eval_expr library [] [] sem.expr in
  let bindings = Seq.map (fun (_,env) -> env) res in
  let vars =
    match Seq.hd_opt bindings with
    | None -> []
    | Some binding -> List.rev_map fst binding in
  { vars; focus_var; bindings }
