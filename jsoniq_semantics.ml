
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
  { sem with env = x::sem.env; expr = Flower (For (x,e1,false, flower_of_expr sem.expr)) }
let sem_map f sem =
  { sem with expr = f sem.expr }
let sem_var x f sem =
  { sem with env = x::sem.env; expr = f sem.expr }
let sem_func name args f sem =
  { sem with funcs = (name,args)::sem.funcs; expr = f sem.expr }
let sem_to_flower sem =
  { sem with expr = flower_of_expr sem.expr }
let sem_to_expr sem =
  { sem with expr = expr_of_flower sem.expr }
 *)
		  
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
  end

type sem = { annot : annot; expr : expr }

let field_focus = "@focus"

let expr_bind_in (x : var) e1 e = Flower (For (Var x,e1,false, flower_of_expr e))

let rec expr_bind_list_in lxe1 e =
  match lxe1 with
  | [] -> e
  | (x,e1)::r -> Flower (For (Var x,e1,false, flower_of_expr (expr_bind_list_in r e)))
			 
let rec sem_focus (foc : focus) : sem =
  let annot = new annot in
  match foc with
  | AtExpr (e,ctx) ->
     let e' = Let (Var field_focus, e, ContextEnv) in
     (*     let e' = Objectify (Concat [ContextEnv; EObject [S field_focus, Arrayify e]]) in *)
     sem_expr_ctx annot e' ctx
  | AtFlower (f,ctx) ->
     let f' = Return (Let (Var field_focus, Flower f, ContextEnv)) in
     (*     let f' = Return (Objectify (Concat [ContextEnv; EObject [S field_focus, Arrayify (Flower f)]])) in *)
     sem_flower_ctx annot f' ctx
and sem_expr_ctx annot e : expr_ctx -> sem = function
  | Root ->
     annot#any_typ;
     { annot; expr=e }
  | ConcatX (ll_rr,ctx) -> sem_expr_ctx annot e ctx
  | Exists1 (x,ctx,e2) ->
     annot#any_typ;
     sem_expr_ctx annot e ctx
  | Exists2 (x,e1,ctx) ->
     (*annot#only_typs [`Bool];*)
     sem_expr_ctx annot (expr_bind_in x e1 e) ctx
  | ForAll1 (x,ctx,e2) ->
     annot#any_typ;
     sem_expr_ctx annot e ctx
  | ForAll2 (x,e1,ctx) ->
     (*annot#only_typs [`Bool];*)
     sem_expr_ctx annot (expr_bind_in x e1 e) ctx
  | If1 (ctx,e2,e3) ->
     annot#only_typs [`Bool];
     sem_expr_ctx annot e ctx
  | If2 (e1,ctx,e3) -> sem_expr_ctx annot e ctx
  | If3 (e1,e2,ctx) -> sem_expr_ctx annot e ctx
  | OrX (ll_rr,ctx) ->
     annot#only_typs [`Bool];
     sem_expr_ctx annot e ctx
  | AndX (ll_rr,ctx) ->
     annot#only_typs [`Bool];
     sem_expr_ctx annot e ctx
  | Not1 ctx ->
     annot#only_typs [`Bool];
     sem_expr_ctx annot e ctx
  | CallX (func,ll_rr,ctx) ->
     (* TODO: type constraints *)
     annot#any_typ;
     sem_expr_ctx annot e ctx
  | Map1 (ctx,e2) ->
     annot#any_typ;
     sem_expr_ctx annot e ctx
  | Map2 (e1,ctx) ->
     sem_expr_ctx annot (expr_bind_in var_context e1 e) ctx
  | Pred1 (ctx,e2) -> sem_expr_ctx annot e ctx
  | Pred2 (e1,ctx) ->
     (*annot#only_typs [`Bool];*) (* annoying constraint when building *)
     sem_expr_ctx annot (expr_bind_in var_context e1 e) ctx
  | Dot1 (ctx,e2) ->
     annot#only_typs [`Object];
     sem_expr_ctx annot e ctx
  | Dot2 (e1,ctx) ->
     annot#only_typs [`String];
     sem_expr_ctx annot e ctx
  | ArrayLookup1 (ctx,e2) ->
     annot#only_typs [`Array];
     sem_expr_ctx annot e ctx
  | ArrayLookup2 (e1,ctx) ->
     annot#only_typs [`Int];
     sem_expr_ctx annot e ctx
  | ArrayUnboxing1 ctx ->
     annot#only_typs [`Array];
     sem_expr_ctx annot e ctx
  | EObjectX1 (ll_rr,ctx,e2) ->
     annot#only_typs [`String];
     sem_expr_ctx annot e ctx
  | EObjectX2 (ll_rr,e1,ctx) ->
     annot#any_typ;
     sem_expr_ctx annot e ctx
  | Objectify1 ctx ->
     annot#only_typs [`Object];
     sem_expr_ctx annot e ctx
  | Arrayify1 ctx ->
     annot#any_typ;
     sem_expr_ctx annot e ctx
  | Let1 (x,ctx,e2) ->
     annot#any_typ;
     sem_expr_ctx annot e ctx
  | Let2 (x,e1,ctx) ->
     sem_expr_ctx annot (Let (x,e1,e)) ctx
  | DefFunc1 (name,args,ctx,e2) -> (* add args' example values *)
     annot#add_func name args;
     sem_expr_ctx annot
		  (expr_bind_list_in
		     (List.map (fun x -> (x,Item (`Int 0))) args) e) ctx
  | DefFunc2 (name,args,e1,ctx) ->
     annot#add_func name args;
     sem_expr_ctx annot (DefFunc (name,args,e1,e)) ctx
  | Return1 ctx -> sem_flower_ctx annot (flower_of_expr e) ctx
  | For1 (br,ctx,opt,f) ->
     annot#any_typ;
     sem_flower_ctx annot (flower_of_expr e) ctx
  | FLet1 (br,ctx,f) ->
     annot#any_typ;
     sem_flower_ctx annot (flower_of_expr e) ctx
  | Where1 (ctx,f) ->
     annot#only_typs [`Bool];
     sem_flower_ctx annot (flower_of_expr e) ctx
  | OrderBy1X (ll_rr,ctx,o,f) ->
     annot#only_typs [`Bool; `Int; `Float; `String];
     sem_flower_ctx annot (flower_of_expr e) ctx
and sem_flower_ctx annot f : flower_ctx -> sem = function
  | Flower1 ctx -> sem_expr_ctx annot (expr_of_flower f) ctx
  | FileData2 (fname,d,ctx) ->
     sem_flower_ctx annot (FileData (fname,d,f)) ctx
  | For2 (br,e1,opt,ctx) ->
     sem_flower_ctx annot (For (br,e1,opt,f)) ctx
  | FLet2 (br,e1,ctx) ->
     sem_flower_ctx annot (FLet (br,e1,f)) ctx
  | Count1 (x,ctx) ->
     sem_flower_ctx annot (Count (x,f)) ctx
  | Where2 (e1,ctx) -> sem_flower_ctx annot (Where (e1,f)) ctx
  | GroupBy1 (lx,ctx) -> sem_flower_ctx annot (GroupBy (lx,f)) ctx
  | Project1 (lx,ctx) -> sem_flower_ctx annot (Project (lx,f)) ctx
  | Slice1 (o,l,ctx) -> sem_flower_ctx annot (Slice (o,l,f)) ctx
  | OrderBy2 (leo,ctx) -> sem_flower_ctx annot (OrderBy (leo,f)) ctx
  | FConcatX (ll_rr,ctx) -> sem_flower_ctx annot f ctx
  | FIf1 (ctx,e2,e3) ->
     annot#only_typs [`Bool];
     sem_flower_ctx annot f ctx
  | FIf2 (e1,ctx,e3) -> sem_flower_ctx annot f ctx
  | FIf3 (e1,e2,ctx) -> sem_flower_ctx annot f ctx

type extent = { vars : var list; bindings : env Seq.t }

let extent (library : #Jsoniq.library) (sem : sem) : extent =
  let res = eval_expr library [] [] sem.expr in
  let bindings = Seq.map (fun (_,env) -> env) res in
  let vars =
    match Seq.hd_opt bindings with
    | None -> []
    | Some binding -> List.rev_map fst binding in
  { vars; bindings }
