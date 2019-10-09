
open Focus
open Jsoniq
open Jsoniq_focus
module Sem = Jsoniq_semantics

type suggestion = transf

let focus_types_lengths (extent : Sem.extent) : Sem.TypSet.t * int Bintree.t =
  List.fold_left
    (fun (typs,lens) binding ->
     try
       let i0 = List.assoc Sem.field_focus binding in
       match i0 with
       | Array li ->
	  let typs, len =
	    List.fold_left
	      (fun (typs,len) i ->
	       let typs =
		 match i with
		 | Bool _ -> Sem.TypSet.add `Bool typs
		 | Int _ -> Sem.TypSet.add `Int typs
		 | Float _ -> Sem.TypSet.add `Float typs
		 | String _ -> Sem.TypSet.add `String typs
		 | Null -> typs
		 | Object _ -> Sem.TypSet.add `Object typs
		 | Array _ -> Sem.TypSet.add `Array typs in
	       typs, len+1)
	      (typs,0) li in
	  typs, Bintree.add len lens
       | _ -> assert false
     with Not_found -> typs, lens)
    (Sem.TypSet.empty, Bintree.empty) extent.Sem.bindings
			     
let suggestions (foc : focus) (sem : Sem.sem) (extent : Sem.extent) : suggestion list list =
  let focus_typs, focus_lens = focus_types_lengths extent in
  let ctx_typs = sem.Sem.annot#typs in
  let allowed_typs = Sem.TypSet.inter ctx_typs focus_typs in
  let multiple_items = Bintree.fold (fun n ok -> ok || n > 1) focus_lens false in
  let multiple_bindings = List.length extent.Sem.bindings > 1 in
  let transfs = ref [] in
  let add kind tr = transfs := (kind,tr) :: !transfs in
  let () =
    add `Val FocusUp;
    List.iter
      (fun x -> if x <> Sem.field_focus then add `Val (InsertVar x))
      sem.Sem.annot#env;
    if Sem.TypSet.mem `Bool ctx_typs then ( add `Val (InsertBool false); add `Val (InsertBool true) );
    if Sem.TypSet.mem `Int ctx_typs then add `Val (InputInt (new input 0));
    if Sem.TypSet.mem `Int ctx_typs then add `Val (InputRange (new input 0, new input 10));
    if Sem.TypSet.mem `Float ctx_typs then add `Val (InputFloat (new input 0.));
    if Sem.TypSet.mem `String ctx_typs then add `Val (InputString (new input ""));
    add `Val InsertNull;
    add `Val InsertConcat;
    if multiple_items then (
      add `Flower InsertMap;
      add `Flower InsertPred);
    if multiple_items then (
      add `Flower (InsertFor (new input "", new input false));
      add `Flower (InsertForObject (new input false)));
    add `Flower (InsertLet (new input ""));
    if Sem.TypSet.mem `Bool ctx_typs then
      List.iter
	(fun func -> add `Op (InsertFunc func))
	[EQ; NE; LE; LT; GE; GT];
    if Sem.TypSet.mem `Int ctx_typs || Sem.TypSet.mem `Float ctx_typs then
      List.iter
	(fun func -> add `Op (InsertFunc func))
	[Plus; Minus; Times; Div; IDiv; Mod; Neg; Range; Sum; Avg];
    if Sem.TypSet.mem `String ctx_typs then
      List.iter
	(fun func -> add `Op (InsertFunc func))
	[StringConcat; Substring];
    if multiple_items then add `Op (InsertFunc Count);
    List.iter
      (fun (name,args) ->
       add `Op (InsertFunc (Defined (name, List.length args))))
      sem.Sem.annot#funcs;
    if Sem.TypSet.mem `Bool ctx_typs then (
      add `Flower (InsertExists (new input ""));
      add `Flower (InsertForAll (new input "")));
    if Sem.TypSet.mem `Bool allowed_typs then (
      add `Op InsertOr;
      add `Op InsertAnd;
      add `Op InsertNot;
      add `Op InsertIf1);
    add `Op InsertIf2;
    add `Op InsertIf3;
    if Sem.TypSet.mem `Object ctx_typs then (
      add `Val InsertObject;
      add `Val InsertContextEnv);      
    if Sem.TypSet.mem `Object allowed_typs then (
      add `Val InsertDot;
      add `Val InsertObjectify);
    if Sem.TypSet.mem `Array allowed_typs then (
      add `Val InsertArrayLookup);
    if Sem.TypSet.mem `Array focus_typs then (
      add `Val InsertArrayUnboxing);
    if Sem.TypSet.mem `Array ctx_typs then (
      add `Val InsertArray;
      add `Val InsertArrayify);
    add `Flower (InsertDefVar (new input ""));
    add `Flower (InsertDefFunc (new input ""));
    add `Flower (InsertArg (new input ""));
    if multiple_bindings then (
      List.iter
	(fun x -> add `Flower (InsertGroupBy x)) (* TODO: suggest only variables not yet grouped *)
	sem.Sem.annot#env;
      if Sem.TypSet.mem `Bool focus_typs then add `Flower InsertWhere;
      add `Flower (InsertOrderBy DESC);
      add `Flower (InsertOrderBy ASC));
  in
  let lsugg_val, lsugg_op, lsugg_flower =
    List.fold_left
      (fun (lv,lo,lf) (kind,tr) ->
       match apply_transf tr foc with
       | Some _ ->
	  ( match kind with
	    | `Val -> (tr::lv,lo,lf)
	    | `Op -> (lv,tr::lo,lf)
	    | `Flower -> (lv,lo,tr::lf) )
       | None -> (lv,lo,lf))
      ([],[],[]) !transfs in
  [lsugg_op; lsugg_val; lsugg_flower]
