
open Jsoniq
open Jsoniq_focus
module Sem = Jsoniq_semantics

type suggestion = transf * focus

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
			     
let suggestions (foc : focus) (sem : Sem.sem) (extent : Sem.extent) : suggestion list =
  let focus_typs, focus_lens = focus_types_lengths extent in
  let ctx_typs = sem.Sem.annot#typs in
  let allowed_typs = Sem.TypSet.inter ctx_typs focus_typs in
  let multiple_items = Bintree.fold (fun n ok -> ok || n > 1) focus_lens false in
  let multiple_bindings = List.length extent.Sem.bindings > 1 in
  let transfs = ref [] in
  let add tr = transfs := tr :: !transfs in
  let () =
    if Sem.TypSet.mem `Bool ctx_typs then add (InputBool None);
    if Sem.TypSet.mem `Int ctx_typs then add (InputInt None);
    if Sem.TypSet.mem `Float ctx_typs then add (InputFloat None);
    if Sem.TypSet.mem `String ctx_typs then add (InputString None);
    add InsertNull;
    add InsertConcat;
    if Sem.TypSet.mem `Bool ctx_typs then (
      add (InsertExists None);
      add (InsertForAll None));
    if Sem.TypSet.mem `Bool allowed_typs then (
      add InsertOr;
      add InsertAnd;
      add InsertNot);
    if Sem.TypSet.mem `Bool ctx_typs then
      List.iter
	(fun func -> add (InsertFunc func))
	[EQ; NE; LE; LT; GE; GT];
    if Sem.TypSet.mem `Int ctx_typs || Sem.TypSet.mem `Float ctx_typs then
      List.iter
	(fun func -> add (InsertFunc func))
	[Plus; Minus; Times; Div; IDiv; Mod; Neg; Range; Sum; Avg];
    if Sem.TypSet.mem `String ctx_typs then
      List.iter
	(fun func -> add (InsertFunc func))
	[StringConcat; Substring];
    List.iter
      (fun (name,args) ->
       add (InsertFunc (Defined (name, List.length args))))
      sem.Sem.annot#funcs;
    if multiple_items then (
      add InsertMap;
      add InsertPred);
    if Sem.TypSet.mem `Object allowed_typs then (
      add InsertDot;
      add InsertObject;
      add InsertObjectify;
      add InsertContextEnv);
    if Sem.TypSet.mem `Array allowed_typs then (
      add InsertArrayLookup);
    if Sem.TypSet.mem `Array focus_typs then (
      add InsertArrayUnboxing);
    if Sem.TypSet.mem `Array ctx_typs then (
      add InsertArray;
      add InsertArrayify);
    List.iter
      (fun x -> add (InsertVar x))
      sem.Sem.annot#env;
    add (InsertDefVar None);
    add (InsertDefFunc None);
    add (InsertArg None);
    if multiple_items then (
      add (InsertFor None);
      add (InsertForObject None));
    add (InsertLet None);
    if multiple_bindings then (
      List.iter
	(fun x -> add (InsertGroupBy x)) (* TODO: suggest only variables not yet grouped *)
	sem.Sem.annot#env;
      if Sem.TypSet.mem `Bool focus_typs then add InsertWhere;
      add (InsertOrderBy DESC);
      add (InsertOrderBy ASC));
  in
  List.fold_left
    (fun res tr ->
     match apply_transf tr foc with
     | Some foc' -> (tr,foc') :: res
     | None -> res)
    [] !transfs
