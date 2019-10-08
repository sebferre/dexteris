
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
			     
let suggestions (foc : focus) (sem : Sem.sem) (extent : Sem.extent) : suggestion list =
  let focus_typs, focus_lens = focus_types_lengths extent in
  let ctx_typs = sem.Sem.annot#typs in
  let allowed_typs = Sem.TypSet.inter ctx_typs focus_typs in
  let multiple_items = Bintree.fold (fun n ok -> ok || n > 1) focus_lens false in
  let multiple_bindings = List.length extent.Sem.bindings > 1 in
  let transfs = ref [] in
  let add tr = transfs := tr :: !transfs in
  let () =
    add FocusUp;
    List.iter
      (fun x -> if x <> Sem.field_focus then add (InsertVar x))
      sem.Sem.annot#env;
    if Sem.TypSet.mem `Bool ctx_typs then ( add (InsertBool false); add (InsertBool true) );
    if Sem.TypSet.mem `Int ctx_typs then add (InputInt (new input 0));
    if Sem.TypSet.mem `Int ctx_typs then add (InputRange (new input 0, new input 10));
    if Sem.TypSet.mem `Float ctx_typs then add (InputFloat (new input 0.));
    if Sem.TypSet.mem `String ctx_typs then add (InputString (new input ""));
    add InsertNull;
    add InsertConcat;
    if multiple_items then (
      add InsertMap;
      add InsertPred);
    if multiple_items then (
      add (InsertFor (new input "", new input false));
      add (InsertForObject (new input false)));
    add (InsertLet (new input ""));
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
    if multiple_items then add (InsertFunc Count);
    List.iter
      (fun (name,args) ->
       add (InsertFunc (Defined (name, List.length args))))
      sem.Sem.annot#funcs;
    if Sem.TypSet.mem `Bool ctx_typs then (
      add (InsertExists (new input ""));
      add (InsertForAll (new input "")));
    if Sem.TypSet.mem `Bool allowed_typs then (
      add InsertOr;
      add InsertAnd;
      add InsertNot;
      add InsertIf1);
    add InsertIf2;
    add InsertIf3;
    if Sem.TypSet.mem `Object ctx_typs then (
      add InsertObject;
      add InsertContextEnv);      
    if Sem.TypSet.mem `Object allowed_typs then (
      add InsertDot;
      add InsertObjectify);
    if Sem.TypSet.mem `Array allowed_typs then (
      add InsertArrayLookup);
    if Sem.TypSet.mem `Array focus_typs then (
      add InsertArrayUnboxing);
    if Sem.TypSet.mem `Array ctx_typs then (
      add InsertArray;
      add InsertArrayify);
    add (InsertDefVar (new input ""));
    add (InsertDefFunc (new input ""));
    add (InsertArg (new input ""));
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
     | Some foc' -> tr :: res
     | None -> res)
    [] !transfs
