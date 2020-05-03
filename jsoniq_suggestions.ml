
open Focus
open Jsoniq
open Jsoniq_focus
module Sem = Jsoniq_semantics

type suggestion = transf

let focus_types_lengths_fields (extent : Sem.extent) : Sem.TypSet.t * int Bintree.t * string Bintree.t * int =
  let max_bindings = 20 in
  let max_items_per_cell = 20 in
  List.fold_left
    (fun (typs,lens,fields,nbindings) binding ->
     try
       let d0 = List.assoc Sem.field_focus binding in
       let li, _ = Seq.take max_items_per_cell d0 in
       let typs, len, fields =
	 List.fold_left
	   (fun (typs,len,fields) i ->
	    let typs, fields =
	      match i with
	      | `Bool _ -> Sem.TypSet.add `Bool typs, fields
	      | `Int _ -> Sem.TypSet.add `Int typs, fields
	      | `Float _ -> Sem.TypSet.add `Float typs, fields
	      | `String _ -> Sem.TypSet.add `String typs, fields
	      | `Null -> typs, fields
	      | `Assoc pairs ->
		 Sem.TypSet.add `Object typs,
		 List.fold_left (fun fields (k,_) -> Bintree.add k fields) fields pairs
	      | `List _ -> Sem.TypSet.add `Array typs, fields in
	    typs, len+1, fields)
	   (typs,0,fields) li in
       typs, Bintree.add len lens, fields, nbindings+1
     with Not_found -> typs, lens, fields, nbindings)
    (Sem.TypSet.empty, Bintree.empty, Bintree.empty, 0)
    (fst (Seq.take max_bindings extent.Sem.bindings))

    
let suggestions (foc : focus) (sem : Sem.sem) (extent : Sem.extent) : suggestion list list =
  let focus_typs, focus_lens, fields, nbindings = focus_types_lengths_fields extent in
  let ctx_typs = sem.Sem.annot#typs in
  (*  let allows_any_type = sem.Sem.annot#allows_any_type in *)
  let allowed_typs = Sem.TypSet.inter ctx_typs focus_typs in
  let multiple_items = Bintree.fold (fun n ok -> ok || n > 1) focus_lens false in
  let multiple_bindings = nbindings > 1 in
  let transfs = ref [] in
  let add kind tr = transfs := (kind,tr) :: !transfs in
  let () =
    add `Val FocusUp;
    add `Val InsertConcat1;
    add `Val InsertConcat2;
    List.iter
      (fun x -> if x <> Sem.field_focus then add `Val (InsertVar x))
      extent.Sem.vars;
    if Sem.TypSet.mem `Bool ctx_typs then ( add `Val (InsertBool false); add `Val (InsertBool true) );
    if Sem.TypSet.mem `Int ctx_typs then add `Val (InputInt (new input 0));
    if Sem.TypSet.mem `Int ctx_typs then add `Val (InputRange (new input 0, new input 10));
    if Sem.TypSet.mem `Float ctx_typs then add `Val (InputFloat (new input 0.));
    if Sem.TypSet.mem `String ctx_typs then add `Val (InputString (new input ""));
    add `Val InsertNull;
    if multiple_items then (
      add `Flower InsertMap;
      add `Flower InsertPred);
    if multiple_items then (
      add `Flower (InsertFor1 (new input "", new input false)));
    add `Flower (InputFileData (new input ("",Seq.empty)));
    add `Flower (InsertFor2 (new input "", new input false));
    add `Flower (InsertLet1 (new input ""));
    add `Flower (InsertLet2 (new input ""));
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
      if not multiple_items then add `Op (InsertFunc ObjectKeys);
      add `Val InsertDot;
      add `Val InsertObjectify);
    Bintree.iter
      (fun k -> add `Val (InsertField k))
      fields;
    if Sem.TypSet.mem `Array allowed_typs then (
      if not multiple_items then add `Op (InsertFunc ArrayLength);
      add `Val InsertArrayLookup);
    if Sem.TypSet.mem `Array focus_typs then (
      add `Val InsertArrayUnboxing);
    if Sem.TypSet.mem `Array ctx_typs then (
      add `Val InsertArray;
      add `Val InsertArrayify);
    add `Flower (InsertDefFunc1 (new input ""));
    add `Flower (InsertDefFunc2 (new input ""));
    add `Flower (InsertArg (new input ""));
    add `Flower InsertWhere2;
    if multiple_bindings then (
      let group_by_vars = (* TODO: suggest only variables not yet grouped *)
	List.filter ((<>) Sem.field_focus) extent.Sem.vars in
      if group_by_vars <> [] then
	add `Flower (InsertGroupBy (group_by_vars, new input (List.hd group_by_vars))); 
      if Sem.TypSet.mem `Bool focus_typs then add `Flower InsertWhere1;
      add `Flower (InsertSlice (new input 0, new input 0));
      add `Flower (InsertOrderBy1 (new input (string_of_order ASC))));
    if extent.Sem.vars <> [] then
      add `Flower (InsertProject (extent.Sem.vars, new input (List.hd extent.Sem.vars)));
(*    List.iter
      (fun x -> add `Flower (InsertProject x))
      extent.Sem.vars; *)
    add `Flower (InsertOrderBy2 (new input (string_of_order ASC)));
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
