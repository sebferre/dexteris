
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

    
let suggestions (foc : focus) (sem : Sem.sem) (extent : Sem.extent) : suggestion Lis.forest list =
  let focus_typs, focus_lens, fields, nbindings = focus_types_lengths_fields extent in
  let focus_typs =
    if Sem.TypSet.is_empty focus_typs (* empty sequence () *)
    then Sem.all_typs
    else focus_typs in
  let ctx_typs = sem.Sem.annot#typs in
  (*  let allows_any_type = sem.Sem.annot#allows_any_type in *)
  let allowed_typs = Sem.TypSet.inter ctx_typs focus_typs in
  let multiple_items = Bintree.fold (fun n ok -> ok || n > 1) focus_lens false in
  let multiple_bindings = nbindings > 1 in
  let forest_op = ref [] in
  let forest_val = ref [] in
  let forest_flower = ref [] in
  let add kind ?(path : string list = []) tr =
    let forest =
      match kind with
      | `Op -> forest_op
      | `Val -> forest_val
      | `Flower -> forest_flower in
    forest := Lis.insert_suggestion path tr !forest in
  let () =
    add `Val FocusUp;
    add `Val InsertConcat1;
    add `Val InsertConcat2;
    List.iter
      (fun x ->
       if x <> Sem.field_focus then
	 add `Val (InsertVar x))
      extent.Sem.vars;
    Bintree.iter
      (fun k -> add `Val (InsertField k))
      fields;
    if Sem.TypSet.mem `Int ctx_typs then
      add `Val ~path:[] (InputInt (new input 0));
    if Sem.TypSet.mem `Int ctx_typs then
      add `Val ~path:[] (InputRange (new input 0, new input 10));
    if Sem.TypSet.mem `Float ctx_typs then
      add `Val ~path:[] (InputFloat (new input 0.));
    if Sem.TypSet.mem `String ctx_typs then
      add `Val ~path:[] (InputString (new input ""));
    if Sem.TypSet.mem `Bool ctx_typs then (
      add `Val ~path:[] (InsertBool false);
      add `Val ~path:[] (InsertBool true)
    );
    add `Val ~path:[] InsertNull;
    add `Flower (InsertLet1 (new input ""));
    add `Flower (InsertLet2 (new input ""));
    if multiple_items then (
      add `Flower ~path:["iterations"] InsertMap;
      add `Flower ~path:["iterations"] InsertPred);
    if multiple_items then (
      add `Flower ~path:["iterations"] (InsertFor1 (new input "", new input false)));
    add `Flower ~path:["iterations"] (InputFileData (new input ("",Seq.empty)));
    add `Flower ~path:["iterations"] (InsertFor2 (new input "", new input false));
    Jsoniq_functions.library#iter
      (fun func ->
       if func#typecheck focus_typs ctx_typs
       then add `Op ~path:func#path (InsertFunc (func#name, func#arity)));
    List.iter
      (fun (name,args) ->
       add `Op ~path:["defined functions"] (InsertFunc (name, List.length args)))
      sem.Sem.annot#funcs;
    if Sem.TypSet.mem `Bool ctx_typs then (
      add `Flower ~path:["iterations"] (InsertExists (new input ""));
      add `Flower ~path:["iterations"] (InsertForAll (new input "")));
    if Sem.TypSet.mem `Bool allowed_typs then (
      add `Op ~path:["logic"] InsertOr;
      add `Op ~path:["logic"] InsertAnd;
      add `Op ~path:["logic"] InsertNot;
      add `Op ~path:["logic"] InsertIf1);
    add `Op ~path:["logic"] InsertIf2;
    add `Op ~path:["logic"] InsertIf3;
    if Sem.TypSet.mem `Object ctx_typs then (
      add `Val ~path:["objects"] InsertObject;
    (*add `Val InsertContextEnv*));      
    if Sem.TypSet.mem `Object allowed_typs then (
      add `Op ~path:["objects"] InsertDot;
      add `Val ~path:["objects"] InsertObjectify);
    if Sem.TypSet.mem `Array allowed_typs then (
      add `Op ~path:["arrays"] InsertArrayLookup);
    if Sem.TypSet.mem `Array focus_typs then (
      add `Val ~path:["arrays"] InsertArrayUnboxing);
    if Sem.TypSet.mem `Array ctx_typs then (
      (*add `Val ~path:["arrays"] InsertArray;*)
      add `Val ~path:["arrays"] InsertArrayify);
    add `Flower ~path:[] InsertWhere2;
    if multiple_bindings then (
      if Sem.TypSet.mem `Bool focus_typs then
	add `Flower ~path:[] InsertWhere1;
      let group_by_vars = (* TODO: suggest only variables not yet grouped *)
	List.filter ((<>) Sem.field_focus) extent.Sem.vars in
      if group_by_vars <> [] then
	add `Flower ~path:["modifiers"] (InsertGroupBy (group_by_vars, new input (List.hd group_by_vars))); 
      add `Flower ~path:["modifiers"] (InsertSlice (new input 0, new input 0));
      add `Flower ~path:["modifiers"] (InsertOrderBy1 (new input (string_of_order ASC))));
    if extent.Sem.vars <> [] then
      add `Flower ~path:["modifiers"] (InsertProject (extent.Sem.vars, new input (List.hd extent.Sem.vars)));
    add `Flower ~path:["modifiers"] (InsertOrderBy2 (new input (string_of_order ASC)));
    add `Flower ~path:["functions"] (InsertDefFunc1 (new input ""));
    add `Flower ~path:["functions"] (InsertDefFunc2 (new input ""));
    add `Flower ~path:["functions"] (InsertArg (new input ""));
  in
  [!forest_op; !forest_val; !forest_flower]
