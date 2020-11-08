
open Js_of_ocaml
open Jsoniq

let escape l s =
  let s = Regexp.global_replace (Regexp.regexp_string "\\") s "\\\\" in
  List.fold_left
    (fun s (a,b) -> Regexp.global_replace (Regexp.regexp_string a) s b)
    s l

let escape_uri = escape [(">","\\>")]
let escape_shortString = escape [("\"","\\\""); ("\n","\\n"); ("\r","\\r"); ("\t","\\t")]
let escape_longString = escape [("\"","\\\"")]
  
       
let rec obj_of_atom (i : item) : (string * item list) option =
  match i with
  | `Bool b -> Some ((if b then "true" else "false"), [])
  | `Int i -> Some (string_of_int i, [])
  | `Float f -> Some (string_of_float f, [])
  | `String s -> Some ("\"" ^ escape_shortString s ^ "\"", [])
  | `Null -> None
  | `Assoc ["@value", `String v; "@language", `String l] -> Some ("\"" ^ escape_shortString v ^ "\"@" ^ l, [])
  | `Assoc ["@value", `String v; "@type", `String t] -> Some ("\"" ^ escape_shortString v ^ "\"^^" ^ t, [])
  | `Assoc ["@list", `List li] ->
     let lo,ln =
       List.fold_right
	 (fun i (lo,ln) ->
	  match obj_of_atom i with
	  | Some (o, ln1) -> o::lo, ln1@ln
	  | None -> lo, ln)
	 li ([],[]) in
     Some ("(" ^ String.concat " " lo ^ ")", ln)
  | `Assoc pairs ->
     (match List.assoc_opt "@id" pairs with
      | Some (`String s) -> Some (s, [i])
      | _ -> failwith "turtle_of_data: invalid_object")
  | `List _ -> failwith "turtle_of_data: invalid object (array)"

let rec turtle_jsonld buf d =
  d |> Seq.iter (turtle_node buf)
and turtle_node buf = function
  | `Assoc pairs ->
     let s, lt, lpo, ln = (* subject IRI, types, properties, sub-nodes *)
       List.fold_right
	 (fun (k,i) (s,lt,lpo,ln) ->
	  match k, i with
	  | "@id", `String s -> s, lt, lpo, ln
	  | "@id", _ -> failwith "turtle_of_data: invalid @id"
	  | "@type", `String t -> s, t::lt, lpo, ln
	  | "@type", `List li ->
	     let lt =
	       List.fold_right
		 (fun i res ->
		  match i with
		  | `String t -> t::res
		  | _ -> res)
		 li lt in
	     s, lt, lpo, ln
	  | "@type", _ -> failwith "turtle_of_data: invalid @type"
	  | _, `List li ->
	     let lo, ln =
	       List.fold_right
		 (fun i (lo,ln) ->
		  match obj_of_atom i with
		  | Some (o, ln1) -> o::lo, ln1@ln
		  | None -> lo, ln)
		 li ([], ln) in
	     s, lt, (k,lo)::lpo, ln
	  | _, _ ->
	     (match obj_of_atom i with
	      | Some (o, ln1) -> s, lt, (k,[o])::lpo, ln1@ln
	      | None -> s, lt, lpo, ln))
	 pairs ("[]", [], [], []) in
     if lt <> [] || lpo <> [] then (
       Buffer.add_string buf s;
       if lt <> [] then (
	 Buffer.add_string buf " a ";
	 Buffer.add_string buf (String.concat ", " lt)
       );
       let first = ref true in
       List.iter
	 (fun (p,lo) ->
	  if not !first || lt <> [] then Buffer.add_string buf ";";
	  first := false;
	  Buffer.add_string buf "\n    ";
	  Buffer.add_string buf p;
	  Buffer.add_string buf " ";
	  Buffer.add_string buf (String.concat ", " lo))
	 lpo;
       Buffer.add_string buf ".\n"
     );
     List.iter (turtle_node buf) ln
  | _ -> ()

       
let turtle_of_data (d : data) : string =
  let buf = Buffer.create 10103 in
  List.iter
    (fun (prefix,iri) ->
     Buffer.add_string buf "@prefix ";
     Buffer.add_string buf prefix;
     Buffer.add_string buf "<";
     Buffer.add_string buf iri;
     Buffer.add_string buf ">.\n")
    ["rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
     "rdfs:", "http://www.w3.org/2000/01/rdf-schema#";
     "owl:", "http://www.w3.org/2002/07/owl#";
     "xsd:", "http://www.w3.org/2001/XMLSchema#"];
  turtle_jsonld buf d;
  Buffer.contents buf
