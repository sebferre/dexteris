
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

let rec turtle_jsonld buf d =
  d |> Seq.iter (turtle_node buf)
and turtle_node buf = function
  | `Assoc pairs
       when List.exists (fun (k,i) -> k<>"@id") pairs ->
       (* else no triple *)
     let () =
       match List.assoc_opt "@id" pairs with
       | Some (`String s) -> Buffer.add_string buf s
       | Some _ -> failwith "turtle_of_data: invalid @id"
       | None -> Buffer.add_string buf "[]" in
     let ln = turtle_props buf "\n    " [] pairs in
     Buffer.add_string buf " .\n";
     List.iter (turtle_node buf) (List.rev ln)
  | `Assoc _ -> () (* {} or {@id} *)
  | _ -> failwith "turtle_of_data: invalid node description"
and turtle_props buf indent ln pairs =
  let not_first = ref false in
  List.fold_left
    (fun ln (k,i) ->
     match k with
     | "@id" -> ln
     | "@type" ->
	let lt =
	  match i with
	  | `String t -> [t]
	  | `List li ->
	     List.fold_right
	       (fun i res ->
		match i with
		| `String t -> t::res
		| _ -> failwith "turtle_of_data: unexpected type")
	       li []
	  | _ -> failwith "turtle_of_data: unexpected type" in
	if lt = [] then failwith "turtle_of_data: unexpected type";
	if !not_first then Buffer.add_string buf " ;";
	not_first := true;
	Buffer.add_string buf indent;
	Buffer.add_string buf "a ";
	Buffer.add_string buf (String.concat ", " lt);
	ln
     | p ->
	if !not_first then Buffer.add_string buf " ;";
	not_first := true;
	Buffer.add_string buf indent;
	Buffer.add_string buf p;
	Buffer.add_string buf " ";
	turtle_objs buf (indent ^ "    ") ln i)
    ln pairs
and turtle_objs buf indent ln i =
  match i with
  | `List li ->
     let not_first = ref false in
     List.fold_left
       (fun ln i ->
	if !not_first then Buffer.add_string buf ", ";
	let ok, ln = turtle_obj buf indent ln i in
	not_first := !not_first || ok;
	ln)
       ln li
  | _ ->
     let ok, ln = turtle_obj buf indent ln i in
     ln
and turtle_obj buf indent ln i =
  match i with
  | `Bool b ->
     Buffer.add_string buf (if b then "true" else "false");
     true, ln
  | `Int i ->
     Buffer.add_string buf (string_of_int i);
     true, ln
  | `Float f ->
     Buffer.add_string buf (string_of_float f);
     true, ln
  | `String s ->
     Buffer.add_string buf ("\"" ^ escape_shortString s ^ "\"");
     true, ln
  | `Null -> false, ln
  | `Assoc ["@value", `String v; "@language", `String l] ->
     Buffer.add_string buf ("\"" ^ escape_shortString v ^ "\"@" ^ l);
     true, ln
  | `Assoc ["@value", `String v; "@type", `String t] ->
     Buffer.add_string buf ("\"" ^ escape_shortString v ^ "\"^^" ^ t);
     true, ln
  | `Assoc ["@list", `List li] ->
     Buffer.add_string buf "(";
     let not_first = ref false in
     let ln =
       List.fold_left
	 (fun ln elt ->
	  if !not_first then Buffer.add_string buf " ";
	  let ok, ln = turtle_obj buf indent ln elt in
	  not_first := !not_first || ok;
	  ln)
	 ln li in
     Buffer.add_string buf ")";
     true, ln
  | `Assoc pairs ->
     (match List.assoc_opt "@id" pairs with
      | Some (`String s) ->
	 Buffer.add_string buf s;
	 true, i::ln
      | Some _ -> failwith "turtle_of_data: invalid_object"
      | None ->
	 Buffer.add_string buf "[";
	 let ln = turtle_props buf indent ln pairs in
	 Buffer.add_string buf " ]";
	 true, ln)
  | `List _ -> failwith "turtle_of_data: invalid object (array)"
       
let turtle_of_data (d : data) : string =
  let buf = Buffer.create 10103 in
  List.iter
    (fun (prefix,iri) ->
     Buffer.add_string buf "@prefix ";
     Buffer.add_string buf prefix;
     Buffer.add_string buf "<";
     Buffer.add_string buf iri;
     Buffer.add_string buf "> .\n")
    ["rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
     "rdfs:", "http://www.w3.org/2000/01/rdf-schema#";
     "owl:", "http://www.w3.org/2002/07/owl#";
     "xsd:", "http://www.w3.org/2001/XMLSchema#"];
  turtle_jsonld buf d;
  Buffer.contents buf
