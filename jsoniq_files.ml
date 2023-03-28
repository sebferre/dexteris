(* conversion from and to file formats *)

open Js_of_ocaml
open Jsoniq

module Semantics = Jsoniq_semantics

module Mime =
  struct
    let text = "text/plain"
    let csv = "text/csv"
    let json = "application/json"
    let turtle = "text/turtle"
  end

(* TEXT *)

let re_newline = Regexp.regexp "[\n\r]+"
let data_of_text (contents : string) : data =
  let lines = Regexp.split re_newline contents in
  Seq.from_list lines
  |> Seq.map (fun s -> `String s)

let text_of_data (d : data) : string =
  let buf = Buffer.create 10103 in
  d
  |> Seq.iter
       (function
	 | `String s ->
	    Buffer.add_string buf s;
	    Buffer.add_char buf '\n'
	 | _ -> failwith "text_of_data: unexpected value (not a string)");
  Buffer.contents buf
  
    
(* JSON *)
    
let rec objectify_data (d : data) : data =
  match Seq.take 2 d with
  | [`List li], None -> (* d has a single elt *)
     objectify_data (Seq.from_list li)
  | _ ->
     d
     |> Seq.map
	  (fun elt ->
	   match elt with
	   | `Assoc _ -> elt
	   | _ -> `Assoc [("?",elt)])
       
let data_of_json ?fname (contents : string) : data =
  let str = Yojson.Basic.stream_from_string ?fname contents in
  objectify_data (Seq.from_stream str)

let json_of_data (d : data) : string =
  let json =
    match Seq.to_list d with
    | [] -> `Null
    | [elt] -> elt
    | elts -> `List elts in
  Yojson.Basic.to_string json

			 
(* CSV *)
		 
let item_of_csv_value (s : string) : item =
  if s = "" then `Null
  else if s = "true" then `Bool true
  else if s = "false" then `Bool false
  else try `Int (int_of_string s)
       with _ ->
	    try `Float (float_of_string s)
	    with _ -> `String s

let data_of_csv ~(has_header : bool) (contents : string) : data =
  let get_ch_header contents sep =
    let ch = Csv.of_string ~separator:sep ~fix:true contents in
    let first_row = Csv.next ch in
    if has_header
    then
      let header = first_row in
      ch, header
    else
      let header = List.mapi (fun i _ -> "col" ^ string_of_int i) first_row in
      let ch = (* resetting ch to keep first row as data *)
	Csv.close_in ch;
	Csv.of_string ~separator:sep ~fix:true contents in
      ch, header in
  let rec aux ~header ch =
    let def_row_seq1 = lazy (Csv.next ch, aux ~header ch) in
    (fun () ->
     try
       let row, seq1 = Lazy.force def_row_seq1 in
       if List.for_all (fun s -> s="") row
       then seq1 ()
       else
	 let l = Csv.combine ~header row in
	 let pairs =
	   List.fold_right
	     (fun (x,s) pairs ->
	      let item = item_of_csv_value s in
	      match List.assoc_opt x pairs with
	      | None ->
		 (x,item)::pairs
	      | Some item0 ->
		 if s=""
		 then pairs
		 else
		   let item =
		     match item0 with
		     | `Null -> item
		     | `List li -> `List (item :: li)
		     | _ -> `List [item; item0] in
		   (x,item) :: List.remove_assoc x pairs)
	     l [] in
	 Seq.Cons (`Assoc pairs, seq1)
     with
     | End_of_file ->
	Csv.close_in ch;
	Seq.Nil)
  in
  let ch, header = get_ch_header contents ',' in
  let ch, header =
    if List.length header <= 1
    then get_ch_header contents ';'
    else ch, header in
  aux ~header ch

let csv_of_data (d : data) : string =
  let buf = Buffer.create 10103 in
  let ch = Csv.to_buffer buf in
  let output_item_list li =
    let ls =
      List.map 
	(function
	  | `Bool b -> if b then "true" else "false"
	  | `Int i -> string_of_int i
	  | `Float f -> string_of_float f
	  | `String s -> s
	  | `Null -> ""
	  | `Assoc _ -> failwith "csv_of_data: unexpected value (object)"
	  | `List _ -> failwith "csv_of_data: unexpected value (array)")
	li in
    Csv.output_record ch ls
  in
  let first_row = ref true in
  d
  |> Seq.iter
       (fun row ->
	let () =
	  match  row with
	  | `List li -> output_item_list li
	  | `Assoc pairs ->
	     let header, li = List.split pairs in
	     if !first_row then Csv.output_record ch header;
	     output_item_list li
	  | _ -> () in
	first_row := false);
  Csv.close_out ch;
  Buffer.contents buf

      
(*let data_of_file (filename : string) (contents : string) : data =
  match Filename.extension filename with
  | ".json" -> data_of_json ~fname:filename contents
  | ".csv" -> data_of_csv ~has_header:true contents
  | _ -> failwith "Unexpected file extension (should be one of .json .csv)"*)

		  
(* deprecated *)
(*let json_of_extent (ext : Semantics.extent) : Yojson.Basic.t =
  let pack (d : data) : item = `List (Seq.to_list d) in
  let rev_elts =
    Seq.fold_left
      (fun res binding ->
       match binding with
       | [(k,d)] when k = Semantics.field_focus -> (pack d)::res
       | _ ->
	  let pairs =
	    List.map
	      (fun (k,d) -> (k, pack d))
	      binding in
	  `Assoc pairs :: res)
      [] ext.Semantics.bindings in
  match rev_elts with
  | [elt] -> elt
  | _ -> `List (List.rev rev_elts)*)

let json_seq_of_extent (ext : Semantics.extent) : Yojson.Basic.t Seq.t =
  let json_of_data d =
    match Seq.to_list d with
    | [] -> `Null
    | [elt] -> elt
    | elts -> `List elts in
  let bindings = ext.Semantics.bindings in
  match Seq.take 2 bindings with
  | [ [(k,d)] ], None (* only one binding *)
       when k = ext.focus_var ->
     d
  | _ ->
     bindings
     |> Seq.map (* generating a sequence of JSON values *)
	  (function
	    | [(k,d)] when k = ext.focus_var ->
	       json_of_data d
	    | binding ->
	       let pairs =
		 List.map
		   (fun (k,d) -> (k, json_of_data d))
		   binding in
	       `Assoc pairs)
	  
let mime_contents_of_extent (ext : Semantics.extent) : string * string =
  let json_seq = json_seq_of_extent ext in
  let contents =
    let buf = Buffer.create 10103 in
    json_seq
    |> Seq.iter
	 (fun json ->
	  Buffer.add_string buf (Yojson.Basic.to_string json);
	  Buffer.add_char buf '\n');
    Buffer.contents buf in
  let mime = "application/x-json-stream" in
  mime, contents

let make_data_file ?(mime = "text/plain") contents : item =
  `Assoc ["mime", `String mime;
	  "contents", `String contents]

let get_data_file_opt (d : data) : (string * string) option =
  match Seq.take 2 d with
  | [`Assoc ["mime", `String mime;
	     "contents", `String contents]], None ->
     Some (mime, contents)
  | _ -> None
