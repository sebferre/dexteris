(* conversion from and to file formats *)

open Jsoniq

module Semantics = Jsoniq_semantics

let rec objectify_data (d : data) : data =
  d
  |> Seq.flat_map
       (function
	 | `List li -> objectify_data (Seq.from_list li)
	 | (`Assoc _ as i) -> Seq.return i
	 | atom -> Seq.return (`Assoc [("?",atom)]))
       
let data_of_json ?fname (contents : string) : data =
  let str = Yojson.Basic.stream_from_string ?fname contents in
  objectify_data (Seq.from_stream str)

let item_of_csv_value (s : string) : item =
  if s = "" then `Null
  else if s = "true" then `Bool true
  else if s = "false" then `Bool false
  else try `Int (int_of_string s)
       with _ ->
	    try `Float (float_of_string s)
	    with _ -> `String s

let data_of_csv (contents : string) : data =
  let get_ch_header contents sep =
    let ch = Csv.of_string ~separator:sep contents in
    let header = Csv.next ch in
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

let data_of_file (filename : string) (contents : string) : data =
  match Filename.extension filename with
  | ".json" -> data_of_json ~fname:filename contents
  | ".csv" -> data_of_csv contents
  | _ -> failwith "Unexpected file extension (should be one of .json .csv)"

		  
			      
let json_of_extent (ext : Semantics.extent) : Yojson.Basic.t =
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
  | _ -> `List (List.rev rev_elts)
		  
let mime_contents_of_extent (ext : Semantics.extent) : string * string =
  let json = json_of_extent ext in
  let contents = Yojson.Basic.to_string json in
  let mime = "application/json" in
  mime, contents
