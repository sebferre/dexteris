(* conversion between javascript repr. and Jsoniq.item *)

open Js_of_ocaml
open Js
open Jsoniq

exception TODO
	    
let rec to_item (x : _ Js.t) : item =
  let typ = to_string (typeof x) in
  match typ with
  | "undefined" -> `Null
  | "boolean" -> `Bool (to_bool (Unsafe.coerce x))
  | "number" ->
     let f = float_of_number (Unsafe.coerce x) in
     if Float.is_integer f
     then `Int (Float.to_int f)
     else `Float f
  | "string" -> `String (to_string (Unsafe.coerce x))
  | "bigint" -> raise TODO
  | "symbol" -> raise TODO
  | "function"
  | "object" (* includes null *) ->
     if some (Unsafe.coerce x) = null then (
       Jsutils.firebug "object: null";
       `Null )
     else (
       let js_keys = to_array (object_keys x) in
       let pairs =
	 js_keys
	 |> Array.map
	      (fun js_key ->
	       to_string js_key, to_item (Unsafe.get x js_key)) in
       match pairs with
       | [|"t", _; "c", `String s; "l", _|] -> `String s
       (* seems to be the internal representation of strings by js_of_ocaml *)
       | _ ->
	  let _, is_array =
	    Array.fold_left
	      (fun (i,res) (key,_) ->
	       i+1, res && key = string_of_int i)
	      (0,true) pairs in
	  if is_array
	  then `List (Array.to_list (Array.map snd pairs))
	  else `Assoc (Array.to_list pairs)
     )
  | t -> invalid_arg ("Unexpected type of js value: " ^ t)

let to_data x = Seq.return (to_item x)
  
let rec of_item (x : item) : Unsafe.any =
  match x with
  | `Bool b -> Unsafe.inject (bool b)
  | `Int i -> Unsafe.inject i
  | `Float f -> Unsafe.inject f
  | `String s -> Unsafe.inject (string s)
  | `Null -> Unsafe.inject null
  | `Assoc pairs ->
     Unsafe.inject
       (Unsafe.obj
	  (Array.of_list
	     (List.map
		(fun (key,v) -> (key, of_item v))
		pairs)))
  | `List vals ->
     Unsafe.inject
       (array
	  (Array.of_list
	     (List.map of_item vals)))
