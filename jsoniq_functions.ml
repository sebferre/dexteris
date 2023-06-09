
open Js_of_ocaml
open Syntax
open Jsoniq
open Jsoniq_syntax
open Jsoniq_semantics

class virtual func (name : string) =
object
  method name = name
  method virtual arity : int
  method path : string list = []
  method virtual syntax : syn list -> syn
  method virtual command : arg:int (* arg pos in [1,arity] *) -> string
  method virtual command_score : arg:int -> string -> float
  method virtual typecheck : int (* arg pos *) -> bool (* multiple items *) -> TypSet.t (* arg types *) -> TypSet.t (* expected return types *) -> bool
  method virtual apply : data list -> data
end

exception Unknown_function of string
	    
let library =
  object (self)
    val ht : (string, func) Hashtbl.t = Hashtbl.create 101
    method register (func : func) : unit =
      Hashtbl.add ht func#name func
    method lookup (name : string) : func (* raises Unknown_function *) =
      try Hashtbl.find ht name
      with Not_found -> raise (Unknown_function name)
    method fold : 'a. ('a -> func -> 'a) -> 'a -> 'a =
      fun f init ->
      Hashtbl.fold (fun name func res -> f res func) ht init
    method iter f =
      Hashtbl.iter (fun name func -> f func) ht
    method syntax func args = (self#lookup func)#syntax args
    method apply func args = (self#lookup func)#apply args
  end
    
exception Invalid_arity

(* utilities *)

let string_of_atomic_item : item -> string option =
  function
  | `Null -> Some ""
  | `Bool b -> Some (string_of_bool b)
  | `Int i -> Some (string_of_int i)
  | `Intlit s -> Some s
  | `Float f -> Some (string_of_float f)
  | `String s -> Some s
  | `Assoc _ -> None
  | `List _ -> None
  | `Tuple _ -> None
  | `Variant _ -> None

let item_of_item_list : item list -> item = function
  | [] -> `Null
  | [i] -> i
  | li -> `List li
	    
let bind_1item (f : item -> data) (ld : data list) =
  match ld with
  | [d] ->
     ( match item_of_data d with
       | None -> Seq.empty
       | Some i -> f i )
  | _ -> raise Invalid_arity
	       
let bind_2items (f : item * item -> data) (ld : data list) =
  match ld with
  | [d1;d2] ->
     ( match item_of_data d1, item_of_data d2 with
       | None, _
       | _, None -> Seq.empty
       | Some i1, Some i2 -> f (i1,i2) )
  | _ -> raise Invalid_arity

let bind_3items (f : item * item * item -> data) (ld : data list) =
  match ld with
  | [d1;d2; d3] ->
     ( match item_of_data d1, item_of_data d2, item_of_data d3 with
       | Some i1, Some i2, Some i3 -> f (i1,i2,i3)
       | _ -> Seq.empty )
  | _ -> raise Invalid_arity

class virtual classic name arity display_name =
object
  inherit func name
  method arity = arity
  method syntax lxml =
    Jsoniq_syntax.syn_func display_name lxml
  method command ~arg =
    Jsoniq_command.command_of_func display_name arity arg
  method command_score ~arg:pos cmd =
    Jsoniq_command.score_of_func display_name arity pos cmd
end

class virtual prefix name (op : string) =
object
  inherit func name
  method arity = 1
  method syntax = function
    | [xml1] -> Word (`Func op) :: xml1
    | _ -> raise Invalid_arity
  method command ~arg =
    assert (arg=1);
    op ^ " _"
  method command_score ~arg cmd =
    assert (arg=1);
    Scanf.sscanf cmd
      (Scanf.format_from_string (op ^ " _") "")
      1.0
end
	       
class virtual infix name (op : string) =
object
  inherit func name
  method arity = 2
  method syntax = function
    | [xml1; xml2] -> xml1 @ Word (`Func op) :: xml2
    | _ -> raise Invalid_arity
  method command ~arg =
    if arg=1 then op
    else if arg=2 then "? " ^ op
    else assert false
  method command_score ~arg cmd =
    if arg=1 then
      Scanf.sscanf cmd
        (Scanf.format_from_string ("%_[_] " ^ op ^ " %_[?]%!") "")
        1.
    else if arg=2 then
      Scanf.sscanf cmd
        (Scanf.format_from_string ("? " ^ op ^ " %_[_]%!") "")
        1.
    else assert false
end

class virtual mixfix name arity (wds : string list) =
object
  inherit func name
  method arity = arity
  method syntax lxml =
    let rec aux = function
      | [], [] -> []
      | wd::wds, xml::lxml ->
	 Word (`Func wd) :: xml @ aux (wds,lxml)
      | [], lxml -> List.concat lxml
      | wds, [] -> List.map (fun wd -> Word (`Func wd)) wds
    in
    aux (wds,lxml)
  method command ~arg =
    let rec aux pos wds =
      let prefix, wds1 =
        match wds with
        | [] -> "", []
        | wd::wds1 -> wd ^ " ", wds1 in
      if pos <= arity
      then
        prefix
        ^ (if arg <= pos then ""
           else "? ")
        ^ aux (pos+1) wds1
      else String.concat " " wds
    in
    aux 1 wds
  method command_score ~arg cmd =
    let rec aux_fmt pos wds =
      let prefix, wds1 =
        match wds with
        | [] -> "", []
        | wd::wds1 -> (wd ^ " "), wds1 in
      if pos <= arity
      then
        prefix
        ^ (if pos<arg then "? "
           else if pos=arg then "%_[_] "
           else "%_[?] ")
        ^ aux_fmt (pos+1) wds1
      else String.concat " " wds ^ "%!"
    in
    let fmt = aux_fmt 1 wds in
    Scanf.sscanf cmd (Scanf.format_from_string fmt "") 1.
(*
  method command ~arg = (* TODO: specialize *)
    let args =
      List.init arity (fun pos -> if pos+1 = arg then "_" else "?") in
    Printf.sprintf "%s(%s)" name (String.concat ", " args)
  method command_score ~arg:pos cmd = (* TODO: specialize *)
    Scanf.sscanf cmd "%s@(%[ ,?_]"
      (fun s1 s2 ->
        if s1=name
        then
          let args = String.split_on_char ',' s2 in
          let args = List.rev_map String.trim args in
          let args = match args with ""::l -> List.rev l | l -> List.rev l in
          let next_p, ok =
            List.fold_left
              (fun (p,ok) arg ->
                p+1, ok && (p=pos) = (arg="_"))
              (1,true) args in
          if ok && pos <= next_p && next_p - 1 <= arity
          then 1.
          else 0.
        else 0.)
 *)
end	  
  
class typecheck_simple ?(single = true) (ar_f_ins : 't list array) (f_outs : 't list) =
object
  method typecheck pos multiple_items ins outs =
    let f_ins = try ar_f_ins.(pos-1) with _ -> list_all_typs in
    not (single && multiple_items)
    && List.exists (fun t -> TypSet.mem t ins) f_ins
       (* there must be an input type that is expected by the function *)
    && List.exists (fun t -> TypSet.mem t outs) f_outs
       (* and some possible output type must be allowed *)
end
  
(* definition of functions *)

class comparator (name : string) (pred : int -> bool) (op : string) =
  object
    inherit infix name op
    method path = ["comparison"]
    inherit typecheck_simple [|list_all_typs; list_all_typs|] [`Bool]
    method apply = function
      | [d1;d2] ->
	 let c : int = compare_data d1 d2 in
	 Seq.return (`Bool (pred c))
      | _ -> raise Invalid_arity
  end

let _ =
  library#register (new comparator "eq" (fun c -> c=0) "==");
  library#register (new comparator "ne" (fun c -> c<>0) "!=");
  library#register (new comparator "le" (fun c -> c<=0) "<=");
  library#register (new comparator "lt" (fun c -> c<0) "<");
  library#register (new comparator "ge" (fun c -> c>=0) ">=");
  library#register (new comparator "gt" (fun c -> c>0) ">")

class binop_num (name : string)
		(f_int : int -> int -> int)
		(f_float : float -> float -> float)
		(op : string) =
object
  inherit infix name op
  method path = ["numbers"]
  inherit typecheck_simple [| [`Int; `Float]; [`Int; `Float] |] [`Int; `Float]
  method apply =
    bind_2items
      (function
	| `Int n1, `Int n2 -> Seq.return (`Int (f_int n1 n2))
	| `Int n1, `Float f2 -> Seq.return (`Float (f_float (float n1) f2))
	| `Float f1, `Int n2 -> Seq.return (`Float (f_float f1 (float n2)))
	| `Float f1, `Float f2 -> Seq.return (`Float (f_float f1 f2))
	| _ -> Seq.empty)
end

let _ =
  library#register (new binop_num "plus" (+) (+.) "+");
  library#register (new binop_num "minus" (-) (-.) "-");
  library#register (new binop_num "times" ( * ) ( *. ) "*");
  
class binop_int (name : string)
		(f_int : int -> int -> int)
		(op : string) =
object
  inherit infix name op
  method path = ["numbers"]
  inherit typecheck_simple [| [`Int]; [`Int] |] [`Int]
  method apply =
    bind_2items
      (function
	| `Int n1, `Int n2 -> Seq.return (`Int (f_int n1 n2))
	| _ -> Seq.empty)
end

let _ =
  library#register (new binop_int "idiv" (/) "div");
  library#register (new binop_int "mod" (mod) "mod")
		   
class binop_float (name : string)
		  (f_float : float -> float -> float)
		  (op : string) =
object
  inherit infix name op
  method path = ["numbers"]
  inherit typecheck_simple [| [`Float]; [`Float] |] [`Float]
  method apply =
    bind_2items
      (function
	| `Int n1, `Int n2 -> Seq.return (`Float (f_float (float n1) (float n2)))
	| `Int n1, `Float f2 -> Seq.return (`Float (f_float (float n1) f2))
	| `Float f1, `Int n2 -> Seq.return (`Float (f_float f1 (float n2)))
	| `Float f1, `Float f2 -> Seq.return (`Float (f_float f1 f2))
	| _ -> Seq.empty)
end

let _ =
  library#register (new binop_float "div" (/.) "/")


class unop_num name (f_int : int -> int) (f_float : float -> float) op =
object
  inherit prefix name op
  method path = ["numbers"]
  inherit typecheck_simple [| [`Int; `Float] |] [`Int; `Float]
  method apply =
    bind_1item
      (function
	| `Int n -> Seq.return (`Int (f_int n))
	| `Float f -> Seq.return (`Float (f_float f))
	| _ -> Seq.empty)
end

let _ =
  library#register (new unop_num "neg" (fun n -> -n) (fun f -> -. f) "-")

let _ =
  let path = ["numbers"] in
  library#register
    (object
	inherit classic "abs" 1 "abs"
	method path = path
	inherit typecheck_simple [| [`Int; `Float] |] [`Int; `Float]
	method apply =
	  bind_1item
	    (function
	      | `Int n -> Seq.return (`Int (abs n))
	      | `Float f -> Seq.return (`Float (abs_float f))
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit infix "range" "to"
	method path = path
	inherit typecheck_simple [| [`Int]; [`Int] |] [`Int]
	method apply =
	  bind_2items
	    (function
	      | `Int a, `Int b -> Seq.map (fun n -> `Int n) (Seq.range a b)
	      | _ -> Seq.empty)
      end)
		   
let _ =
  let path = ["strings"] in
  library#register
    (object
	inherit infix "stringConcat" "||"
	method path = []
	inherit typecheck_simple [| [`Bool; `Int; `Float; `String]; [`Bool; `Int; `Float; `String] |] [`String]
	method apply =
	  bind_2items
	    (fun (i1,i2) ->
	     match string_of_atomic_item i1, string_of_atomic_item i2 with
	     | Some s1, Some s2 -> Seq.return (`String (s1 ^ s2))
	     | _ -> Seq.empty )
      end);
  library#register
    (object
	inherit classic "stringLength" 1 "length"
	method path = path
	inherit typecheck_simple [| [`String] |] [`Int]
	method apply =
	  bind_1item
	    (function
	      | `String s ->
		 let len = (Js.string s)##.length in
		 Seq.return (`Int len)
	      | _ -> Seq.empty)
      end);
  
  library#register
    (object
	inherit classic "substr" 3 "substr"
	method path = path
	inherit typecheck_simple [| [`String]; [`Int]; [`Int] |] [`String]
	method apply =
	  bind_3items
	    (function
	      | `String str, `Int start, `Int len ->
		 let res = Js.to_string ((Js.string str)##substring start len) in
		 Seq.return (`String res)
	      | _ -> Seq.empty)
      end);

  library#register
    (object
	inherit classic "uppercase" 1 "uppercase"
	method path = path
	inherit typecheck_simple [| [`String] |] [`String]
	method apply =
	  bind_1item
	    (function
	      | `String str ->
		 let res = Js.to_string ((Js.string str)##toUpperCase) in
		 Seq.return (`String res)
	      | _ -> Seq.empty)
      end);
  
  library#register
    (object
	inherit classic "lowercase" 1 "lowercase"
	method path = path
	inherit typecheck_simple [| [`String] |] [`String]
	method apply =
	  bind_1item
	    (function
	      | `String str ->
		 let res = Js.to_string ((Js.string str)##toLowerCase) in
		 Seq.return (`String res)
	      | _ -> Seq.empty)
      end);
  
  library#register
    (object
	inherit infix "stringMatch" "matches"
	method path = path
	inherit typecheck_simple [| [`String]; [`String] |] [`Bool]
	method apply =
	  bind_2items
	    (function
	      | `String str, `String re ->
		 ( match Regexp.string_match (Regexp.regexp re) str 0 with
		   | None -> Seq.return (`Bool false)
		   | Some _ -> Seq.return (`Bool true) )
	      | _ -> Seq.empty)
      end);
  
  library#register
    (object
	inherit mixfix "split" 2 ["split"; "by"]
	method path = path
	inherit typecheck_simple [| [`String]; [`String] |] [`String]
	method apply =
	  bind_2items
	    (function
	      | `String str, `String re_sep ->
		 let ls =
		   Regexp.split
		     (Regexp.regexp re_sep)
		     str in
		 Seq.from_list
		   (List.map
		      (fun s -> `String s)
		      ls)
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit mixfix "replace" 3 ["in"; "replace"; "by"]
	method path = path
	inherit typecheck_simple [| [`String]; [`String]; [`String] |] [`String]
	method apply =
	  bind_3items
	    (function
	      | `String str, `String re, `String by ->
		 let res = Regexp.global_replace (Regexp.regexp re) str by in
		 Seq.return (`String res)
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit classic "URLencode" 1 "URLencode"
	method path = path
	inherit typecheck_simple [| [`String] |] [`String]
	method apply =
	  bind_1item
	    (function
	      | `String str -> Seq.return (`String (Url.urlencode str))
	      | _ -> Seq.empty)
      end)

let _ =
  let path = ["conversion"] in
  library#register
    (object
	inherit classic "toString" 1 "string"
	method path = path
	inherit typecheck_simple [|list_all_typs|] [`String]
	method apply =
	  bind_1item
	    (fun i -> Seq.return (`String (Yojson.Safe.to_string ~std:true i)))
      end);
  library#register
    (object
	inherit classic "toBool" 1 "bool"
	method path = path
	inherit typecheck_simple [|list_all_typs|] [`Bool]
	method apply =
	  bind_1item
	    (function
	      | `Bool b -> Seq.return (`Bool b)
	      | `Int n -> Seq.return (`Bool (n <> 0))
              | `Intlit s -> Seq.return (`Bool true)
	      | `Float f -> Seq.return (`Bool (f <> 0.))
	      | `String s ->
		 if s="true" || s="1" then Seq.return (`Bool true)
		 else if s="false" || s="0" then Seq.return (`Bool false)
		 else Seq.empty
	      | `Null -> Seq.return (`Bool false)
	      | `List li -> Seq.return (`Bool (li<>[]))
	      | `Assoc pairs -> Seq.return (`Bool (pairs<>[]))
	      | `Tuple li -> Seq.return (`Bool (li<>[]))
              | `Variant _ -> Seq.return (`Bool true))
      end);
  library#register
    (object
	inherit classic "toInt" 1 "int"
	method path = path
	inherit typecheck_simple [|[`Bool; `Float; `String]|] [`Int]
	method apply =
	  bind_1item
	    (function
	      | `Bool b -> Seq.return (`Int (if b then 1 else 0))
	      | `Int n -> Seq.return (`Int n)
	      | `Float f -> Seq.return (`Int (int_of_float f))
	      | `String s ->
		 (try Seq.return (`Int (int_of_string s))
		  with _ -> Seq.empty)
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit classic "toFloat" 1 "float"
	method path = path
	inherit typecheck_simple [| [`Bool; `Int; `String] |] [`Float]
	method apply =
	  bind_1item
	    (function
	      | `Bool b -> Seq.return (`Float (if b then 1. else 0.))
	      | `Int n -> Seq.return (`Float (float n))
	      | `Float f -> Seq.return (`Float f)
	      | `String s ->
		 (try Seq.return (`Float (float_of_string s))
		  with _ -> Seq.empty)
	      | _ -> Seq.empty)
      end)

				 
    
let _ =
  library#register
    (object
	inherit classic "arrayLength" 1 "size"
	method path = ["arrays"]
	inherit typecheck_simple [| [`Array] |] [`Int]
	method apply =
	  bind_1item
	    (function
	      | `List l -> Seq.return (`Int (List.length l))
	      | _ -> Seq.empty)
      end);

  library#register
    (object
	inherit classic "objectKeys" 1 "keys"
	method path = ["objects"]
	inherit typecheck_simple [| [`Object] |] [`String]
	method apply =
	  bind_1item
	    (function
	      | `Assoc pairs -> Seq.map (fun (k,_) -> `String k) (Seq.from_list pairs)
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit classic "objectItems" 1 "items"
	method path = ["objects"]
	inherit typecheck_simple [| [`Object] |] [`Object]
	method apply =
	  bind_1item
	    (function
	      | `Assoc pairs ->
		 Seq.map (fun (k,i) ->
			  `Assoc ["$key", `String k;
				  "$value", i])
			 (Seq.from_list pairs)
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit mixfix "objectOfArray" 2 ["{"; "with keys"; "}"]
	method path = ["objects"]
	inherit typecheck_simple [| [`Array]; [`Array] |] [`Object]
	method apply =
	  let rec aux = function
	    | _, [] -> []
	    | [], _ -> failwith "there are not enough keys"
	    | `String k :: lk, v::lv -> (k,v)::aux (lk,lv)
	    | _ -> failwith "some key is not a string"
	  in
	  bind_2items
	    (function
	      | `List lv, `List lk ->
		 let pairs = aux (lk,lv) in
		 Seq.return (`Assoc pairs)
	      | _ -> failwith "expects two lists: a list of values and a list of keys")
      end)
   

class aggreg name func (g_ins : typ list) (g_outs : typ list) (g : data -> data) =
object
  inherit classic name 1 func
  method path = ["aggregation"]
  inherit typecheck_simple ~single:false [|g_ins|] g_outs
  method apply = function
    | [d1] -> g d1
    | _ -> raise Invalid_arity
end

let string_of_data ?(sep = "") (d : data) : string =
  let buf = Buffer.create 1001 in
  let first = ref true in
  Seq.iter
    (fun i ->
     if !first
     then first := false
     else if sep <> "" then Buffer.add_string buf sep;
     Buffer.add_string buf (item_as_string i))
    d;
  Buffer.contents buf
  
let _ =
  library#register
    (new aggreg "count" "count" list_all_typs [`Int]
	 (fun d -> Seq.return (`Int (Seq.length d))));
  library#register
    (new aggreg "min" "min" list_all_typs list_all_typs
	 (fun d ->
	  Seq.case
	    ~nil:(fun () -> Seq.empty)
	    ~cons:(fun i next ->
		   Seq.return
		     (Seq.fold_left
			(fun res j ->
			 if compare_item i j <= 0
			 then i
			 else j)
			i next))
	    d));
  library#register
    (new aggreg "max" "max" list_all_typs list_all_typs
	 (fun d ->
	  Seq.case
	    ~nil:(fun () -> Seq.empty)
	    ~cons:(fun i next ->
		   Seq.return
		     (Seq.fold_left
			(fun res j ->
			 if compare_item i j >= 0
			 then i
			 else j)
			i next))
	    d));
  library#register
    (new aggreg "some" "some" [`Bool] [`Bool]
	 (fun d ->
	  Seq.return
	    (`Bool
	      (Seq.fold_left
		 (fun res ->
		  function
		  | `Bool b -> res || b
		  | _ -> res)
		 false d))));
  library#register
    (new aggreg "all" "all" [`Bool] [`Bool]
	 (fun d ->
	  Seq.return
	    (`Bool
	      (Seq.fold_left
		 (fun res ->
		  function
		  | `Bool b -> res && b
		  | _ -> res)
		 true d))));
  library#register
    (new aggreg "sum" "sum" [`Bool; `Int; `Float] [`Float]
	 (fun d ->
	  Seq.return
	    (`Float
	      (Seq.fold_left
		 (fun res ->
		  function
		  | `Bool b -> if b then res +. 1. else res
		  | `Int n -> res +. float n
		  | `Float f -> res +. f
		  | _ -> res)
		 0. d))));
  library#register
    (new aggreg "prod" "prod" [`Bool; `Int; `Float] [`Float]
	 (fun d ->
	  Seq.return
	    (`Float
	      (Seq.fold_left
		 (fun res ->
		  function
		  | `Bool b -> if b then res *. 1. else res
		  | `Int n -> res *. float n
		  | `Float f -> res *. f
		  | _ -> res)
		 1. d))));
  library#register
    (new aggreg "avg" "avg" [`Bool; `Int; `Float] [`Float]
	 (fun d ->
	  let count, sum =
	    Seq.fold_left
	      (fun (c,s) ->
	       function
	       | `Bool b -> c+1, (if b then s +. 1. else s)
	       | `Int n -> c+1, s +. float n
	       | `Float f -> c+1, s +. f
	       | _ -> c, s)
	      (0,0.) d in
	  Seq.return (`Float (sum /. float count))));
  library#register
    (new aggreg "reverse" "reverse" list_all_typs list_all_typs
       (fun d -> Seq.from_list (Seq.to_rev_list d)));
  library#register
    (new aggreg "unique" "unique" list_all_typs list_all_typs
       (fun d -> Seq.unique d));
  library#register
    (new aggreg "sort" "sort" list_all_typs list_all_typs
       (fun d ->
         Seq.from_list
           (List.sort compare_item
              (Seq.to_rev_list d))));
  library#register
    (new aggreg "concat" "concat" [`Bool; `Int; `Float; `String] [`String]
	 (fun d -> Seq.return (`String (string_of_data d))));
  library#register
    (object
        inherit mixfix "concatSep" 2 ["concat"; "with separator"]
	method path = ["aggregation"]
	inherit typecheck_simple ~single:false [| [`Bool; `Int; `Float; `String]; [`String] |] [`String]
	method apply = function
	  | [d1; d2] ->
	     let sep =
	       match item_of_data d2 with
	       | Some (`String s) -> s
	       | _ -> "" in
	     Seq.return (`String (string_of_data ~sep d1))
	  | _ -> Seq.empty
      end)


let _ =
  let path = ["files"] in
  library#register
    (object
	inherit classic "parseJSON" 1 "parseJSON"
	method path = path
	inherit typecheck_simple [| [`String] |] list_all_typs
	method apply =
	  bind_1item
	    (function
	      | `String contents -> Jsoniq_files.data_of_json contents
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit classic "parseText" 1 "parseText"
	method path = path
	inherit typecheck_simple [| [`String] |] [`Object]
	method apply =
	  bind_1item
	    (function
	      | `String contents -> Jsoniq_files.data_of_text contents
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit classic "parseCSV" 1 "parseCSV"
	method path = path
	inherit typecheck_simple [| [`String] |] [`Object]
	method apply =
	  bind_1item
	    (function
	      | `String contents -> Jsoniq_files.data_of_csv ~has_header:true contents
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit mixfix "parseCSVnoHeader" 1 ["parseCSV("; ") without header"]
	method path = path
	inherit typecheck_simple [| [`String] |] [`Object]
	method apply =
	  bind_1item
	    (function
	      | `String contents -> Jsoniq_files.data_of_csv ~has_header:false contents
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit classic "printJSON" 1 "printJSON"
	method path = path
	inherit typecheck_simple ~single:false [|list_all_typs|] [`Object]
	method apply =
	  function
	  | [d] ->
	     let contents = Jsoniq_files.json_of_data d in
	     Seq.return
	       Jsoniq_files.(make_data_file ~mime:Mime.json contents)
	  | _ -> Seq.empty
      end);
  library#register
    (object
	inherit classic "printText" 1 "printText"
	method path = path
	inherit typecheck_simple ~single:false [|[`String]|] [`Object]
	method apply : data list -> data =
	  function
	  | [d] ->
	     let contents = Jsoniq_files.text_of_data d in
	     Seq.return
	       Jsoniq_files.(make_data_file ~mime:Mime.text contents)
	  | _ -> Seq.empty
      end);
  library#register
    (object
	inherit classic "printCSV" 1 "printCSV"
	method path = path
	inherit typecheck_simple ~single:false [| [`Object; `Array] |] [`Object]
	method apply =
	  function
	  | [d] ->
	     let contents = Jsoniq_files.csv_of_data d in
	     Seq.return
	       Jsoniq_files.(make_data_file ~mime:Mime.csv contents)
	  | _ -> Seq.empty
      end)


let _ =
  let path = ["RDF"] in
  library#register
    (object
	inherit mixfix "RDFdescr" 3 ["Descr("; "a"; ";"; ")"]
	method path = path
	inherit typecheck_simple ~single:false [| [`String]; [`String]; [`Object] |] [`Object]
	method apply = function
	  | [d1;d2;d3] -> (* ids, types, properties *)
	     let d1 =
	       if Seq.is_empty d1
	       then Seq.return (`String "")
	       else d1 in
	     let ltypes =
	       item_of_item_list
		 (d2
		  |> Seq.filter (function `String s2 -> true | _ -> false)
		  |> Seq.to_list) in
	     let pairs =
	       match item_of_data d3 with
	       | Some (`Assoc pairs) -> pairs
	       | _ -> [] in
	     d1
	     |> Seq.flat_map
		  (function
		    | `String s1 as i1 ->
		       let pairs = if ltypes = `Null then pairs else ("@type",ltypes)::pairs in
		       let pairs = if s1 = "" then pairs else ("@id",i1)::pairs in
		       Seq.return (`Assoc pairs)
		    | _ -> Seq.empty)
	  | _ -> Seq.empty
      end);
  library#register
    (object
	inherit classic "RDFlist" 1 "List"
	method path = path
	inherit typecheck_simple ~single:false [|list_all_typs|] [`Object]
	method apply = function
	  | [d1] -> Seq.return (`Assoc ["@list", `List (Seq.to_list d1)])
	  | _ -> Seq.empty
      end);
  library#register
    (object
	inherit infix "PlainLiteral" "@"
	method path = path
	inherit typecheck_simple [| [`String]; [`String] |] [`Object]
	method apply =
	  bind_2items
	    (function
	      | `String str, `String lang ->
		 if lang = ""
		 then Seq.return (`String str)
		 else
		   let pairs =
		     [ "@value", `String str;
		       "@language", `String lang ] in
		   Seq.return (`Assoc pairs)
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit infix "TypedLiteral" "^^"
	method path = path
	inherit typecheck_simple [| [`String]; [`String] |] [`Object]
	method apply =
	  bind_2items
	    (function
	      | `String str, `String dt ->
		 if dt = ""
		 then Seq.return (`String str)
		 else
		   let pairs =
		     [ "@value", `String str;
		       "@type", `String dt ] in
		   Seq.return (`Assoc pairs)
	      | _ -> Seq.empty)
      end);
  library#register
    (object
	inherit classic "printTurtle" 1 "printTurtle"
	method path = path
	inherit typecheck_simple ~single:false [| [`Object] |] [`Object]
	method apply = function
	  | [d] ->
	     let contents = Rdf.turtle_of_data d in
	     Seq.return
	       Jsoniq_files.(make_data_file ~mime:Mime.turtle contents)
	  | _ -> Seq.empty
      end)
	  
