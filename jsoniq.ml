
class ['a,'b] dico =
object
  val ht : ('a, 'b list ref) Hashtbl.t = Hashtbl.create 13
  method add (k : 'a) (v : 'b) : unit =
    let ref_l =
      try Hashtbl.find ht k
      with Not_found ->
	let ref_l = ref [] in
	Hashtbl.add ht k ref_l;
	ref_l in
    ref_l := v :: !ref_l
  method fold : 'c. ('c -> 'a -> 'b list -> 'c) -> 'c -> 'c =
    fun f init ->
    Hashtbl.fold
      (fun k ref_l acc -> f acc k !ref_l)
      ht init
end

let result_bind_list (f : 'a -> ('b,'e) Result.result) (lx : 'a list) : ('b list,'e) Result.result =
  List.fold_right
    (fun x res ->
     Result.bind res (fun ly -> Result.bind (f x) (fun y -> Result.Ok (y::ly))))
    lx (Ok [])

let rec list_sort (cmp : 'a -> 'a -> int) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::r ->
     let ll, rr = list_partition cmp x r [] [] in
     list_sort cmp ll @ x :: list_sort cmp rr
and list_partition cmp x r ll rr =
  match r with
  | [] -> ll, rr
  | y::r1 ->
     if cmp x y > 0
     then list_partition cmp x r1 (y::ll) rr
     else list_partition cmp x r1 ll (y::rr)
    
(* ======================================================= *)

module Seq = Myseq
	       
exception TODO
    
(*type item = (* JSON *)
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Null
  | Object of (string * item) list
  | Array of item list*)

type item = Yojson.Safe.t
(* Basic = [`Null | `Bool of bool | `Int of int | `Float of float | `String of string | `Assoc of (string * item) list | `List of item list ] *)
(* Safe = Basic + [`Intlit of string | `Tuple of t list | `Variant of string * t option *)

(* for deriving yojson on items *)
let item_to_yojson : item -> Yojson.Safe.t = fun i -> i
let item_of_yojson : Yojson.Safe.t -> (item,string) Result.t = fun i -> Result.Ok i
          
type data = item Seq.t
		      
type var = string [@@deriving yojson]
type order = DESC | ASC [@@deriving yojson]

let asc = "ascending"
let desc = "descending"
let order_strings = [asc; desc]
let string_of_order = function
  | ASC -> asc
  | DESC -> desc
let order_of_string s =
  if s = asc then ASC
  else DESC

type binder =
  | Var of var
  | Fields [@@deriving yojson]
	 
type expr =
  | S of string
  | Item of item
  | FileString of string * string (* filename, contents *)
  | Empty
  | Concat of expr list
  | Flower of flower (* USE function [flower] to build values *)
  | Exists of var * expr * expr
  | ForAll of var * expr * expr
  | If of expr * expr * expr
  | Or of expr list
  | And of expr list
  | Not of expr
  | Call of string * expr list
  | Map of expr * expr
  | Pred of expr * expr
  | Dot of expr * expr
  | ArrayLookup of expr * expr
  | ArrayUnboxing of expr
  | Var of var
  | ContextItem
  | ContextEnv
  | EObject of (expr * expr) list
  | EnvObject (* object from current env *)
  | Objectify of expr
  | Arrayify of expr
  | Let of binder * expr * expr
  | DefFunc of string * var list (* args *) * expr (* ex. inputs as object with args as fields *)
	       * expr (* body *) * expr (* remaining expression *)
					    [@@deriving yojson]
 and flower =
  | Return of expr (* USE function [return] to build values *)
  | For of binder * expr * bool * flower (* optional flag *)
  | FLet of binder * expr * flower
  | Count of var * flower
  | Where of expr * flower
  | GroupBy of var list * flower
  | Hide of var list * flower
  | Slice of int * int option (* offset, limit? *) * flower
  | OrderBy of (expr * order) list * flower
  | FConcat of flower list
  | FIf of flower * flower * flower
			       [@@deriving yojson]

let return : expr -> flower = function
  | Flower f -> f
  | e -> Return e
let flower : flower -> expr = function
  | Return e -> e
  | f -> Flower f

let concat : expr list -> expr = function
  | [] -> Empty
  | [e] -> e
  | le -> Concat le
let fconcat : flower list -> flower = function
  | [] -> Return Empty
  | [f] -> f
  | lf -> FConcat lf
		
exception TypeError of string
exception Unbound of var
exception Undefined of string

let item_of_data (d : data) : item option =
  d
  |> Seq.case
       ~nil:(fun () -> None)
       ~cons:(fun i next ->
	      next
	      |> Seq.case
		   ~nil:(fun () -> Some i)
		   ~cons:(fun _ _ -> raise (TypeError "multiple items where one expected")))

let item_as_string (i : item) : string =
  match i with
  | `Bool b -> string_of_bool b
  | `Int n -> string_of_int n
  | `Intlit s -> s
  | `Float f -> string_of_float f
  | `String s -> s
  | _ -> raise (TypeError "invalid item as string")

let rec output_data out d =
  d |> Seq.iter
	 (fun i ->
	  output_item out i;
	  output_string out "\n")
and output_item out = function
  | `Bool b -> output_string out (string_of_bool b)
  | `Int n -> output_string out (string_of_int n)
  | `Intlit s -> output_string out s
  | `Float f -> output_string out (string_of_float f)
  | `String s -> output_char out '"';
		output_string out (String.escaped s);
		output_char out '"'
  | `Null -> output_string out "null"
  | `Assoc pairs ->
     output_char out '{';
     List.iter
       (fun (k,i) ->
	output_string out k;
	output_string out ": ";
	output_item out i;
	output_string out ", ")
       pairs;
     output_char out '}'
  | `List li ->
     output_char out '[';
     List.iter
       (fun i ->
	output_item out i;
	output_string out ", ")
     li;
     output_char out ']'
  | `Tuple li ->
     output_char out '(';
     List.iter
       (fun i ->
	output_item out i;
	output_string out ", ")
     li;
     output_char out ')'
  | `Variant (c, i_opt) ->
     output_char out '<';
     output_string out c;
     (match i_opt with
      | None -> ()
      | Some i ->
         output_char out ':';
         output_item out i);
     output_char out '>'
     
	       
let compare_item (i1 : item) (i2 : item) : int (* -1, 0, 1 *) =
  match i1, i2 with
  | `Null, `Null -> 0
  | `Null, _ -> -1
  | _, `Null -> 1
  | `Bool b1, `Bool b2 -> compare b1 b2
  | `Int n1, `Int n2 -> compare n1 n2
  | `Intlit s1, `Intlit s2 -> compare s1 s2
  | `Float f1, `Float f2 -> compare f1 f2
  | `Int n1, `Float f2 -> compare  (float n1) f2
  | `Float f1, `Int n2 -> compare f1 (float n2)
  | `String s1, `String s2 -> compare s1 s2
  | _ -> raise (TypeError "invalid types in item comparison")

let compare_item_opt (i1_opt : item option) (i2_opt : item option) : int (* -1, 0, 1 *) =
  match i1_opt, i2_opt with
  | None, None -> 0
  | None, _ -> -1
  | _, None -> 1
  | Some i1, Some i2 -> compare_item i1 i2

let rec compare_data (d1 : data) (d2 : data) : int (* -1, 0, 1 *) =
  d1
  |> Seq.case
       ~nil:(fun () ->
	     d2
	     |> Seq.case
		  ~nil:(fun () -> 0)
		  ~cons:(fun i2 r2 -> -1))
       ~cons:(fun i1 r1 ->
	      d2
	      |> Seq.case
		   ~nil:(fun () -> 1)
		   ~cons:(fun i2 r2 ->
			  let c = compare_item i1 i2 in
			  if c = 0
			  then compare_data r1 r2
			  else c))
		 
				     
let rec compare_ordering_key (lo : order list) (k1 : item option list) (k2 : item option list) : int (* -1, 0, 1 *) =
  match lo, k1, k2 with
  | [], [], [] -> 0
  | o::ro, i1::r1, i2::r2 ->
     let c =
       match o with
       | ASC -> compare_item_opt i1 i2
       | DESC -> compare_item_opt i2 i1 in
     if c = 0
     then compare_ordering_key ro r1 r2
     else c
  | _ -> invalid_arg "compare_ordering_key: argument lists have different lengths"


let var_context = "$"

let var_position x = "#" ^ x
let binder_position : binder -> string = function
  | Var x -> var_position x
  | Fields -> var_position "*" (* TODO: make them unique *)
let is_var_position x = x <> "" && x.[0] = '#'

					     
type env = (var * data) list
type funcs = (string * (env * var list * expr)) list
type result = (item * env) Seq.t (* sequence of main results along with additional key-value pairs *)

let result_of_item i = Seq.return (i,[])
let result_of_data d = Seq.map (fun i -> (i,[])) d
let data_of_result res = Seq.map (fun (i,_) -> i) res
				 
let is_true (res : result) : bool =
  match Seq.hd_opt res with
  | None -> false
  | Some (i,_) ->
     match i with
     | `Bool b -> b
     | `Int n -> n <> 0
     | `String s -> s <> ""
     | `Null -> false
     | _ -> true (* TODO: ?? *)

class type library =
  object
    method apply : string -> data list -> data
  end

let env_add k d env = (k,d) :: List.remove_assoc k env
  
let env_add_fields (res : result) (env : env) : env =
  match Seq.hd_opt res with
  | None -> env
  | Some (i,_) ->
     match i with
     | `Assoc pairs ->
	List.fold_left
	  (fun env (k,i) ->
	   let d =
	     match i with
	     | `Null -> Seq.empty
	     | `List li -> Seq.from_list li
	     | _ -> Seq.return i in
	   env_add k d env)
	  env pairs
     | _ -> env

let eval_binder (env : env) (res : result) : binder -> env = function
  | Var v -> env_add v (data_of_result res) env
  | Fields -> env_add_fields res env

let rec eval_expr (library : #library) (funcs : funcs) (env : env) : expr -> result = function
  | S s -> Seq.return (`String s, [])
  | Item i -> Seq.return (i, [])
  | FileString (fname,contents) -> Seq.return (`String contents, [])
  | Empty -> Seq.empty
  | Concat le -> Seq.from_list le |> Seq.flat_map (eval_expr library funcs env)
  | Flower lf -> eval_flower library funcs (Seq.return env) lf
  | Exists (x,e1,e2) ->
     let pos_x = var_position x in
     let ok =
       Seq.with_position (eval_expr library funcs env e1)
       |> Seq.fold_left
	    (fun ok (pos,(i1,_)) ->
	     ok
	     || let env = env_add x (Seq.return i1) (env_add pos_x (Seq.return (`Int pos)) env) in
		is_true (eval_expr library funcs env e2))
	    false in
     Seq.return (`Bool ok, [])
  | ForAll (x,e1,e2) ->
     let pos_x = var_position x in
     let ok =
       Seq.with_position (eval_expr library funcs env e1)
       |> Seq.fold_left
	    (fun ok (pos,(i1,_)) ->
	     ok
	     && let env = env_add x (Seq.return i1) (env_add pos_x (Seq.return (`Int pos)) env) in
		is_true (eval_expr library funcs env e2))
	    true in
     Seq.return (`Bool ok, [])
  | If (cond,e1,e2) ->
     if is_true (eval_expr library funcs env cond)
     then eval_expr library funcs env e1
     else eval_expr library funcs env e2
  | Or le ->
     let ok =
       List.fold_left
	 (fun ok e -> ok || is_true (eval_expr library funcs env e))
	 false le in
     Seq.return (`Bool ok, [])
  | And le ->
     let ok =
       List.fold_left
	 (fun ok e -> ok && is_true (eval_expr library funcs env e))
	 true le in
     Seq.return (`Bool ok, [])
  | Not e ->
     let ok = not (is_true (eval_expr library funcs env e)) in
     Seq.return (`Bool ok, [])
  | Call (name,le) when List.mem_assoc name funcs ->
     let func_env, args, e = List.assoc name funcs in
     if List.length args = List.length le
     then
       let call_env =
	 List.fold_left2
	   (fun call_env v e ->
	    let d = data_of_result (eval_expr library funcs env e) in
	    (v,d)::call_env)
	   func_env args le in
       eval_expr library funcs call_env e
     else Seq.empty (* raise (TypeError (name ^ ": wrong number of arguments")) *)
  | Call (func,le) ->
     let ld = List.map (fun e -> data_of_result (eval_expr library funcs env e)) le in
     result_of_data (library#apply func ld)
  | Map (e1,e2) ->
     let pos_x = var_position var_context in
     Seq.with_position (eval_expr library funcs env e1)
     |> Seq.flat_map
	  (fun (pos,(i,_)) ->
	   let env = env_add var_context (Seq.return i) (env_add pos_x (Seq.return (`Int pos)) env) in
	   eval_expr library funcs env e2)
  | Pred (e1,e2) ->
     let pos_x = var_position var_context in
     Seq.with_position (eval_expr library funcs env e1)
     |> Seq.flat_map
	  (fun (pos,(i,_)) ->
	   let env = env_add var_context (Seq.return i) (env_add pos_x (Seq.return (`Int pos)) env) in
	   if is_true (eval_expr library funcs env e2)
	   then Seq.return (i,[])
	   else Seq.empty)
  | Dot (e1,e2) ->
     eval_expr library funcs env e1
     |> Seq.flat_map
	  (function
	    | (`Assoc pairs, _) ->
	       eval_expr library funcs env e2
	       |> Seq.flat_map
		    (function
		      | (`String key, _) ->
			 (try Seq.return (List.assoc key pairs, [])
			  with Not_found -> Seq.empty)
		      | _ -> Seq.empty)
	    | _ -> Seq.empty)
  | ArrayLookup (e1,e2) ->
     eval_expr library funcs env e1
     |> Seq.flat_map
	  (function
	    | (`List li, _) ->
	       eval_expr library funcs env e2
	       |> Seq.flat_map
		    (function
		      | (`Int n, _) ->
                         let l = List.length li in
                         let pos = if n < 0 then l+n else n in
                         if pos >= 0 && pos < l
                         then Seq.return (List.nth li pos, [])
			 else Seq.empty
		      | _ -> Seq.empty)
	    | _ -> Seq.empty)
  | ArrayUnboxing e ->
     eval_expr library funcs env e
     |> Seq.flat_map
	  (fun (i,_) ->
	   match i with
	   | `List li -> result_of_data (Seq.from_list li)
	   | _ -> Seq.empty)
  | Var x ->
     (try result_of_data (List.assoc x env)
      with _ -> Seq.return (`Null, []) (*raise (Unbound x)*))
  | ContextItem ->
     (try result_of_data (List.assoc var_context env)
      with _ -> Seq.return (`Null, []) (*raise (Unbound var_context)*))
  | ContextEnv -> Seq.return (`Null, env) (* (item_of_env env) *)
  | EObject lkv ->
     let pairs =
       List.fold_right
	 (fun (e1,e2) pairs ->
	  match item_of_data (data_of_result (eval_expr library funcs env e1)) with
	  | None -> pairs
	  | Some (`String key) ->
	     ( match Seq.to_list (data_of_result (eval_expr library funcs env e2)) with
	       | [] -> pairs
	       | [`Null] -> pairs
	       | [i] -> (key,i)::pairs
	       | li -> (key, `List li)::pairs )
	  | _ -> raise (TypeError "string expected for object fields"))
	 lkv [] in
     Seq.return (`Assoc pairs, [])
  | EnvObject ->
     let pairs =
       List.fold_left
         (fun pairs (x,d) ->
           if is_var_position x
           then pairs
           else
             match Seq.to_list d with
             | [] -> pairs
             | [`Null] -> pairs
             | [i] -> (x,i)::pairs
             | li -> (x, `List li)::pairs)
         [] env in
     Seq.return (`Assoc pairs, [])
  | Objectify e -> (* behaves like JSONiq/accumulates function *)
     let dico = new dico in
     eval_expr library funcs env e
     |> Seq.iter
	  (fun (i,_) ->
	   match i with
	   | `Assoc pairs ->
	      pairs |> List.iter (fun (k,i) -> dico#add k i)
	   | _ -> ());
     let pairs =
       dico#fold
	 (fun pairs k li ->
	  let i =
	    match li with
	    | [] -> assert false
	    | [i] -> i
	    | _ -> `List li in
	  (k, i) :: pairs)
	 [] in
     Seq.return (`Assoc pairs, [])
  | Arrayify e ->
     Seq.return (`List (Seq.to_list (data_of_result (eval_expr library funcs env e))), [])
  | Let (br,e1,e2) ->
     let res = eval_expr library funcs env e1 in
     let env = eval_binder env res br in
     eval_expr library funcs env e2
  | DefFunc (name,args,e0,e1,e2) ->
(*
     let res0 = eval_expr library funcs env e0 in
     let res0 =
       if Seq.is_empty res0
       then (* ensuring at least one example input *)
	 let i = `Assoc (List.map (fun arg -> (arg, `Null)) args) in
	 Seq.return (i,[])
       else res0 in
 *)
     (* e0 only used when focus in function body *)
     let funcs = (name, (env,args,e1))::funcs in
     eval_expr library funcs env e2
     (*
     res0
     |> Seq.flat_map
	  (fun (i,_) ->
	   match i with
	   | `Assoc pairs ->
	      let env =
		List.fold_left
		  (fun env (k,j) ->
		   if List.mem k args
		   then
		     let d =
		       match j with
		       | `Null -> Seq.empty
		       | `List lj -> Seq.from_list lj
		       | _ -> Seq.return j in
		     (k,d)::env
		   else env) in
	      eval_expr library funcs env e2
	   | _ -> Seq.empty) (* no result *)
      *)
(*		    
     if inputs = []
     then eval_expr library funcs env e2
     else
       Seq.from_list inputs
       |> Seq.flat_map
	    (fun ds ->
	     let env =
	       try List.fold_left2
		     (fun env arg d -> (arg,d)::env)
		     env args ds
	       with _ -> env in
	     eval_expr library funcs env e2)
 *)
and eval_flower (library : #library) (funcs : funcs) (ctx : env Seq.t) : flower -> result = function
  | Return e ->
     ctx
     |> Seq.flat_map
	  (fun env -> eval_expr library funcs env e)
  | For (br, e, optional, f) ->
     let pos_br = binder_position br in
     let ctx =
       ctx
       |> Seq.flat_map
	    (fun env ->
	     let res = eval_expr library funcs env e in
	     if optional && Seq.is_empty res
	     then Seq.return env
	     else
	       Seq.with_position res
	       |> Seq.flat_map
		    (fun (pos,(i,_)) ->
		     let env = env_add pos_br (Seq.return (`Int pos)) env in
		     let env = eval_binder env (result_of_item i) br in
		     Seq.return env))
     in
     eval_flower library funcs ctx f
  | FLet (br,e, f) ->
     let ctx =
       ctx
       |> Seq.map
	    (fun env ->
	     let res = eval_expr library funcs env e in
	     eval_binder env res br)
     in
     eval_flower library funcs ctx f
  | Count (x, f) ->
     let ctx =
       Seq.with_position ctx
       |> Seq.map
	    (fun (pos,env) -> env_add x (Seq.return (`Int pos)) env)
     in
     eval_flower library funcs ctx f
  | Where (e, f) ->
     let ctx =
       ctx
       |> Seq.filter
	    (fun env -> is_true (eval_expr library funcs env e)) in
     eval_flower library funcs ctx f
  | GroupBy (lx, f) ->
     let dico_key = new dico in
     ctx
     |> Seq.iter
	  (fun env ->
	   let key = List.map
		      (fun x ->
		       try item_of_data (List.assoc x env)
		       with Not_found -> None)
		      lx in
	   dico_key#add key env);
     let lenv =
       dico_key#fold
	 (fun lenv key val_lenv ->
	  let dico_val = new dico in
	  val_lenv (* recording values for each non-grouped variable *)
	  |> List.iter
	       (fun val_env ->
		val_env
		|> List.iter
		     (fun (y,d) ->
		      if not (List.mem y lx)
		      then dico_val#add y d));
	  let env = (* grouped variables *)
	    List.map2
	      (fun x k ->
	       match k with
	       | None -> (x, Seq.empty)
	       | Some i -> (x, Seq.return i))
	      lx key in
	  let env = (* adding other variables *)
	    dico_val#fold
	      (fun env y y_vals ->
	        env_add y (Seq.concat y_vals) env)
	      (List.rev env) in (* solution environments are in reverse column order *)
	  env::lenv)
	 [] in
     let ctx = Seq.from_list lenv in
     eval_flower library funcs ctx f
  | Hide (lx,f) ->
     let ctx =
       ctx
       |> Seq.map
	    (fun env -> List.filter (fun (y,_) -> not (List.mem y lx)) env) in
     eval_flower library funcs ctx f
  | Slice (offset,limit,f) ->
     let ctx = Seq.slice ~offset ?limit ctx in
     eval_flower library funcs ctx f
  | OrderBy (leo, f) ->
     let le, lo = List.split leo in
     let l_key_env =
       ctx
       |> Seq.fold_left
	    (fun res env ->
	     let ordering_key : item option list =
	       List.map
		 (fun e -> item_of_data (data_of_result (eval_expr library funcs env e)))
		 le in
	     (ordering_key, env) :: res)
	    [] in
     let sorted_l_key_env =
       list_sort
	 (fun (k1,env1) (k2,env2) ->
	  compare_ordering_key lo k1 k2)
	 l_key_env in
     let ctx =
       Seq.from_list
	 (List.map snd sorted_l_key_env) in
     eval_flower library funcs ctx f
  | FConcat lf ->
     Seq.from_list lf |> Seq.flat_map (eval_flower library funcs ctx)
  | FIf (f1,f2,f3) ->
     if is_true (eval_flower library funcs ctx f1)
     then eval_flower library funcs ctx f2
     else eval_flower library funcs ctx f3

		      
(* ================  tests  ===================== *)
(*
module Test =
  struct
		      
let ex1 (csv : data) : expr =
  (* CSV has columns: dateTime, store, amount, consumer *)
  Flower (For
	    (Fields,
             Call (parseCSV, [File "example.csv",csv]),
	     FLet ("date", Call (Substring,
				 [Var "dateTime";
				  Item (`Int 0); Item (`Int 10)]),
		   FConcat
		     [ GroupBy (["store"],
				Return (EObject
					  [S "s", Call (StringConcat, [S "<store/"; Var "store"; S ">"]);
					   S "p", S "ex:totalAmount";
					   S "o", Call (Sum, [Dot (Var "row", S "amount")])]));
		       GroupBy (["date"],
				OrderBy ([Var "date", DESC],
					 FLet ("uriDate", Call (StringConcat,
								[S "<day/";
								 Var "date";
								 S ">"]),
					       Return
						 (Concat
						    [EObject
						       [S "s", Var "uriDate";
							S "p", Item (`String "ex:date");
							S "o", Var "date"];
						     EObject
						       [S "s", Var "uriDate";
							S "p", S "ex:averageAmount";
							S "o", Call (Avg, [Dot (Var "row", S "amount")])]]))))])))

(* CNL tentative version
  for each row in CSV
    let date be the substring of dateTime starting at 0 with length 10
    first
      group by store
      generate triples
        { <store/{the store}> ex:totalAmount {the total amount} . }
    then
      group by date
      order by date
      generate triples
        { <day/{the date}> ex:date {the date} ;
	                   ex:averageAmount {the average amount} . }
 *)
	    
let csv1 : data =
  Seq.from_list
    [`Assoc ["dateTime", `String "2019-05-01T00:50:09-07:00";
	     "store", `Int 95;
	     "amount", `Float 352.33;
	     "consumer", `String "Raja"];
     `Assoc ["dateTime", `String "2019-05-01T23:52:06-07:00";
	     "store", `Int 69;
	     "amount", `Float 354.95;
	     "consumer", `String "Tyrone"];
     `Assoc ["dateTime", `String "2019-05-02T00:50:34-07:00";
	     "store", `Int 95;
	     "amount", `Float 358.67;
	     "consumer", `String "Herrod"];
     `Assoc ["dateTime", `String "2019-05-03T01:00:44-07:00";
	     "store", `Int 70;
	     "amount", `Float 325.35;
	     "consumer", `String "Jackson"]]

let main () =
  eval_expr [] [] (ex1 csv1)
  |> data_of_result
  |> output_data stdout

  end
 *)
