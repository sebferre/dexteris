
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
    
(* ======================================================= *)
    
exception TODO
    
type item = (* JSON *)
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Null
  | Object of (string * item) list
  | Array of item list
			      
type data = item Seq.t

type var = string
type order = DESC | ASC
type func =
  | EQ | NE | LE | LT | GE | GT
  | Plus | Minus | Times | Div | IDiv | Mod
  | Neg
  | StringConcat | Substring
  | Range
  | Sum | Avg
  | Defined of string * int

type expr =
  | S of string
  | Item of item
  | Empty
  | Data of data
  | Concat of expr list
  | Flower of flower
  | Exists of var * expr * expr
  | ForAll of var * expr * expr
  | If of expr * expr * expr
  | Or of expr list
  | And of expr list
  | Not of expr
  | Call of func * expr list
  | Map of expr * expr
  | Pred of expr * expr
  | Dot of expr * expr
  | ArrayLookup of expr * expr
  | ArrayUnboxing of expr
  | Var of var
  | ContextItem
  | ContextEnv
  | EObject of (expr * expr) list
  | Objectify of expr
  | Arrayify of expr
  | DefVar of var * expr * expr
  | DefFunc of string * var list * expr * expr
 and flower =
  | Return of expr
  | For of var * expr * bool * flower (* optional flag *)
  | ForObject of expr * bool * flower (* optional flag *)
  | Let of var * expr * flower
  | Where of expr * flower
  | GroupBy of var list * flower
  | OrderBy of (expr * order) list * flower
  | FConcat of flower list
  | FIf of flower * flower * flower

let flower_of_expr : expr -> flower = function
  | Flower f -> f
  | e -> Return e
let expr_of_flower : flower -> expr = function
  | Return e -> e
  | f -> Flower f
		
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
  | Bool b -> string_of_bool b
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | String s -> s
  | _ -> raise (TypeError "invalid item as string")

let rec output_data out d =
  d |> Seq.iter
	 (fun i ->
	  output_item out i;
	  output_string out "\n")
and output_item out = function
  | Bool b -> output_string out (string_of_bool b)
  | Int n -> output_string out (string_of_int n)
  | Float f -> output_string out (string_of_float f)
  | String s -> output_char out '"';
		output_string out (String.escaped s);
		output_char out '"'
  | Null -> output_string out "null"
  | Object pairs ->
     output_char out '{';
     List.iter
       (fun (k,i) ->
	output_string out k;
	output_string out ": ";
	output_item out i;
	output_string out ", ")
       pairs;
     output_char out '}'
  | Array li ->
     output_char out '[';
     List.iter
       (fun i ->
	output_item out i;
	output_string out ", ")
     li;
     output_char out ']'
	       
let compare_item (i1 : item) (i2 : item) : int (* -1, 0, 1 *) =
  match i1, i2 with
  | Null, Null -> 0
  | Null, _ -> -1
  | _, Null -> 1
  | Bool b1, Bool b2 -> compare b1 b2
  | Int n1, Int n2 -> compare n1 n2
  | Float f1, Float f2 -> compare f1 f2
  | Int n1, Float f2 -> compare  (float n1) f2
  | Float f1, Int n2 -> compare f1 (float n2)
  | String s1, String s2 -> compare s1 s2
  | _ -> raise (TypeError "invalid types in item comparison")

let compare_item_opt (i1_opt : item option) (i2_opt : item option) : int (* -1, 0, 1 *) =
  match i1_opt, i2_opt with
  | None, None -> 0
  | None, _ -> -1
  | _, None -> 1
  | Some i1, Some i2 -> compare_item i1 i2
	       
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

let arity_of_func : func -> int = function
  | EQ | NE | LE | LT | GE | GT -> 2
  | Plus | Minus | Times | Div | IDiv | Mod -> 2
  | Neg -> 1
  | Substring -> 3
  | Range -> 2
  | Sum | Avg | StringConcat -> 1
  | Defined (name,n) -> n

let apply_func (func : func) (args : data list) : data =
  match func, args with
  | (EQ|NE|GT|GE|LT|LE), [d1;d2] ->
     let c : int = compare_item_opt (item_of_data d1) (item_of_data d2) in
     let ok = 
       match func with
       | EQ -> c = 0
       | NE -> c <> 0
       | LT -> c < 0
       | LE -> c <= 0
       | GT -> c > 0
       | GE -> c >= 0
       | _ -> assert false in
     Seq.return (Bool ok)
  | (Plus|Minus|Times|Div), [d1; d2] ->
     ( match item_of_data d1, item_of_data d2 with
       | None, _
       | _, None -> Seq.empty
       | Some i1, Some i2 ->
	  let opn, opf =
	    match func with
	    | Plus -> (+), (+.)
	    | Minus -> (-), (-.)
	    | Times -> ( * ), ( *. )
	    | Div -> (/), (/.)
	    | _ -> assert false in
	  let i =
	    match i1, i2 with
	    | Int n1, Int n2 -> Int (opn n1 n2)
	    | Int n1, Float f2 -> Float (opf (float n1) f2)
	    | Float f1, Int n2 -> Float (opf f1 (float n2))
	    | Float f1, Float f2 -> Float (opf f1 f2)
	    | _ -> raise (TypeError "apply_func: invalid types in arguments") in
	  Seq.return i )
  | (IDiv|Mod), [d1;d2] ->
     ( match item_of_data d1, item_of_data d2 with
       | None, _
       | _, None -> Seq.empty
       | Some (Int n1), Some (Int n2) ->
	  let n =
	    match func with
	    | IDiv -> n1 / n2
	    | Mod -> n1 mod n2
	    | _ -> assert false in
	  Seq.return (Int n)
       | _ -> raise (TypeError "apply_func: invalid types in arguments") )
  | Neg, [arg] ->
     ( match item_of_data arg with
       | None -> Seq.empty
       | Some (Int n1) -> Seq.return (Int (- n1))
       | Some (Float f1) -> Seq.return (Float (-. f1))
       | _ -> raise (TypeError "apply_func: invalid types in arguments") )
  | Substring, [str; start; len] ->
     let str : string = match item_of_data str with Some (String s) -> s | _ -> raise (TypeError "substring: undefined string") in
     let start : int = match item_of_data start with Some (Int pos) -> pos | _ -> 0 in
     let len : int = match item_of_data len with Some (Int len) -> len | _ -> String.length str - start in
     Seq.return (String (String.sub str start len))
  | Range, [d1;d2] ->
     ( match item_of_data d1, item_of_data d2 with
       | None, _
       | _, None -> Seq.empty
       | Some (Int a), Some (Int b) -> Seq.map (fun n -> Int n) (Seq.range a b)
       | _ -> raise (TypeError "apply_func: invalid types in arguments") )
  | Sum, [d] ->
     let sum =
       Seq.fold_left
	 (fun sum i ->
	  match i with
	  | Bool b -> if b then sum +. 1. else sum
	  | Int n -> sum +. float n
	  | Float f -> sum +. f
	  | _ -> sum)
	 0. d in
     Seq.return (Float sum)
  | Avg, [d] ->
     let nb, sum =
       Seq.fold_left
	 (fun (nb,sum) i ->
	  match i with
	  | Bool b -> if b then nb+1, sum +. 1. else nb+1, sum
	  | Int n -> nb+1, sum +. float n
	  | Float f -> nb+1, sum +. f
	  | _ -> nb, sum)
	 (0,0.) d in
     Seq.return (Float (sum /. float nb))
  | StringConcat, [d] ->
     let li = Seq.to_list d in
     let ls = List.map item_as_string li in
     Seq.return (String (String.concat "" ls))
  | Defined _, _ -> invalid_arg "apply_func: defined function as argument"
  | _ -> raise (Undefined ("apply_func: wrong number of arguments"))
	       

let var_context = "$"

type env = (var * data) list
type funcs = (string * (env * var list * expr)) list

let item_of_env (env : env) : item =
  let pairs =
    List.fold_left
      (fun pairs (x,d) ->
       match Seq.to_list d with
       | [] -> pairs
       | [i] -> (x,i)::pairs
       | li -> (x, Array li)::pairs)
      [] env in
  Object pairs

let is_true (d : data) : bool =
  match Seq.hd_opt d with
  | None -> false
  | Some i ->
     match i with
     | Bool b -> b
     | Int n -> n <> 0
     | String s -> s <> ""
     | Null -> false
     | _ -> true (* TODO: ?? *)

let rec eval_expr (funcs : funcs) (env : env) : expr -> data = function
  | S s -> Seq.return (String s)
  | Item i -> Seq.return i
  | Empty -> Seq.empty
  | Data d -> d
  | Concat le -> Seq.from_list le |> Seq.flat_map (eval_expr funcs env)
  | Flower lf -> eval_flower funcs (Seq.return env) lf
  | Exists (x,e1,e2) ->
     let ok =
       eval_expr funcs env e1
       |> Seq.fold_left
	    (fun ok i1 ->
	     ok
	     || let env = (x, Seq.return i1)::env in
		is_true (eval_expr funcs env e2))
	    false in
     Seq.return (Bool ok)
  | ForAll (x,e1,e2) ->
     let ok =
       eval_expr funcs env e1
       |> Seq.fold_left
	    (fun ok i1 ->
	     ok
	     && let env = (x, Seq.return i1)::env in
		is_true (eval_expr funcs env e2))
	    true in
     Seq.return (Bool ok)
  | If (cond,e1,e2) ->
     if is_true (eval_expr funcs env cond)
     then eval_expr funcs env e1
     else eval_expr funcs env e2
  | Or le ->
     let ok =
       List.fold_left
	 (fun ok e -> ok || is_true (eval_expr funcs env e))
	 false le in
     Seq.return (Bool ok)
  | And le ->
     let ok =
       List.fold_left
	 (fun ok e -> ok && is_true (eval_expr funcs env e))
	 true le in
     Seq.return (Bool ok)
  | Not e ->
     let ok = not (is_true (eval_expr funcs env e)) in
     Seq.return (Bool ok)
  | Call (Defined (name,arity), le) ->
     if List.mem_assoc name funcs
     then
       if List.length le = arity
       then
	 let func_env, args, e = List.assoc name funcs in
	 let call_env =
	   List.fold_left2
	     (fun env v e ->
	      let d = eval_expr funcs env e in
	      (v,d)::env)
	     func_env args le in
	 eval_expr funcs call_env e
       else raise (TypeError (name ^ ": wrong number of arguments"))
     else raise (Undefined ("function " ^ name))
  | Call (func,le) ->
     let ld = List.map (eval_expr funcs env) le in
     apply_func func ld
  | Map (e1,e2) ->
     eval_expr funcs env e1
     |> Seq.flat_map
	  (fun i ->
	   let env = (var_context, Seq.return i) :: env in
	   eval_expr funcs env e2)
  | Pred (e1,e2) ->
     eval_expr funcs env e1
     |> Seq.filter
	  (fun i ->
	   let env = (var_context, Seq.return i) :: env in
	   is_true (eval_expr funcs env e2))
  | Dot (e1,e2) ->
     eval_expr funcs env e1
     |> Seq.flat_map
	  (function
	    | Object pairs ->
	       eval_expr funcs env e2
	       |> Seq.flat_map
		    (function
		      | String key ->
			 (try Seq.return (List.assoc key pairs)
			  with _ -> Seq.empty)
		      | _ -> Seq.empty)
	    | _ -> Seq.empty)
  | ArrayLookup (e1,e2) ->
     eval_expr funcs env e1
     |> Seq.flat_map
	  (function
	    | Array li ->
	       eval_expr funcs env e2
	       |> Seq.flat_map
		    (function
		      | Int n ->
			 (try Seq.return (List.nth li (n-1))
			  with _ -> Seq.empty)
		      | _ -> Seq.empty)
	    | _ -> Seq.empty)
  | ArrayUnboxing e ->
     eval_expr funcs env e
     |> Seq.flat_map
	  (fun i ->
	   match i with
	   | Array li -> Seq.from_list li
	   | _ -> Seq.empty)
  | Var x -> (try List.assoc x env with _ -> raise (Unbound x))
  | ContextItem -> (try List.assoc var_context env with _ -> raise (Unbound var_context))
  | ContextEnv -> Seq.return (item_of_env env)
  | EObject lkv ->
     let pairs =
       List.fold_right
	 (fun (e1,e2) pairs ->
	  match item_of_data (eval_expr funcs env e1) with
	  | None -> pairs
	  | Some (String key) ->
	     ( match Seq.to_list (eval_expr funcs env e2) with
	       | [] -> pairs
	       | [Null] -> pairs
	       | [i] -> (key,i)::pairs
	       | li -> (key, Array li)::pairs )
	  | _ -> raise (TypeError "string expected for object fields"))
	 lkv [] in
     Seq.return (Object pairs)
  | Objectify e ->
     let dico = new dico in
     eval_expr funcs env e
     |> Seq.iter
	  (fun i ->
	   match i with
	   | Object pairs ->
	      pairs |> List.iter (fun (k,i) -> dico#add k i)
	   | _ -> ());
     let pairs =
       dico#fold
	 (fun pairs k li ->
	  let i =
	    match li with
	    | [] -> assert false
	    | [i] -> i
	    | _ -> Array li in
	  (k, i) :: pairs)
	 [] in
     Seq.return (Object pairs)
  | Arrayify e ->
     Seq.return (Array (Seq.to_list (eval_expr funcs env e)))
  | DefVar (v,e1,e2) ->
     let d = eval_expr funcs env e1 in
     let env = (v,d)::env in
     eval_expr funcs env e2
  | DefFunc (name,args,e1,e2) ->
     let funcs = (name, (env,args,e1))::funcs in
     eval_expr funcs env e2
and eval_flower (funcs : funcs) (ctx : env Seq.t) : flower -> data = function
  | Return e ->
     ctx
     |> Seq.flat_map
	  (fun env -> eval_expr funcs env e)
  | For (x, e, optional, f) ->
     let ctx =
       ctx
       |> Seq.flat_map
	    (fun env ->
	     let d = eval_expr funcs env e in
	     if optional && Seq.is_empty d
	     then Seq.return env
	     else
	       d
	       |> Seq.flat_map
		    (fun i ->
		     Seq.return ((x, Seq.return i)::env))) in
     eval_flower funcs ctx f
  | ForObject (e, optional, f) ->
     let ctx =
       ctx
       |> Seq.flat_map
	    (fun env ->
	     let d = eval_expr funcs env e in
	     if optional && Seq.is_empty d
	     then Seq.return env
	     else
	       d
	       |> Seq.flat_map
		    (function
		      | Object pairs ->
			 let env =
			   List.fold_left
			     (fun env (x,i) -> (x, Seq.return i)::env)
			     env pairs in
			 Seq.return env
		      | _ -> Seq.empty)) in
     eval_flower funcs ctx f
  | Let (x,e, f) ->
     let ctx =
       ctx
       |> Seq.map
	    (fun env -> (x, eval_expr funcs env e)::env) in
     eval_flower funcs ctx f
  | Where (e, f) ->
     let ctx =
       ctx
       |> Seq.filter
	    (fun env -> is_true (eval_expr funcs env e)) in
     eval_flower funcs ctx f
  | GroupBy (lx, f) ->
     let dico_key = new dico in
     ctx
     |> Seq.iter
	  (fun env ->
	   let key = List.map
		      (fun x ->
		       try item_of_data (List.assoc x env)
		       with _ -> None)
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
	       (y, Seq.concat y_vals) :: env)
	      env in
	  env::lenv)
	 [] in
     let ctx = Seq.from_list lenv in
     eval_flower funcs ctx f
  | OrderBy (leo, f) ->
     let le, lo = List.split leo in
     let l_key_env =
       ctx
       |> Seq.fold_left
	    (fun res env ->
	     let ordering_key : item option list =
	       List.map
		 (fun e -> item_of_data (eval_expr funcs env e))
		 le in
	     (ordering_key, env) :: res)
	    [] in
     let sorted_l_key_env =
       List.sort
	 (fun (k1,env1) (k2,env2) ->
	  compare_ordering_key lo k1 k2)
	 l_key_env in
     let ctx =
       Seq.from_list
	 (List.map snd sorted_l_key_env) in
     eval_flower funcs ctx f
  | FConcat lf ->
     Seq.concat
       (List.map (eval_flower funcs ctx) lf)
  | FIf (f1,f2,f3) ->
     if is_true (eval_flower funcs ctx f1)
     then eval_flower funcs ctx f2
     else eval_flower funcs ctx f3

		      
(* ================  tests  ===================== *)

module Test =
  struct
		      
let ex1 (csv : data) : expr =
  (* CSV has columns: dateTime, store, amount, consumer *)
  Flower (ForObject (Data csv, false,
       Let ("date", Call (Substring,
			  [Var "dateTime";
			   Item (Int 0); Item (Int 10)]),
	    FConcat
	      [ GroupBy (["store"],
			 Return (EObject
				   [S "s", Call (StringConcat, [S "<store/"; Var "store"; S ">"]);
				    S "p", S "ex:totalAmount";
				    S "o", Call (Sum, [Var "amount"])]));
		GroupBy (["date"],
			 OrderBy ([Var "date", DESC],
				  Let ("uriDate", Call (StringConcat,
							[S "<day/";
							 Var "date";
							 S ">"]),
				       Return
					 (Concat
					    [EObject
					       [S "s", Var "uriDate";
						S "p", Item (String "ex:date");
						S "o", Var "date"];
					     EObject
					       [S "s", Var "uriDate";
						S "p", S "ex:averageAmount";
						S "o", Call (Avg, [Var "amount"])]]))))])))

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
    [Object ["dateTime", String "2019-05-01T00:50:09-07:00";
	     "store", Int 95;
	     "amount", Float 352.33;
	     "consumer", String "Raja"];
     Object ["dateTime", String "2019-05-01T23:52:06-07:00";
	     "store", Int 69;
	     "amount", Float 354.95;
	     "consumer", String "Tyrone"];
     Object ["dateTime", String "2019-05-02T00:50:34-07:00";
	     "store", Int 95;
	     "amount", Float 358.67;
	     "consumer", String "Herrod"];
     Object ["dateTime", String "2019-05-03T01:00:44-07:00";
	     "store", Int 70;
	     "amount", Float 325.35;
	     "consumer", String "Jackson"]]

let main () =
  eval_expr [] [] (ex1 csv1)
  |> output_data stdout

  end
