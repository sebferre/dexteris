
let command_of_func name arity arg =
  let args =
    List.init arity (fun pos -> if pos+1 = arg then "_" else "?") in
  Printf.sprintf "%s(%s)" name (String.concat ", " args)

let command_of_suggestion library : Jsoniq_focus.transf -> string = function
  | FocusUp -> "up"
  | FocusRight -> "right"
  | Delete -> "del"
  | InsertBool b -> string_of_bool b
  | InputInt in_n -> string_of_int in_n#get
  | InputRange (in_a,in_b) -> Printf.sprintf "%d to %d" in_a#get in_b#get
  | InputFloat in_f -> string_of_float in_f#get
  | InputString in_s -> Printf.sprintf "%S" in_s#get
  | InputFileString _ -> "" (* no command for that *)
  | InputFileTable _ -> "" (* no command for that *)
  | InsertNull -> "null"
  | InsertConcat1 -> ";"
  | InsertConcat2 -> "; _"
  | InsertExists1 in_x -> Printf.sprintf "some %s" in_x#get
  | InsertForAll1 in_x -> Printf.sprintf "every %s" in_x#get
  | InsertExists2 in_x -> Printf.sprintf "some %s in ?" in_x#get
  | InsertForAll2 in_x -> Printf.sprintf "every %s in ?" in_x#get
  | InsertIf1 -> "if"
  | InsertIf2 -> "then"
  | InsertIf3 -> "else"
  | InsertOr -> "or"
  | InsertAnd -> "and"
  | InsertNot -> "not"
  | InsertFunc (name,arity,arg) ->
     (try
        let func = library#lookup name in
        if func#arity = arity
        then func#command ~arg
        else assert false
      with _ -> command_of_func name arity arg)
  | InsertMap -> "map"
  | InsertPred -> "filter"
  | InsertDot -> "."
  | InsertField f -> "." ^ f
  | InsertArrayLookup -> "[["
  | InsertArrayUnboxing -> "[]"
  | InsertVar v -> Jsoniq_syntax.label_of_var v
  | InsertContextItem -> ""
  | InsertContextEnv -> ""
  | InsertObject in_field ->
     let field = in_field#get in
     if field = ""
     then "{"
     else "{ " ^ Printf.sprintf "%S" field ^ " :"
  | InsertObjectField in_field ->
     let field = in_field#get in
     if field = ""
     then ":"
     else Printf.sprintf "%S" field ^ " :"
  | InsertEnvObject -> "{*}"
  | InsertArray -> "["
  | InsertObjectify -> "{_}"
  | InsertArrayify -> "[_]"
  | InsertDefFunc1 in_func ->
     let name, args = in_func#get in
     Printf.sprintf "def %s(%s)" name (String.concat "," args)
  | InsertDefFunc2 in_func ->
     let name, args = in_func#get in
     Printf.sprintf "def %s(%s) = ?" name (String.concat "," args)
  | InsertArg in_x -> Printf.sprintf ", %s" in_x#get
  | InsertForVar1 (in_x,in_opt) -> Printf.sprintf "for %s" in_x#get (* TODO: optional *)
  | InsertForVar2 (in_x,in_opt) -> Printf.sprintf "for %s in ?" in_x#get
  | InsertForFields1 in_opt -> Printf.sprintf "for *"
  | InsertLetVar1 in_x -> Printf.sprintf "let %s" in_x#get
  | InsertLetVar2 in_x -> Printf.sprintf "let %s = ?" in_x#get
  | InsertLetFields1 -> "let *"
  | InsertCount1 in_x -> Printf.sprintf "count %s" in_x#get
  | InsertWhere1 -> "where"
  | InsertWhere2 -> "where ?"
  | InsertGroupBy (lx,in_x) -> Printf.sprintf "group by %s" in_x#get
  | InsertHide (_lx,in_x) -> Printf.sprintf "hide %s" in_x#get
  | InsertSlice (in_offset,in_limit) ->
     let offset, limit = in_offset#get, in_limit#get in
     if offset = 0
     then Printf.sprintf "limit %d" limit
     else Printf.sprintf "offset %d limit %d" offset limit
  | InsertOrderBy1 in_o -> in_o#get
  | InsertOrderBy2 in_o -> Printf.sprintf "%s ?" in_o#get

let score_of_bool b =
  if b then 1. else 0.

let score_of_func name arity pos cmd =
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
  
let score_of_suggestion library (sugg : Jsoniq_focus.transf) (cmd : string) : float =
  assert (cmd <> "");
  let cmd = String.trim cmd in
  if cmd="" then 0.
  else try
    match sugg with
    | FocusUp ->
       if cmd="up" then 0.9
       else if cmd="u" then 0.5
       else 0.
    | FocusRight ->
       if cmd="right" then 0.9
       else if cmd="r" then 0.5
       else 0.
    | Delete ->
       if cmd="del" || cmd="delete" then 0.9
       else 0.
    | InsertBool true -> score_of_bool (cmd="true")
    | InsertBool false -> score_of_bool (cmd="false")
    | InputInt in_n ->
       Scanf.sscanf cmd "%d%!"
         (fun n -> in_n#set n; 1.)
    | InputRange (in_a,in_b) ->
       Scanf.sscanf cmd "%d to %d%!"
         (fun a b -> in_a#set a; in_b#set b; 1.)
    | InputFloat in_f ->
       Scanf.sscanf cmd "%F%!"
         (fun f -> in_f#set f; 1.)
    | InputString in_s ->
       Scanf.sscanf cmd "%S%!"
         (fun s -> in_s#set s; 1.)
(*        with
        | _ -> Scanf.sscanf cmd "%s"
                 (fun s -> in_s#set s; 0.1)) *)
    | InputFileString _ -> 0.
    | InputFileTable _ -> 0.
    | InsertNull -> score_of_bool (cmd="null")
    | InsertConcat1 -> score_of_bool (cmd=";")
    | InsertConcat2 -> score_of_bool (cmd="; _")
    | InsertExists1 in_x ->
       Scanf.sscanf cmd "some %[^ ()]%!"
         (fun s -> in_x#set s; 1.)
    | InsertForAll1 in_x ->
       Scanf.sscanf cmd "every %[^ ()]%!"
         (fun s -> in_x#set s; 1.)
    | InsertExists2 in_x ->
       Scanf.sscanf cmd "some %[^ ()] in ?%!"
         (fun s -> in_x#set s; 1.)
    | InsertForAll2 in_x ->
       Scanf.sscanf cmd "every %[^ ()] in ?%!"
         (fun s -> in_x#set s; 1.)
    | InsertIf1 -> score_of_bool (cmd="if")
    | InsertIf2 -> score_of_bool (cmd="then")
    | InsertIf3 -> score_of_bool (cmd="else")
    | InsertOr -> score_of_bool (cmd="or")
    | InsertAnd -> score_of_bool (cmd="and")
    | InsertNot -> score_of_bool (cmd="not")
    | InsertFunc (name,arity,arg) ->
       (try
          let func = library#lookup name in
          if func#arity = arity
          then func#command_score ~arg cmd
          else 0.
        with _ -> score_of_func name arity arg cmd)
    | InsertMap ->
       if cmd="!" then 1.
       else if cmd="map" then 0.9
       else 0.
    | InsertPred ->
       if cmd="[?]" then 1.
       else if cmd="filter" then 0.9
       else 0.
    | InsertDot ->
       if cmd="." then 1.
       else 0.
    | InsertField f ->
       Scanf.sscanf cmd ". %s"
         (fun s -> if s=f then 1. else 0.)
    | InsertArrayLookup ->
       if cmd="[[" then 1.
       else Scanf.sscanf cmd "[[%_[ ?]]]%!" 1.
    | InsertArrayUnboxing ->
       if cmd="[]" then 1.
       else if cmd="unbox" then 0.9
       else 0.
    | InsertVar v -> score_of_bool (cmd = Jsoniq_syntax.label_of_var v)
    | InsertContextItem -> 0.
    | InsertContextEnv -> 0.
    | InsertObject in_field ->
       if cmd="{" then 1.
       else
         (try Scanf.sscanf cmd "{ %s : %_[?]%!" (fun s -> in_field#set s; 1.)
          with _ ->
            try Scanf.sscanf cmd "{ %S : %_[?]%!" (fun s -> in_field#set s; 1.)
            with _ -> Scanf.sscanf cmd "{ %_[?] : %_[?]%!" 1.)
    (* Scanf.sscanf cmd "{ %_[?] }%!" 1. *)
    | InsertObjectField in_field ->
       (try Scanf.sscanf cmd "%S : %_[?]%!" (fun s -> in_field#set s; 1.)
        with _ ->
          try Scanf.sscanf cmd "%s : %_[?]%!" (fun s -> in_field#set s; 1.)
          with _ -> Scanf.sscanf cmd "%_[?] : %_[?]%!" 1.)
    | InsertEnvObject ->
       Scanf.sscanf cmd "{ * }%!" 1.
    | InsertArray ->
       if cmd="[" then 1.
       else if cmd<>"[]" then Scanf.sscanf cmd "[ %_[?] ]%!" 1.
       else 0.
    | InsertObjectify ->
       Scanf.sscanf cmd "{ _ }%!" 1.
    | InsertArrayify ->
       Scanf.sscanf cmd "[ _ ]%!" 1.
    | InsertDefFunc1 in_func ->
       Scanf.sscanf cmd "def %s@(%s@)%!"
         (fun name sargs ->
           if name="" then 0.
           else
             let args = String.split_on_char ',' sargs in
             let args = List.map String.trim args in
             if List.mem "" args then 0.
             else (
               in_func#set (name,args);
               1.))
    | InsertDefFunc2 in_func ->
       Scanf.sscanf cmd "def %s@(%s@) = ?%!"
         (fun name sargs ->
           if name="" then 0.
           else
             let args = String.split_on_char ',' sargs in
             let args = List.map String.trim args in
             if List.mem "" args then 0.
             else (
               in_func#set (name,args);
               1.))
    | InsertArg in_x ->
       Scanf.sscanf cmd ", %s"
         (fun s -> in_x#set s; 1.)
    | InsertForVar1 (in_x,in_opt) ->
       Scanf.sscanf cmd "for %[^ ]%!"
         (fun s -> in_x#set s; 1.)
    | InsertForVar2 (in_x,in_opt) ->
       Scanf.sscanf cmd "for %[^ ] in ?%!"
         (fun s -> in_x#set s; 1.)
    | InsertForFields1 in_opt ->
       Scanf.sscanf cmd "for *" 1.
    | InsertLetVar1 in_x ->
       Scanf.sscanf cmd "let %[^ ]%!"
         (fun s -> in_x#set s; 1.)
    | InsertLetVar2 in_x ->
       Scanf.sscanf cmd "let %[^ ] = ?%!"
         (fun s -> in_x#set s; 1.)
    | InsertLetFields1 ->
       Scanf.sscanf cmd "let *" 1.
    | InsertCount1 in_x ->
       Scanf.sscanf cmd "count %s"
         (fun s -> in_x#set s; 1.)
    | InsertWhere1 ->
       if cmd="where" then 1.
       else Scanf.sscanf cmd "where _%!" 1.
    | InsertWhere2 ->
       Scanf.sscanf cmd "where ?%!" 1.
    | InsertGroupBy (lx,in_x) ->
       Scanf.sscanf cmd "group by %s"
         (fun s -> if List.mem s lx then (in_x#set s; 1.) else 0.)
    | InsertHide (lx,in_x) ->
       Scanf.sscanf cmd "hide %s"
         (fun s -> if List.mem s lx then (in_x#set s; 1.) else 0.)
    | InsertSlice (in_offset,in_limit) ->
       (try Scanf.sscanf cmd "limit %d%!"
             (fun n -> in_limit#set n; 1.)
        with _ -> Scanf.sscanf cmd "offset %d limit %d"
                    (fun n1 n2 -> in_offset#set n1; in_limit#set n2; 1.))
    | InsertOrderBy1 in_o ->
       if cmd="ascending" then (in_o#set cmd; 1.)
       else if cmd="descending" then (in_o#set cmd; 1.)
       else 0.
    | InsertOrderBy2 in_o ->
       (try Scanf.sscanf cmd "ascending %[?]%!"
              (fun _ -> in_o#set "ascending"; 1.)
        with _ -> Scanf.sscanf cmd "descending %[?]%!"
                    (fun _ -> in_o#set "descending"; 1.))
  with
  | Invalid_argument _ -> assert false
  | _ -> 0.
                                                                
