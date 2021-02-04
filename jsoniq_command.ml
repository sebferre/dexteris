

let command_of_suggestion : Jsoniq_suggestions.suggestion -> string = function
  | FocusUp -> "up"
  | FocusRight -> "right"
  | Delete -> "del"
  | InsertBool b -> string_of_bool b
  | InputInt in_n -> string_of_int in_n#get
  | InputRange (in_a,in_b) -> Printf.sprintf "%d to %d" in_a#get in_b#get
  | InputFloat in_f -> string_of_float in_f#get
  | InputString in_s -> Printf.sprintf "\"%S\"" in_s#get
  | InputFileString _ -> "" (* no command for that *)
  | InsertNull -> "null"
  | InsertConcat1 -> ";"
  | InsertConcat2 -> "; _"
  | InsertExists in_x -> Printf.sprintf "some %s" in_x#get
  | InsertForAll in_x -> Printf.sprintf "every %s" in_x#get
  | InsertIf1 -> "if"
  | InsertIf2 -> "then"
  | InsertIf3 -> "else"
  | InsertOr -> "or"
  | InsertAnd -> "and"
  | InsertNot -> "not"
  | InsertFunc (name,arity,arg) -> (* TODO: customize for operators *)
     let args =
       List.init arity (fun pos -> if pos=arg then "_" else "?") in
     Printf.sprintf "%s(%s)" name (String.concat "," args)
  | InsertMap -> "map"
  | InsertPred -> "filter"
  | InsertDot -> "."
  | InsertField f -> "." ^ f
  | InsertArrayLookup -> "[["
  | InsertArrayUnboxing -> "[]"
  | InsertVar v -> v
  | InsertContextItem -> ""
  | InsertContextEnv -> ""
  | InsertObject -> "{"
  | InsertObjectField -> ":"
  | InsertArray -> "["
  | InsertObjectify -> "{_}"
  | InsertArrayify -> "[_]"
  | InsertDefFunc1 in_func ->
     let name, args = in_func#get in
     Printf.sprintf "def %s(%s)" name (String.concat "," args)
  | InsertDefFunc2 in_func ->
     let name, args = in_func#get in
     Printf.sprintf "def %s(%s) = ?" name (String.concat "," args)
  | InsertArg in_x -> Printf.sprintf ",%s" in_x#get
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
  | InsertProject (lx,in_x) -> Printf.sprintf "project on %s" in_x#get
  | InsertSlice (in_offset,in_limit) ->
     let offset, limit = in_offset#get, in_limit#get in
     if offset = 0
     then Printf.sprintf "limit %d" limit
     else Printf.sprintf "offset %d limit %d" offset limit
  | InsertOrderBy1 in_o -> in_o#get
  | InsertOrderBy2 in_o -> Printf.sprintf "%s ?" in_o#get

let score_of_suggestion sugg cmd = 1. (* TODO *)

                                                                  
