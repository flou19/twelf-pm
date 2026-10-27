structure Lint = struct
type issue = {file:string, line:int, sev:string, msg:string}
fun isIdent c = Char.isAlphaNum c orelse c = #"_" orelse c = #"'" orelse c = #"/"
fun takeName s =
  let fun go [] a = String.implode (List.rev a)
        | go (c::t) a = if isIdent c then go t (c::a) else String.implode (List.rev a)
  in go (String.explode (U.trim s)) [] end
fun familyOf name =
  case String.fields (fn c => c = #"/") name of f::_ => f | [] => name
fun declsFile path =
  let val ls = U.lines (U.readFile path)
      fun go i [] acc = List.rev acc
        | go i (l::ls) acc =
            let val t = U.trim l
            in if t = "" orelse U.startsWith "%" t then go (i+1) ls acc
               else if String.isSubstring ":" t then
                 let val n = takeName t
                 in if n = "" then go (i+1) ls acc else go (i+1) ls ((n,i,t)::acc) end
               else go (i+1) ls acc
            end
  in go 1 ls [] end
fun directives path =
  let val ls = U.lines (U.readFile path)
      fun go i [] (modes, worlds, totals, freezes, asserts) =
            (List.rev modes, List.rev worlds, List.rev totals, List.rev freezes, List.rev asserts)
        | go i (l::ls) (modes, worlds, totals, freezes, asserts) =
            let val t = U.trim l
            in if U.startsWith "%mode" t then go (i+1) ls ((i,t)::modes, worlds, totals, freezes, asserts)
               else if U.startsWith "%worlds" t then go (i+1) ls (modes, (i,t)::worlds, totals, freezes, asserts)
               else if U.startsWith "%total" t then go (i+1) ls (modes, worlds, (i,t)::totals, freezes, asserts)
               else if U.startsWith "%freeze" t then go (i+1) ls (modes, worlds, totals, (i,t)::freezes, asserts)
               else if U.startsWith "%assert" t then go (i+1) ls (modes, worlds, totals, freezes, (i,t)::asserts)
               else go (i+1) ls (modes, worlds, totals, freezes, asserts)
            end
  in go 1 ls ([], [], [], [], []) end
fun dups xs =
  let fun go [] seen d = d
        | go ((n,i,_)::rest) seen d =
            if List.exists (fn (x:string) => x = n) seen then go rest seen ((n,i)::d)
            else go rest (n::seen) d
  in go xs [] [] end
fun uses body name =
  let val n = size name
      fun match at =
        at + n <= size body andalso String.substring (body, at, n) = name andalso
        (at = 0 orelse not (isIdent (String.sub (body, at-1)))) andalso
        (at + n = size body orelse not (isIdent (String.sub (body, at+n))))
      fun go i c = if i > size body - n then c else go (i+1) (if match i then c+1 else c)
  in go 0 0 end
fun isTypeDecl t =
  let val t = U.trim t in String.isSubstring "type." t orelse
    (String.isSubstring " type" t andalso not (String.isSubstring "->" t)) end
fun isRelationDecl t =
  String.isSubstring "->" t andalso String.isSubstring "type" t
fun modeSig t =
  let val t = U.trim t
      val t = if U.startsWith "%mode" t then String.extract (t, 5, NONE) else t
  in U.trim t end
fun countModes s =
  let fun go [] n = n
        | go (c::cs) n = go cs (if c = #"+" orelse c = #"-" orelse c = #"*" then n+1 else n)
  in go (String.explode s) 0 end
fun conclusionHead t =
  let val t = U.trim t
      val t = if U.endsWith "." t then String.substring (t, 0, size t - 1) else t
      fun afterArrow s =
        case String.fields (fn c => c = #"-") s of
          xs as _::_ =>
            let val last = List.last xs
                val last = if U.startsWith ">" last then String.extract (last, 1, NONE) else last
            in U.trim last end
          | [] => s
      val tail = if String.isSubstring "->" t then afterArrow t else t
      val name = takeName tail
  in name end
fun arityOfType t =
  let fun go [] n = n
        | go (c::cs) n = go cs (if c = #"-" andalso (null cs orelse hd cs = #">") then n+1 else n)
      val n = go (String.explode t) 0
  in if n = 0 then 0 else n end
fun hasTab s = Char.contains s #"\t"
fun trailingWs s =
  let val n = size s
  in n > 0 andalso Char.isSpace (String.sub (s, n-1)) andalso not (String.sub (s, n-1) = #"\n") end
fun lintFile path =
  let val f = OS.Path.file path
      val body = U.readFile path
      val lines = U.lines body
      val ds = declsFile path
      val (modes, worlds, totals, freezes, asserts) = directives path
      val issues = ref ([]:issue list)
      fun push line sev msg = issues := {file=f, line=line, sev=sev, msg=msg} :: !issues
      val _ = app (fn (n,i) => push i "error" ("duplicate name `" ^ n ^ "`")) (dups ds)
      val _ = app (fn (n,i,t) =>
        if isTypeDecl t then ()
        else if uses body n <= 1 then push i "warn" ("possibly unused `" ^ n ^ "`") else ()) ds
      val _ = app (fn (i,t) =>
        if U.trim t = "%total" orelse U.trim t = "%total." then push i "warn" "empty %total"
        else ()) totals
      val _ = app (fn (i,t) =>
        if U.trim t = "%mode" orelse U.trim t = "%mode." then push i "warn" "empty %mode"
        else ()) modes
      val _ = app (fn (i,t) =>
        if U.trim t = "%worlds" orelse U.trim t = "%worlds." then push i "warn" "empty %worlds"
        else ()) worlds
      val rels = List.mapPartial (fn (n,i,t) => if isRelationDecl t then SOME (n,i,t) else NONE) ds
      val clauses = List.mapPartial (fn (n,i,_) =>
        if String.isSubstring "/" n then SOME (familyOf n, i, n) else NONE) ds
      val _ = app (fn (r,i,_) =>
        let val hasClause = List.exists (fn (fam,_,_) => fam = r) clauses
            val hasTotal = List.exists (fn (_,t) => String.isSubstring r t) totals
            val hasMode = List.exists (fn (_,t) => String.isSubstring r t) modes
            val hasWorld = List.exists (fn (_,t) => String.isSubstring r t) worlds
        in if hasClause andalso not hasTotal then
             push i "error" ("relation `" ^ r ^ "` has clauses but no %total (Twelf will not check coverage)")
           else ();
           if hasClause andalso not hasMode then
             push i "warn" ("relation `" ^ r ^ "` has clauses but no %mode")
           else ();
           if hasClause andalso not hasWorld then
             push i "warn" ("relation `" ^ r ^ "` has clauses but no %worlds")
           else ()
        end) rels
      val _ = app (fn (r,i,t) =>
        let val ar = arityOfType t
            val ms = List.mapPartial (fn (_,mt) =>
              if String.isSubstring r mt then SOME (countModes mt) else NONE) modes
        in case ms of
             m::_ => if ar > 0 andalso m > 0 andalso m <> ar then
                       push i "error" ("relation `" ^ r ^ "` arity " ^ Int.toString ar ^
                                       " vs %mode polarity count " ^ Int.toString m)
                     else ()
           | [] => ()
        end) rels
      val _ = app (fn (fam,i,cn) =>
        let val typ = case List.find (fn (n,_,_) => n = cn) ds of SOME (_,_,t) => t | NONE => ""
            val head = conclusionHead typ
            val hasFam = List.exists (fn (n,_,_) => n = fam) ds
            val hasRel = List.exists (fn (r,_,_) => r = fam) rels
        in if head = fam andalso not hasFam andalso not hasRel then
             push i "error" ("clause `" ^ cn ^ "` without declared family `" ^ fam ^ "`")
           else ()
        end) clauses
      val _ = app (fn (i,t) =>
        if not (String.isSubstring "(" t) then
          push i "warn" ("%total without explicit coverage argument")
        else ()) totals
      val _ = List.foldl (fn (l,i) =>
        (if hasTab l then push i "warn" "tab character (prefer spaces)" else ();
         if size l > 0 andalso Char.isSpace (String.sub (l, size l - 1)) then
           push i "info" "trailing whitespace"
         else ();
         i+1)) 1 lines
      val _ = if null ds andalso null modes andalso null worlds andalso null totals then
                push 1 "warn" "empty elf file"
              else ()
      val orphans = List.filter (fn (n,i,t) =>
        isTypeDecl t andalso uses body n = 1 andalso n <> "type") ds
      val _ = app (fn (n,i,_) => push i "warn" ("orphan type family `" ^ n ^ "` never used")) orphans
  in List.rev (!issues) end
fun lintDir dir =
  let val files = List.filter U.isElf (U.listDir dir)
  in List.concat (map (fn f => lintFile (OS.Path.joinDirFile {dir=dir, file=f})) files) end
fun render issues =
  if null issues then "lint ok\n"
  else String.concat (map (fn (i:issue) =>
    #file i ^ ":" ^ Int.toString (#line i) ^ ": " ^ #sev i ^ ": " ^ #msg i ^ "\n") issues)
fun run dir json =
  let val issues = lintDir dir
  in if json then
       let fun one (i:issue) =
             "{\"file\":\"" ^ #file i ^ "\",\"line\":" ^ Int.toString (#line i) ^
             ",\"sev\":\"" ^ #sev i ^ "\",\"msg\":\"" ^ #msg i ^ "\"}"
       in "[" ^ U.join "," (map one issues) ^ "]\n" end
     else render issues
  end
end
