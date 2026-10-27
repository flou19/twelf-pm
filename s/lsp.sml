structure Lsp = struct
fun isIdent c = Char.isAlphaNum c orelse c = #"_" orelse c = #"'"
fun symbols dir =
  let val files = List.filter U.isElf (U.listDir dir)
      fun fromFile f =
        let val p = OS.Path.joinDirFile {dir=dir, file=f}
            val ls = U.lines (U.readFile p)
            fun decl (i, l) =
              let val t = U.trim l
              in if t = "" orelse U.startsWith "%" t then NONE
                 else case String.tokens (fn c => c = #":" orelse Char.isSpace c) t of
                        n::_ => if Char.isAlpha (String.sub (n, 0)) handle _ => false
                                then SOME (n, f, t, i) else NONE
                      | _ => NONE
              end
            fun go i [] = [] | go i (l::ls) = (case decl (i,l) of SOME x => x::go (i+1) ls | NONE => go (i+1) ls)
        in go 1 ls end
  in List.concat (map fromFile files) end
fun complete prefix dir = map #1 (List.filter (fn (n,_,_,_) => String.isPrefix prefix n) (symbols dir))
fun definition name dir =
  case List.find (fn (n,_,_,_) => n = name) (symbols dir) of
    SOME (_, f, _, line) => SOME (f ^ ":" ^ Int.toString line) | NONE => NONE
fun hover name dir =
  case List.find (fn (n,_,_,_) => n = name) (symbols dir) of
    SOME (n, f, t, line) => SOME ("name: " ^ n ^ "\nfile: " ^ f ^ ":" ^ Int.toString line ^ "\ndecl: " ^ t)
  | NONE => NONE
fun workspaceSymbols query dir =
  map (fn (n,f,t,line) => n ^ "\t" ^ f ^ ":" ^ Int.toString line ^ "\t" ^ t)
    (List.filter (fn (n,_,_,_) => query = "" orelse String.isSubstring query n) (symbols dir))
fun diagnose dir =
  let val cfg = OS.Path.joinDirFile {dir=dir, file="sources.cfg"}
      val (_, out) = if U.exists cfg then Build.runTwelf dir else (false, "no sources.cfg")
      val issues = Dbg.parseOutput out
  in map (fn (i:Dbg.issue) =>
        #file i ^ ":" ^ Int.toString (#line i) ^ ":" ^ Dbg.showKind (#kind i) ^ ":" ^ #msg i) issues
  end
fun rewriteIdent old new body =
  let val n = size old
      val cs = String.explode body
      fun isB i = i < 0 orelse not (isIdent (String.sub (body, i)))
      fun isE i = i >= size body orelse not (isIdent (String.sub (body, i)))
      fun go i acc =
        if i >= size body then String.implode (List.rev acc)
        else if i + n <= size body andalso String.substring (body, i, n) = old andalso isB (i-1) andalso isE (i+n)
        then go (i+n) (List.rev (String.explode new) @ acc)
        else go (i+1) (String.sub (body, i)::acc)
  in go 0 [] end
fun rename old new dirs =
  let val files = List.concat (map (fn d =>
        if U.exists d then map (fn f => OS.Path.joinDirFile {dir=d, file=f}) (List.filter U.isElf (U.listDir d))
        else []) dirs)
      val count = ref 0
      fun one p =
        let val body = U.readFile p
            val body' = rewriteIdent old new body
        in if body' <> body then (U.writeFile p body'; count := !count + 1) else () end
      val _ = app one files
  in "renamed " ^ old ^ " -> " ^ new ^ " in " ^ Int.toString (!count) ^ " files" end
fun serve () =
  let fun loop () =
        case TextIO.inputLine TextIO.stdIn of
          NONE => ()
        | SOME l =>
            let val t = U.trim l
                val r =
                  if U.startsWith "COMPLETE " t then U.join "\n" (complete (String.extract (t, 9, NONE)) "t")
                  else if U.startsWith "DEF " t then (case definition (String.extract (t, 4, NONE)) "t" of SOME f => f | NONE => "")
                  else if U.startsWith "HOVER " t then (case hover (String.extract (t, 6, NONE)) "t" of SOME s => s | NONE => "")
                  else if U.startsWith "SYMBOLS" t then
                    let val q = if size t > 8 then String.extract (t, 8, NONE) else ""
                    in U.join "\n" (workspaceSymbols q "t") end
                  else if U.startsWith "RENAME " t then
                    let val rest = String.extract (t, 7, NONE)
                        val parts = String.tokens Char.isSpace rest
                    in case parts of
                         [old, new] => rename old new ["t", "vendor"]
                       | _ => "usage: RENAME old new"
                    end
                  else if t = "DIAG" then U.join "\n" (diagnose "t")
                  else if t = "QUIT" then (OS.Process.exit OS.Process.success; "")
                  else "?"
            in U.outln r; loop () end
  in loop () end
end
