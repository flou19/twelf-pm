structure Lsp = struct
fun symbols dir =
  let val files = List.filter U.isElf (U.listDir dir)
      fun fromFile f =
        let val p = OS.Path.joinDirFile {dir=dir, file=f}
            val ls = U.lines (U.readFile p)
            fun decl l =
              let val t = U.trim l
              in if t = "" orelse U.startsWith "%" t then NONE
                 else case String.tokens (fn c => c = #":" orelse Char.isSpace c) t of
                        n::_ => if Char.isAlpha (String.sub (n, 0)) handle _ => false
                                then SOME (n, f, t) else NONE
                      | _ => NONE
              end
        in List.mapPartial decl ls end
  in List.concat (map fromFile files) end
fun complete prefix dir =
  let val syms = symbols dir
  in map #1 (List.filter (fn (n,_,_) => String.isPrefix prefix n) syms) end
fun definition name dir =
  case List.find (fn (n,_,_) => n = name) (symbols dir) of
    SOME (_, f, _) => SOME f | NONE => NONE
fun hover name dir =
  case List.find (fn (n,_,_) => n = name) (symbols dir) of
    SOME (n, f, t) => SOME ("name: " ^ n ^ "\nfile: " ^ f ^ "\ndecl: " ^ t)
  | NONE => NONE
fun workspaceSymbols query dir =
  let val syms = symbols dir
      val q = query
  in map (fn (n,f,t) => n ^ "\t" ^ f ^ "\t" ^ t)
       (List.filter (fn (n,_,_) => q = "" orelse String.isSubstring q n) syms)
  end
fun diagnose dir =
  let val cfg = OS.Path.joinDirFile {dir=dir, file="sources.cfg"}
      val (_, out) = if U.exists cfg then Build.runTwelf dir else (false, "no sources.cfg")
  in Dbg.parseOutput out end
fun serve () =
  let fun loop () =
        case TextIO.inputLine TextIO.stdIn of
          NONE => ()
        | SOME l =>
            let val t = U.trim l
                val r =
                  if U.startsWith "COMPLETE " t then
                    U.join "\n" (complete (String.extract (t, 9, NONE)) "t")
                  else if U.startsWith "DEF " t then
                    (case definition (String.extract (t, 4, NONE)) "t" of SOME f => f | NONE => "")
                  else if U.startsWith "HOVER " t then
                    (case hover (String.extract (t, 6, NONE)) "t" of SOME s => s | NONE => "")
                  else if U.startsWith "SYMBOLS" t then
                    let val q = if size t > 8 then String.extract (t, 8, NONE) else ""
                    in U.join "\n" (workspaceSymbols q "t") end
                  else if t = "DIAG" then Dbg.render (diagnose "t")
                  else if t = "QUIT" then (OS.Process.exit OS.Process.success; "")
                  else "?"
            in U.outln r; loop () end
  in loop () end
end
