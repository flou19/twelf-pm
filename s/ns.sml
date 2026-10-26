structure Ns = struct
fun isIdent c = Char.isAlphaNum c orelse c = #"_" orelse c = #"'"
fun decls body =
  let val ls = U.lines body
      fun takeName s =
        let val cs = String.explode (U.trim s)
            fun go [] acc = String.implode (List.rev acc)
              | go (c::t) acc = if isIdent c then go t (c::acc) else String.implode (List.rev acc)
        in go cs [] end
      fun fromLine l =
        let val t = U.trim l
        in if t = "" orelse U.startsWith "%" t orelse U.startsWith "%" t then NONE
           else if String.isSubstring ":" t then
             let val n = takeName t
             in if n = "" then NONE else SOME n end
           else NONE
        end
  in List.mapPartial fromLine ls end
fun declsFile path = if U.exists path then decls (U.readFile path) else []
fun declsDir dir =
  let val files = List.filter U.isElf (U.listDir dir)
  in List.concat (map (fn f => map (fn n => (n, f)) (declsFile (OS.Path.joinDirFile {dir=dir, file=f}))) files) end
fun intersect a b =
  List.filter (fn (n:string,_) => List.exists (fn (m:string,_) => n = m) b) a
fun conflicts projectDir depDirs =
  let val proj = declsDir projectDir
      fun go [] = []
        | go (d::ds) =
            let val dep = declsDir d
                val hit = intersect proj dep
            in map (fn (n,f) => (n, f, d)) hit @ go ds
            end
  in go depDirs end
fun report cs =
  if null cs then "ns: clean\n"
  else "ns: CONFLICTS\n" ^
    String.concat (map (fn (n,f,d) => "  " ^ n ^ " in " ^ f ^ " vs " ^ d ^ "\n") cs)
fun prefixName pref n = pref ^ "_" ^ n
fun rewriteLine pref names line =
  let val t = U.trim line
  in if t = "" orelse U.startsWith "%" t then line
     else
       let fun repl token =
             if List.exists (fn n => n = token) names then prefixName pref token else token
           val cs = String.explode line
           fun go [] acc = String.implode (List.rev acc)
             | go (c::t) acc =
                 if isIdent c then
                   let fun take [] a = (String.implode (List.rev a), [])
                         | take (x::xs) a = if isIdent x then take xs (x::a) else (String.implode (List.rev a), x::xs)
                       val (tok, rest) = take (c::t) []
                   in go rest (List.rev (String.explode (repl tok)) @ acc) end
                 else go t (c::acc)
       in go cs [] end
  end
fun prefixFile pref src dest =
  let val names = declsFile src
      val ls = U.lines (U.readFile src)
      val out = map (rewriteLine pref names) ls
  in U.writeFile dest (U.join "\n" out ^ "\n"); length names end
fun prefixDir pref srcDir destDir =
  let val _ = U.mkdir destDir
      val files = List.filter U.isElf (U.listDir srcDir)
      fun one f =
        let val n = prefixFile pref
              (OS.Path.joinDirFile {dir=srcDir, file=f})
              (OS.Path.joinDirFile {dir=destDir, file=pref ^ "__" ^ f})
        in (pref ^ "__" ^ f, n) end
  in map one files end
fun checkAndPrefix (m:Man.t) depPaths =
  let val cs = conflicts (#elfDir m) depPaths
      val _ = U.out (report cs)
  in if null cs then (depPaths, [])
     else
       let fun handleOne d =
             let val pref = String.map (fn c => if Char.isAlphaNum c then c else #"_")
                             (OS.Path.file d handle _ => "pkg")
                 val dest = OS.Path.joinDirFile {dir="t", file=(".ns_" ^ pref)}
                 val files = prefixDir pref d dest
             in (dest, files) end
           val rewritten = map handleOne depPaths
       in (map #1 rewritten, List.concat (map #2 rewritten)) end
  end
end
