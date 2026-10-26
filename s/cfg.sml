structure Cfg = struct
fun read path =
  if U.exists path then
    List.filter (fn l => l <> "" andalso not (U.startsWith "%" l) andalso not (U.startsWith "#" l))
      (map U.trim (U.lines (U.readFile path)))
  else []
fun write path modules =
  let val modules = List.filter U.isElf modules
  in U.writeFile path (U.join "\n" modules ^ "\n") end
fun fromSources path = read path
fun topoSort modules deps =
  let fun indep (m:string) = not (List.exists (fn (a:string,b:string) => b = m andalso List.exists (fn (x:string) => x = a) modules) deps)
      fun go [] acc = List.rev acc
        | go rest acc =
            case List.partition indep rest of
              ([], _) => List.rev acc @ rest
            | (ready, wait) => go wait (List.rev ready @ acc)
  in go modules [] end
fun inferEdges modules dir =
  let fun refs m =
        let val p = OS.Path.joinDirFile {dir=dir, file=m}
            val body = if U.exists p then U.readFile p else ""
            val stem = if size m > 4 then String.substring (m, 0, size m - 4) else m
            fun hit other = other <> m andalso String.isSubstring stem body
        in List.filter hit modules end
      fun edges [] = []
        | edges (m::ms) = map (fn other => (other, m)) (refs m) @ edges ms
  in edges modules end
end
