structure Doc = struct
type decl = {name:string, sort:string, file:string, mode:string, worlds:string, total:string}
fun collectFile path =
  let val ls = U.lines (U.readFile path)
      val f = OS.Path.file path
      fun takeName s =
        let fun go [] a = String.implode (List.rev a)
              | go (c::t) a = if Char.isAlphaNum c orelse c = #"_" orelse c = #"'" then go t (c::a)
                              else String.implode (List.rev a)
        in go (String.explode (U.trim s)) [] end
      fun scan [] acc = List.rev acc
        | scan (l::ls) acc =
            let val t = U.trim l
            in if U.startsWith "%mode" t then
                 case acc of d::ds => scan ls ({name= #name d, sort= #sort d, file= #file d,
                   mode=t, worlds= #worlds d, total= #total d}::ds)
                 | [] => scan ls acc
               else if U.startsWith "%worlds" t then
                 case acc of d::ds => scan ls ({name= #name d, sort= #sort d, file= #file d,
                   mode= #mode d, worlds=t, total= #total d}::ds)
                 | [] => scan ls acc
               else if U.startsWith "%total" t then
                 case acc of d::ds => scan ls ({name= #name d, sort= #sort d, file= #file d,
                   mode= #mode d, worlds= #worlds d, total=t}::ds)
                 | [] => scan ls acc
               else if String.isSubstring ":" t andalso not (U.startsWith "%" t) then
                 let val n = takeName t
                 in if n = "" then scan ls acc
                    else scan ls ({name=n, sort=t, file=f, mode="", worlds="", total=""}::acc)
                 end
               else scan ls acc
            end
  in scan ls [] end
fun collectDir dir =
  List.concat (map (fn f => collectFile (OS.Path.joinDirFile {dir=dir, file=f}))
    (List.filter U.isElf (U.listDir dir)))
fun html decls outDir =
  let val _ = U.mkdir outDir
      fun row (d:decl) =
        "<tr><td><code>" ^ #name d ^ "</code></td><td><code>" ^ #sort d ^
        "</code></td><td>" ^ #file d ^ "</td><td>" ^ #mode d ^ "</td><td>" ^
        #worlds d ^ "</td><td>" ^ #total d ^ "</td></tr>\n"
      val body =
        "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>twelf-pm doc</title>" ^
        "<style>body{font-family:monospace;background:#0b0d10;color:#c9d1d9;padding:2rem}" ^
        "table{border-collapse:collapse;width:100%}td,th{border:1px solid #30363d;padding:.4rem}" ^
        "th{background:#161b22;text-align:left}code{color:#79c0ff}</style></head><body>" ^
        "<h1>twelf-pm documentation</h1><table><tr><th>name</th><th>decl</th><th>file</th>" ^
        "<th>mode</th><th>worlds</th><th>total</th></tr>\n" ^
        String.concat (map row decls) ^ "</table></body></html>\n"
  in U.writeFile (OS.Path.joinDirFile {dir=outDir, file="index.html"}) body end
fun latex decls outDir =
  let val _ = U.mkdir outDir
      fun esc s =
        let fun e #"_" = "\\_" | e #"{" = "\\{" | e #"}" = "\\}" | e #"%" = "\\%"
              | e #"&" = "\\&" | e #"#" = "\\#" | e c = str c
        in String.concat (map e (String.explode s)) end
      fun row (d:decl) =
        "\\texttt{" ^ esc (#name d) ^ "} & \\texttt{" ^ esc (#sort d) ^ "} & " ^
        esc (#file d) ^ " \\\\\n"
      val body =
        "\\section*{Twelf Declarations}\n\\begin{tabular}{lll}\n" ^
        "\\textbf{Name} & \\textbf{Declaration} & \\textbf{File} \\\\\n\\hline\n" ^
        String.concat (map row decls) ^ "\\end{tabular}\n"
  in U.writeFile (OS.Path.joinDirFile {dir=outDir, file="decls.tex"}) body end
fun run format outDir dir =
  let val decls = collectDir dir
  in case format of
       "html" => (html decls outDir; U.outln ("doc html " ^ Int.toString (length decls)))
     | "latex" => (latex decls outDir; U.outln ("doc latex " ^ Int.toString (length decls)))
     | _ => (html decls outDir; latex decls outDir;
             U.outln ("doc both " ^ Int.toString (length decls)))
  end
end
