structure Fmt = struct
fun normalizeSpace s =
  let val cs = String.explode s
      fun go [] acc = List.rev acc
        | go (#" ":: #" "::t) acc = go (#" "::t) acc
        | go (c::t) acc = go t (c::acc)
  in String.implode (go cs []) end
fun fmtDecl line =
  let val t = U.trim line
  in if t = "" then ""
     else if U.startsWith "%" t then t
     else if String.isSubstring ":" t then
       case U.split #":" t of
         [l, r] => U.trim l ^ " : " ^ U.trim r
       | _ => t
     else t
  end
fun formatFile path =
  let val ls = U.lines (U.readFile path)
      val out = map fmtDecl ls
      val body = U.join "\n" out ^ "\n"
  in U.writeFile path body; body end
fun formatDir dir =
  let val files = List.filter U.isElf (U.listDir dir)
  in map (fn f => formatFile (OS.Path.joinDirFile {dir=dir, file=f})) files end
end
