structure U = struct
fun readFile p =
  let val ins = TextIO.openIn p
      val s = TextIO.inputAll ins
      val _ = TextIO.closeIn ins
  in s end
fun writeFile p s =
  let val outs = TextIO.openOut p
      val _ = TextIO.output (outs, s)
      val _ = TextIO.closeOut outs
  in () end
fun exists p = (OS.FileSys.access (p, []) handle _ => false)
fun mkdir p = if exists p then () else OS.FileSys.mkDir p
fun lines s = String.tokens (fn c => c = #"\n") s
fun trim s =
  let fun l (#" "::t) = l t | l (#"\t"::t) = l t | l xs = xs
      fun r [] = [] | r [x] = if Char.isSpace x then [] else [x]
        | r (x::xs) = let val ys = r xs in if ys = [] andalso Char.isSpace x then [] else x::ys end
  in String.implode (r (l (String.explode s))) end
fun startsWith p s = String.isPrefix p s
fun endsWith p s = String.isSuffix p s
fun split c s = String.tokens (fn x => x = c) s
fun join sep xs = String.concatWith sep xs
fun die m = (TextIO.output (TextIO.stdErr, m ^ "\n"); OS.Process.exit OS.Process.failure)
fun out s = TextIO.output (TextIO.stdOut, s)
fun outln s = out (s ^ "\n")
fun listDir d =
  let val ds = OS.FileSys.openDir d
      fun go acc = case OS.FileSys.readDir ds of
                     NONE => (OS.FileSys.closeDir ds; List.rev acc)
                   | SOME n => go (n::acc)
  in go [] end handle _ => []
fun isElf n = endsWith ".elf" n
fun isCfg n = n = "sources.cfg"
fun mtime p = OS.FileSys.modTime p handle _ => Time.zeroTime
fun newer a b = Time.> (mtime a, mtime b)
end
