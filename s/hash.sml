structure Hash = struct
fun fnv1a s =
  let val cs = String.explode s
      fun go [] h = h
        | go (c::t) h =
            let val h = Word.xorb (h, Word.fromInt (Char.ord c))
                val h = Word.*(h, Word.fromInt 16777619)
            in go t h end
  in Word.toString (go cs (Word.fromInt 2166136261)) end
fun file p = if U.exists p then fnv1a (U.readFile p) else "0"
fun dir d =
  let val fs = List.filter U.isElf (U.listDir d) handle _ => []
      val xs = map (fn f => f ^ ":" ^ file (OS.Path.joinDirFile {dir=d, file=f})) fs
  in fnv1a (U.join ";" xs) end
end
