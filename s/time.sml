structure Tm = struct
fun now () = Time.now ()
fun ms a b = IntInf.toInt (Time.toMilliseconds (Time.- (b, a)))
fun show label t = label ^ ": " ^ Int.toString t ^ "ms"
end
