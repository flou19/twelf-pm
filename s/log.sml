structure Log = struct
val verbose = ref false
val quiet = ref false
fun setV v = verbose := v
fun setQ q = quiet := q
fun info s = if not (!quiet) then U.outln s else ()
fun verb s = if !verbose andalso not (!quiet) then U.outln (Color.dim ("+ " ^ s)) else ()
fun err s = TextIO.output (TextIO.stdErr, Color.red s ^ "\n")
end
