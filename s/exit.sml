structure Ex = struct
val ok = 0
val build = 1
val config = 2
val ns = 3
val lock = 4
val registry = 5
fun quit code = OS.Process.exit (if code = 0 then OS.Process.success else OS.Process.failure)
fun die code msg = (TextIO.output (TextIO.stdErr, msg ^ "\n"); quit code)
end
