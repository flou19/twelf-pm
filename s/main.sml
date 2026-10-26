structure Main = struct
fun main (_, args) = (Cli.main args; OS.Process.success)
end
