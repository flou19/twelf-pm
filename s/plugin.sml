structure Plug = struct
fun dir () =
  case OS.Process.getEnv "TWELF_PM_HOME" of
    SOME h => OS.Path.joinDirFile {dir=h, file="plugins"}
  | NONE =>
      (case OS.Process.getEnv "HOME" of
         SOME h => OS.Path.joinDirFile {dir=OS.Path.joinDirFile {dir=h, file=".twelf-pm"}, file="plugins"}
       | NONE => "/tmp/twelf-pm/plugins")
fun list () =
  let val d = dir ()
      val _ = U.mkdir d
  in U.listDir d handle _ => [] end
fun run name args =
  let val p = OS.Path.joinDirFile {dir=dir (), file=name}
      val sh = OS.Path.joinDirFile {dir=p, file="run.sh"}
  in if U.exists sh then
       (OS.Process.system ("sh " ^ sh ^ " " ^ U.join " " args); ())
     else U.die ("plugin missing " ^ name)
  end
end
