structure Cache = struct
fun dir () =
  let val h = case OS.Process.getEnv "TWELF_PM_HOME" of
                SOME x => x
              | NONE => (case OS.Process.getEnv "HOME" of SOME x => OS.Path.joinDirFile {dir=x, file=".twelf-pm"} | NONE => "/tmp/twelf-pm")
      val d = OS.Path.joinDirFile {dir=h, file="cache"}
      val _ = U.mkdir h
      val _ = U.mkdir d
  in d end
fun keyOfManifest path =
  if U.exists path then Sha.hashFile path else "none"
fun resolvePath key = OS.Path.joinDirFile {dir=dir (), file="resolve-" ^ key}
fun getResolve key =
  let val p = resolvePath key
  in if U.exists p then SOME (U.readFile p) else NONE end
fun putResolve key body = U.writeFile (resolvePath key) body
end
