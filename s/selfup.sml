structure SelfUp = struct
fun rootOf () =
  let val home = case OS.Process.getEnv "TWELF_PM_ROOT" of
                   SOME r => r
                 | NONE =>
                     let val s = OS.FileSys.getDir ()
                     in if U.exists (OS.Path.joinDirFile {dir=s, file="bin/twelf-pm"}) then s
                        else case OS.Process.getEnv "HOME" of
                               SOME h => OS.Path.joinDirFile {dir=h, file="src/twelf-pm"}
                             | NONE => s
                     end
  in home end
fun update () =
  let val root = case OS.Process.getEnv "TWELF_PM_ROOT" of
                   SOME r => r
                 | NONE =>
                     (* discover from script location is handled by launcher; fallback *)
                     if U.exists "bin/twelf-pm" then OS.FileSys.getDir ()
                     else U.die "update --self: set TWELF_PM_ROOT to the twelf-pm checkout"
      val git = OS.Path.joinDirFile {dir=root, file=".git"}
      val _ = if not (U.exists git) then U.die "update --self: not a git checkout" else ()
      val st = OS.Process.system ("cd " ^ root ^ " && git pull --ff-only")
  in if OS.Process.isSuccess st then U.outln ("updated " ^ root)
     else U.die "update --self: git pull failed (offline or diverged)"
  end
end
