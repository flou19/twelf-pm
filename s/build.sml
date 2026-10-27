structure Build = struct
fun stampPath elfDir = OS.Path.joinDirFile {dir=elfDir, file=".ok"}
fun hashModules modules dir =
  let fun h f = f ^ ":" ^ Sha.hashFile (OS.Path.joinDirFile {dir=dir, file=f})
  in Sha.hashString (U.join ";" (map h modules)) end
fun cachePath elfDir = OS.Path.joinDirFile {dir=elfDir, file=".buildhash"}
fun needsRebuild (m:Man.t) =
  let val mods = #modules (Man.ensureModules m)
      val h = hashModules mods (#elfDir m)
      val old = if U.exists (cachePath (#elfDir m)) then U.readFile (cachePath (#elfDir m)) else ""
  in h <> old end
fun writeHash (m:Man.t) =
  let val mods = #modules (Man.ensureModules m)
  in U.writeFile (cachePath (#elfDir m)) (hashModules mods (#elfDir m)) end
fun twelfBin () =
  case OS.Process.getEnv "TWELF_SERVER" of
    SOME p => p
  | NONE => if U.exists "/usr/local/bin/twelf-server" then "/usr/local/bin/twelf-server" else "twelf-server"
fun runTwelf cfgDir =
  let val tw = twelfBin ()
      val cmd = "cd " ^ cfgDir ^ " && printf 'set chatter 1\\nmake sources.cfg\\nquit\\n' | " ^ tw ^ " 2>&1"
      val tmp = OS.FileSys.tmpName ()
      val st = OS.Process.system (cmd ^ " > " ^ tmp)
      val out = U.readFile tmp handle _ => ""
      val _ = OS.FileSys.remove tmp handle _ => ()
  in (String.isSubstring "%% OK %%" out andalso not (String.isSubstring "%% ABORT %%" out), out) end
fun build (m:Man.t) force =
  let val m = Man.ensureModules m
      val cfg = OS.Path.joinDirFile {dir= #elfDir m, file="sources.cfg"}
      val order = Res.generateCfg m cfg
      val _ = U.outln ("cfg " ^ Int.toString (length order) ^ " modules")
      val rebuild = force orelse needsRebuild m orelse not (U.exists (stampPath (#elfDir m)))
  in if not rebuild then (U.outln "fresh"; true)
     else
       let val (ok, out) = runTwelf (#elfDir m)
           val _ = U.out out
           val issues = Dbg.parseOutput out
           val _ = if null issues then () else U.out (Dbg.render issues)
       in if ok then
            (U.writeFile (stampPath (#elfDir m)) "OK\n"; writeHash m;
             let val paths = map Res.installDep (#deps m)
                 val _ = Lock.save (Lock.fromProject m paths)
             in () end;
             U.outln "ok"; true)
          else (if U.exists (stampPath (#elfDir m)) then OS.FileSys.remove (stampPath (#elfDir m)) handle _ => () else ();
                U.outln "fail"; false)
       end
  end
end
