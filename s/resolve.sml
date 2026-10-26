structure Res = struct
fun order (m:Man.t) =
  let val m = Man.ensureModules m
      val explicit = #modules m
  in if null explicit then
       let val mods = Man.discoverModules (#elfDir m)
           val edges = Cfg.inferEdges mods (#elfDir m)
       in Cfg.topoSort mods edges end
     else explicit
  end
fun generateCfg (m:Man.t) outPath =
  let val ord = order m
      val _ = Cfg.write outPath ord
  in ord end
fun home () =
  case OS.Process.getEnv "TWELF_PM_HOME" of
    SOME h => h
  | NONE => (case OS.Process.getEnv "HOME" of
               SOME h => OS.Path.joinDirFile {dir=h, file=".twelf-pm"}
             | NONE => OS.Path.joinDirFile {dir="/tmp", file="twelf-pm"})
fun cacheDir () =
  let val c = OS.Path.joinDirFile {dir=home (), file="cache"}
      val _ = U.mkdir (home ())
      val _ = U.mkdir c
  in c end
fun vendorDir () = (U.mkdir "vendor"; "vendor")
fun isLocalPath s =
  U.startsWith "/" s orelse U.startsWith "./" s orelse U.startsWith "../" s
fun installDep (d:Man.dep) =
  if isLocalPath (#source d) orelse isLocalPath (#name d) then
    let val src = if isLocalPath (#source d) then #source d else #name d
    in if U.exists src then src else U.die ("offline missing " ^ src)
    end
  else
    case Reg.resolvePkg (#name d) (#version d) of
      SOME p =>
        if U.exists p then p
        else
          let val dest = OS.Path.joinDirFile {dir=cacheDir (), file= #name d ^ "-" ^ #version d}
              val _ = U.mkdir dest
              val _ = U.writeFile (OS.Path.joinDirFile {dir=dest, file="PACKAGE"})
                        (#name d ^ "\n" ^ #version d ^ "\nlocal\n")
          in dest end
    | NONE =>
        let val dest = OS.Path.joinDirFile {dir=cacheDir (), file= #name d ^ "-" ^ #version d}
            val _ = U.mkdir dest
            val _ = U.writeFile (OS.Path.joinDirFile {dir=dest, file="PACKAGE"})
                      (#name d ^ "\n" ^ #version d ^ "\nlocal\n")
        in dest end
fun installAll (m:Man.t) (opts:Opt.t) =
  let val paths = map installDep (#deps m)
      val cs = Ns.conflicts (#elfDir m) paths
      val _ = U.out (Ns.report cs)
      val _ = if #strict opts andalso not (null cs) then U.die "ns: strict conflict" else ()
  in if #rewrite opts andalso not (null cs) then
       let val (paths', _) = Ns.checkAndPrefix m paths in paths' end
     else paths
  end
fun vendorize name path =
  let val dest = OS.Path.joinDirFile {dir=vendorDir (), file=name}
      val _ = if U.exists dest then () else U.mkdir dest
      val _ = OS.Process.system ("cp -a " ^ path ^ "/. " ^ dest ^ "/")
      val _ = Reg.publish name "0.1.0" dest ""
  in dest end
end
