structure Cfgx = struct
type t = {color:bool, verbose:bool, quiet:bool, registry:string, vendor:string}
val default = {color=true, verbose=false, quiet=false, registry="registry/index.json", vendor="vendor"}
fun load () =
  let val home = case OS.Process.getEnv "TWELF_PM_HOME" of
                   SOME h => h
                 | NONE => (case OS.Process.getEnv "HOME" of
                              SOME h => OS.Path.joinDirFile {dir=h, file=".twelf-pm"}
                            | NONE => "/tmp/twelf-pm")
      val p = OS.Path.joinDirFile {dir=home, file="config.toml"}
  in if not (U.exists p) then default
     else
       let val t = Toml.parse (U.readFile p)
           fun b k d = case Toml.getStr t k of SOME "true" => true | SOME "false" => false | _ => d
           fun s k d = getOpt (Toml.getStr t k, d)
       in {color=b "color" true, verbose=b "verbose" false, quiet=b "quiet" false,
           registry=s "registry" "registry/index.json", vendor=s "vendor" "vendor"}
       end
  end
end
