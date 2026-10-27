structure Lock = struct
type modh = {file:string, sha256:string}
type pkg = {name:string, version:string, source:string, checksum:string}
type t = {version:int, modules:modh list, packages:pkg list, cfgSha:string}
val path = "twelf.lock"
val empty = {version=2, modules=[], packages=[], cfgSha=""}
fun parse body =
  let val ls = U.lines body
      val mods = ref ([]:modh list)
      val pkgs = ref ([]:pkg list)
      val cfg = ref ""
      val ver = ref 2
      fun go [] = {version= !ver, modules=List.rev (!mods), packages=List.rev (!pkgs), cfgSha= !cfg}
        | go (l::ls) =
            let val t = U.trim l
            in if t = "" orelse U.startsWith "#" t then go ls
               else if t = "[[module]]" then
                 (mods := {file="", sha256=""} :: !mods; go ls)
               else if t = "[[package]]" then
                 (pkgs := {name="", version="", source="", checksum=""} :: !pkgs; go ls)
               else case U.split #"=" t of
                 [k,v] =>
                   let val k = U.trim k
                       val v = U.trim v
                       val v = if U.startsWith "\"" v andalso U.endsWith "\"" v
                               then String.substring (v, 1, size v - 2) else v
                   in if k = "lock-version" then ver := getOpt (Int.fromString v, 2)
                      else if k = "cfg-sha256" then cfg := v
                      else if k = "file" then
                        (case !mods of m::ms => mods := {file=v, sha256= #sha256 m}::ms | [] => ())
                      else if k = "sha256" then
                        (case !mods of m::ms => mods := {file= #file m, sha256=v}::ms | [] => ())
                      else if k = "name" then
                        (case !pkgs of p::ps => pkgs := {name=v, version= #version p, source= #source p, checksum= #checksum p}::ps | [] => ())
                      else if k = "version" then
                        (case !pkgs of p::ps => pkgs := {name= #name p, version=v, source= #source p, checksum= #checksum p}::ps | [] => ())
                      else if k = "source" then
                        (case !pkgs of p::ps => pkgs := {name= #name p, version= #version p, source=v, checksum= #checksum p}::ps | [] => ())
                      else if k = "checksum" then
                        (case !pkgs of p::ps => pkgs := {name= #name p, version= #version p, source= #source p, checksum=v}::ps | [] => ())
                      else ();
                      go ls
                   end
               | _ => go ls
            end
  in go ls end
fun load () = if U.exists path then parse (U.readFile path) else empty
fun save (lk:t) =
  let val hdr = "lock-version = " ^ Int.toString (#version lk) ^ "\ncfg-sha256 = \"" ^ #cfgSha lk ^ "\"\n\n"
      val ms = String.concat (map (fn (m:modh) =>
        "[[module]]\nfile = \"" ^ #file m ^ "\"\nsha256 = \"" ^ #sha256 m ^ "\"\n\n") (#modules lk))
      val ps = String.concat (map (fn (p:pkg) =>
        "[[package]]\nname = \"" ^ #name p ^ "\"\nversion = \"" ^ #version p ^
        "\"\nsource = \"" ^ #source p ^ "\"\nchecksum = \"" ^ #checksum p ^ "\"\n\n") (#packages lk))
  in U.writeFile path (hdr ^ ms ^ ps) end
fun moduleHashes dir files =
  map (fn f => {file=f, sha256=Sha.hashFile (OS.Path.joinDirFile {dir=dir, file=f})}) files
fun cfgHash path = if U.exists path then Sha.hashFile path else ""
fun fromProject (m:Man.t) paths =
  let val files = #modules (Man.ensureModules m)
      val dir = #elfDir m
      val cfg = OS.Path.joinDirFile {dir=dir, file="sources.cfg"}
      val pkgs = ListPair.map (fn (d:Man.dep, p) =>
        {name= #name d, version= #version d, source=p, checksum=Sha.hashFile p handle _ => Sha.hashString p})
        (#deps m, paths)
  in {version=2, modules=moduleHashes dir files, packages=pkgs, cfgSha=cfgHash cfg} end
fun verify (m:Man.t) paths frozen =
  let val lk = load ()
      val dir = #elfDir m
      val files = #modules (Man.ensureModules m)
      val cfg = OS.Path.joinDirFile {dir=dir, file="sources.cfg"}
      val errs = ref ([]:string list)
      fun push s = errs := s :: !errs
      val modn = length (List.filter (fn (mh:modh) => #file mh <> "") (#modules lk))
      val _ = if frozen andalso modn = 0 then push "empty lock modules" else ()
      val _ = app (fn (mh:modh) =>
        if #file mh = "" then ()
        else
          let val p = OS.Path.joinDirFile {dir=dir, file= #file mh}
              val h = Sha.hashFile p
          in if h <> #sha256 mh then push (#file mh ^ " sha256 mismatch") else () end) (#modules lk)
      val _ = if U.exists cfg andalso #cfgSha lk <> "" andalso cfgHash cfg <> #cfgSha lk
              then push "sources.cfg sha256 mismatch" else ()
      val _ = if frozen andalso modn <> length files
              then push ("module count mismatch (" ^ Int.toString modn ^ " lock vs " ^ Int.toString (length files) ^ " toml)")
              else ()
  in List.rev (!errs) end
end
