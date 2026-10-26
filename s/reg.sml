structure Reg = struct
type entry = {name:string, version:string, path:string, desc:string}
fun indexPaths () =
  let val home = case OS.Process.getEnv "TWELF_PM_HOME" of
                   SOME h => h
                 | NONE => (case OS.Process.getEnv "HOME" of SOME h => OS.Path.joinDirFile {dir=h, file=".twelf-pm"} | NONE => "/tmp/twelf-pm")
  in ["registry/index.json", "vendor/index.json", OS.Path.joinDirFile {dir=home, file="index.json"}] end
fun parseEntry line =
  let val t = U.trim line
  in if t = "" orelse U.startsWith "#" t then NONE
     else case U.split #"|" t of
            [n, v, p] => SOME {name=n, version=v, path=p, desc=""}
          | [n, v, p, d] => SOME {name=n, version=v, path=p, desc=d}
          | [n, p] => SOME {name=n, version="*", path=p, desc=""}
          | _ => NONE
  end
fun loadIndex path =
  if not (U.exists path) then []
  else List.mapPartial parseEntry (U.lines (U.readFile path))
fun allEntries () = List.concat (map loadIndex (indexPaths ()))
fun find name versionSpec =
  let val es = allEntries ()
      val hits = List.filter (fn (e:entry) =>
        #name e = name andalso (versionSpec = "*" orelse Sem.satisfies versionSpec (#version e))) es
      fun better (a:entry, b:entry) = Sem.cmp (Sem.parse (#version a), Sem.parse (#version b)) = GREATER
      fun best [] = NONE | best (x::xs) = SOME (List.foldl (fn (e,acc) => if better (e,acc) then e else acc) x xs)
  in best hits end
fun hasElf dir =
  List.exists U.isElf (U.listDir dir) handle _ => false
fun validatePublish name version path =
  let val errs = ref ([]:string list)
      fun push s = errs := s :: !errs
  in if name = "" then push "empty name" else ();
     if not (Sem.valid version) then push ("bad semver " ^ version) else ();
     if not (U.exists path) then push ("missing path " ^ path) else ();
     if U.exists path andalso not (hasElf path) then push ("no .elf in " ^ path) else ();
     List.rev (!errs)
  end
fun resolvePkg name version =
  case find name version of
    SOME (e:entry) =>
      if U.exists (#path e) then SOME (#path e)
      else if U.exists ("vendor/" ^ name) then SOME ("vendor/" ^ name)
      else SOME (#path e)
  | NONE =>
      if U.exists ("vendor/" ^ name) then SOME ("vendor/" ^ name)
      else if U.exists name then SOME name
      else NONE
fun writeLocalIndex (entries:entry list) path =
  let val body = String.concat (map (fn (e:entry) =>
        #name e ^ "|" ^ #version e ^ "|" ^ #path e ^ "|" ^ #desc e ^ "\n") entries)
      val dir = OS.Path.dir path handle _ => "registry"
      val _ = U.mkdir dir
  in U.writeFile path body end
fun publish name version path desc =
  case validatePublish name version path of
    e::_ => U.die ("publish: " ^ U.join "; " (validatePublish name version path))
  | [] =>
      let val e = {name=name, version=version, path=path, desc=desc}
          val idx = "registry/index.json"
          val cur = List.filter (fn (x:entry) => not (#name x = name andalso #version x = version)) (loadIndex idx)
      in writeLocalIndex (e::cur) idx; idx end
end
