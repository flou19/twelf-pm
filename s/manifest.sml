structure Man = struct
type dep = {name:string, version:string, source:string}
type t = {name:string, version:string, modules:string list, deps:dep list, elfDir:string}
fun empty name = {name=name, version="0.1.0", modules=[], deps=[], elfDir="t"}
fun parseDep s =
  if U.startsWith "/" s orelse U.startsWith "./" s orelse U.startsWith "../" s then
    {name=s, version="local", source=s}
  else case U.split #"@" s of
    [n, v] => {name=n, version=v, source="local"}
  | [n] => {name=n, version="*", source="local"}
  | n::v::src::_ => {name=n, version=v, source=src}
  | _ => {name=s, version="*", source="local"}
fun load path =
  let val t = Toml.parse (U.readFile path)
      val name = getOpt (Toml.getStr t "package.name", "unnamed")
      val version = getOpt (Toml.getStr t "package.version", "0.1.0")
      val elfDir = getOpt (Toml.getStr t "package.elf-dir", "t")
      val modules = Toml.getStrList t "package.modules"
      val deps = map parseDep (Toml.getStrList t "dependencies.list")
  in {name=name, version=version, modules=modules, deps=deps, elfDir=elfDir} end
fun save path (m:t) =
  let val body =
        "[package]\n" ^
        "name = \"" ^ #name m ^ "\"\n" ^
        "version = \"" ^ #version m ^ "\"\n" ^
        "elf-dir = \"" ^ #elfDir m ^ "\"\n" ^
        "modules = [" ^ U.join ", " (map (fn x => "\"" ^ x ^ "\"") (#modules m)) ^ "]\n\n" ^
        "[dependencies]\n" ^
        "list = [" ^ U.join ", " (map (fn d =>
          if #source d <> "local" andalso #source d <> "registry"
          then "\"" ^ #source d ^ "\""
          else "\"" ^ #name d ^ "@" ^ #version d ^ "\"") (#deps m)) ^ "]\n"
  in U.writeFile path body end
fun isort xs =
  let fun ins (x:string) [] = [x]
        | ins x (y::ys) = if String.<= (x, y) then x::y::ys else y::ins x ys
      fun go [] = [] | go (x::xs) = ins x (go xs)
  in go xs end
fun readCfgOrder dir =
  let val cfg = OS.Path.joinDirFile {dir=dir, file="sources.cfg"}
  in if not (U.exists cfg) then []
     else List.filter (fn l => l <> "" andalso not (U.startsWith "%" l) andalso not (U.startsWith "#" l))
            (map U.trim (U.lines (U.readFile cfg)))
  end
fun discoverModules dir =
  let val fromCfg = readCfgOrder dir
  in if not (null fromCfg) then fromCfg
     else if U.exists dir then isort (List.filter U.isElf (U.listDir dir))
     else []
  end
fun ensureModules (m:t) =
  case #modules m of
    [] => {name= #name m, version= #version m,
           modules= discoverModules (#elfDir m),
           deps= #deps m, elfDir= #elfDir m}
  | _ => m
end
