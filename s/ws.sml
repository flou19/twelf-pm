structure Ws = struct
fun members () =
  if not (U.exists "workspace") then []
  else
    let val ds = U.listDir "workspace"
        fun hasToml d =
          U.exists (OS.Path.joinDirFile {
            dir=OS.Path.joinDirFile {dir="workspace", file=d},
            file="twelf.toml"})
    in List.filter hasToml ds end
fun buildOne root force d =
  let val dir = OS.Path.joinDirFile {dir="workspace", file=d}
      val pm = OS.Path.joinDirFile {dir=root, file="bin/twelf-pm"}
      val fl = if force then " --force" else ""
      val cmd = "cd " ^ dir ^ " && bash " ^ pm ^ " build" ^ fl
      val st = OS.Process.system cmd
  in U.outln ("workspace " ^ d ^ (if OS.Process.isSuccess st then " ok" else " fail"));
     OS.Process.isSuccess st
  end
fun buildAll force =
  let val root = OS.FileSys.getDir ()
      val ms = members ()
      val _ = if null ms then U.outln "workspace: empty" else ()
      val results = map (buildOne root force) ms
  in List.all (fn x => x) results end
fun checkAll () =
  let val root = OS.FileSys.getDir ()
      val pm = OS.Path.joinDirFile {dir=root, file="bin/twelf-pm"}
      fun one d =
        let val dir = OS.Path.joinDirFile {dir="workspace", file=d}
            val st = OS.Process.system ("cd " ^ dir ^ " && bash " ^ pm ^ " check")
        in OS.Process.isSuccess st end
  in List.all one (members ()) end
end
