structure Hook = struct
fun install () =
  let val dir = ".git/hooks"
      val _ = if U.exists ".git" then U.mkdir dir else ()
      val body =
        "#!/bin/sh\n" ^
        "bash bin/twelf-pm fmt >/dev/null 2>&1 || true\n" ^
        "bash bin/twelf-pm ns --strict || exit 1\n" ^
        "bash bin/twelf-pm build --locked || exit 1\n"
      val path = OS.Path.joinDirFile {dir=dir, file="pre-commit"}
  in if U.exists ".git" then
       (U.writeFile path body; OS.Process.system ("chmod +x " ^ path); U.outln "hook installed")
     else U.outln "hook: no .git"
  end
end
