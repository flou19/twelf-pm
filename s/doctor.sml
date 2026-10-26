structure Doctor = struct
fun has cmd =
  OS.Process.isSuccess (OS.Process.system ("command -v " ^ cmd ^ " >/dev/null 2>&1"))
fun versionSml () =
  let val tmp = OS.FileSys.tmpName ()
      val _ = OS.Process.system ("sml </dev/null >" ^ tmp ^ " 2>&1 || true")
      val s = U.readFile tmp handle _ => ""
      val _ = OS.FileSys.remove tmp handle _ => ()
      val ls = U.lines s
  in case ls of l::_ => U.trim l | [] => "unknown" end
fun check () =
  let val lines = ref ([]:string list)
      fun ok s = lines := ("[✓] " ^ s) :: !lines
      fun warn s = lines := ("[!] " ^ s) :: !lines
      fun bad s = lines := ("[✗] " ^ s) :: !lines
      val _ = if has "sml" then ok ("SML/NJ: " ^ versionSml ()) else bad "SML/NJ: not found"
      val _ = case OS.Process.getEnv "TWELF_SERVER" of
                SOME p => if U.exists p then ok ("Twelf: " ^ p) else bad ("Twelf: TWELF_SERVER missing " ^ p)
              | NONE => if has "twelf-server" then ok "Twelf: twelf-server in PATH"
                        else if U.exists "/usr/local/bin/twelf-server" then ok "Twelf: /usr/local/bin/twelf-server"
                        else bad "Twelf: not found"
      val _ = if U.exists "registry/index.json" then ok "registry/index.json: accessible"
              else warn "registry/index.json: missing"
      val _ = if U.exists "vendor" then
                let val n = length (U.listDir "vendor")
                in if n > 0 then warn ("vendor/ contains " ^ Int.toString n ^ " entries") else ok "vendor/: empty"
                end
              else ok "vendor/: absent"
      val _ = if U.exists "twelf-pm.lock" andalso U.exists "twelf.toml" then
                if U.newer "twelf.toml" "twelf-pm.lock" then warn "twelf-pm.lock exists but twelf.toml is newer"
                else ok "lockfile: in sync with toml mtime"
              else if U.exists "twelf-pm.lock" then ok "lockfile: present"
              else warn "lockfile: missing"
  in String.concatWith "\n" (List.rev (!lines)) ^ "\n" end
end
