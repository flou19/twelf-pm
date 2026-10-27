structure Cli = struct
fun usage () = U.outln
  "twelf-pm check|build|lint|ci|resolve|tree|verify|doctor|test|update --self|...\n  flags: --json --locked --frozen --force --strict --no-rewrite --timings --verbose --quiet --no-color"
fun has (f:string) xs = List.exists (fn (x:string) => x = f) xs
fun baseOpts () = Opt.applyFlags (Opt.fromToml "twelf.toml") []
fun applyLog flags =
  (if has "--verbose" flags then Log.setV true else ();
   if has "--quiet" flags then Log.setQ true else ())
fun loadMan () =
  if U.exists "twelf.toml" then Man.load "twelf.toml"
  else Ex.die Ex.config "missing twelf.toml"
fun init name =
  let val name = if name = "" then "project" else name
      val _ = U.mkdir "t"
      val m = {name=name, version="0.1.0", modules=["main.elf"], deps=[], devDeps=[], elfDir="t"}
      val _ = Man.save "twelf.toml" m
      val _ = if U.exists "t/main.elf" then ()
              else U.writeFile "t/main.elf" "nat : type.\nz : nat.\ns : nat -> nat.\n"
      val _ = Cfg.write "t/sources.cfg" ["main.elf"]
      val _ = U.mkdir "registry"
      val _ = if U.exists "registry/index.json" then ()
              else U.writeFile "registry/index.json" "# name|version|path|desc\n"
  in U.outln ("init " ^ name) end
fun vendor name path =
  let val dest = Res.vendorize name path in U.outln ("vendor " ^ name ^ " -> " ^ dest) end
fun publish name ver path =
  let val idx = Reg.publish name ver path "" in U.outln ("publish " ^ name ^ "@" ^ ver ^ " -> " ^ idx) end
fun add pkg =
  let val m = if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "project"
      val d = Man.parseDep pkg
      val deps = d :: List.filter (fn x => #name x <> #name d) (#deps m)
      val m = {name= #name m, version= #version m, modules= #modules m, deps=deps, devDeps= #devDeps m, elfDir= #elfDir m}
      val _ = Man.save "twelf.toml" m
      val p = Res.installDep d
      val opts = baseOpts ()
      val cs = Ns.conflicts (#elfDir m) [p]
      val _ = U.out (Ns.report cs)
      val _ = if #strict opts andalso not (null cs) then Ex.die Ex.ns "ns: strict conflict" else ()
      val _ = Lock.save (Lock.fromProject m [p])
  in U.outln ("add " ^ #name d ^ " @ " ^ p) end
fun nsCheck flags =
  let val opts = Opt.applyFlags (baseOpts ()) flags
      val m = Man.ensureModules (if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p")
      val paths = map Res.installDep (#deps m)
      val cs = Ns.conflicts (#elfDir m) paths
      val _ = U.out (Ns.report cs)
  in if #strict opts andalso not (null cs) then Ex.die Ex.ns "ns: strict conflict" else () end
fun doBuild flags =
  let val _ = applyLog flags
      val opts = Opt.applyFlags (baseOpts ()) flags
      val locked = has "--locked" flags orelse has "--frozen" flags
      val frozen = has "--frozen" flags orelse locked
      val timings = has "--timings" flags
      val json = has "--json" flags
      val t0 = Tm.now ()
      val m = if U.exists "twelf.toml" then Man.load "twelf.toml" else
                {name="project", version="0.1.0", modules=Man.discoverModules "t", deps=[], devDeps=[], elfDir="t"}
      val m = Man.ensureModules m
      val t1 = Tm.now ()
      val paths = Res.installAll m opts
      val t2 = Tm.now ()
      val _ = if locked then
                case Lock.verify m paths frozen of
                  [] => ()
                | es => Ex.die Ex.lock ("locked: " ^ U.join "; " es)
              else ()
      val t3 = Tm.now ()
      val ok = Build.build m (#force opts)
      val t4 = Tm.now ()
      val _ = if timings then (
                U.outln (Tm.show "parse/load" (Tm.ms t0 t1));
                U.outln (Tm.show "resolve/ns" (Tm.ms t1 t2));
                U.outln (Tm.show "lock verify" (Tm.ms t2 t3));
                U.outln (Tm.show "twelf check" (Tm.ms t3 t4));
                U.outln (Tm.show "total" (Tm.ms t0 t4)))
              else ()
      val n = length (#modules m)
  in if json then U.outln ("{\"ok\":" ^ Bool.toString ok ^ ",\"modules\":" ^ Int.toString n ^ "}") else ();
     if ok then OS.Process.success else (Ex.quit Ex.build; OS.Process.failure)
  end
fun check flags =
  let val _ = applyLog flags
      val json = has "--json" flags
      val errs = ref ([]:string list)
      fun push s = errs := s :: !errs
      val _ = (Fmt.formatDir "t"; ()) handle _ => push "fmt"
      val m = Man.ensureModules (if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p")
      val paths = map Res.installDep (#deps m)
      val cs = Ns.conflicts (#elfDir m) paths
      val _ = if not (null cs) then push "ns conflict" else ()
      val lockE = if U.exists "twelf.lock" orelse U.exists "twelf-pm.lock" then Lock.verify m paths true else []
      val _ = if not (null lockE) then push ("lock: " ^ hd lockE) else ()
      val okBuild = (Build.build m true handle _ => false)
      val _ = if not okBuild then push "build" else ()
      val ok = null (!errs) andalso okBuild
  in if json then
       U.outln ("{\"ok\":" ^ Bool.toString ok ^ ",\"errors\":[" ^
         U.join "," (map (fn e => "\"" ^ e ^ "\"") (List.rev (!errs))) ^ "]}")
     else if ok then U.outln (Color.green "check ok")
     else U.outln (Color.red ("check fail: " ^ U.join ", " (List.rev (!errs))));
     if ok then OS.Process.success
     else if not (null cs) then (Ex.quit Ex.ns; OS.Process.failure)
     else if not (null lockE) then (Ex.quit Ex.lock; OS.Process.failure)
     else (Ex.quit Ex.build; OS.Process.failure)
  end
fun verify flags =
  let val m = loadMan ()
      val paths = map Res.installDep (#deps m)
      val es = Lock.verify m paths true
      val json = has "--json" flags
  in if json then
       U.outln ("{\"ok\":" ^ Bool.toString (null es) ^ ",\"errors\":[" ^
         U.join "," (map (fn e => "\"" ^ e ^ "\"") es) ^ "]}")
     else if null es then U.outln (Color.green "verify ok")
     else (app (fn e => U.outln (Color.red e)) es; Ex.die Ex.lock "verify fail");
     OS.Process.success
  end
fun tree flags =
  let val m = if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p"
  in if has "--json" flags then U.out (Tree.toJson m) else U.out (Tree.render m) end
fun doctor () = U.out (Doctor.check ())
fun graph () =
  let val m = if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p"
      val _ = U.writeFile "deps.dot" (Graph.dot m)
  in U.outln "deps.dot" end
fun main args =
  case args of
    ["init"] => init "project"
  | ["init", n] => init n
  | ["new", tmpl] => Tmpl.create tmpl "project"
  | ["new", tmpl, n] => Tmpl.create tmpl n
  | ["add", p] => add p
  | ["vendor", n, p] => vendor n p
  | ["publish", n, v, p] => publish n v p
  | "ns"::flags => nsCheck flags
  | "build"::flags => OS.Process.exit (doBuild flags)
  | "check"::flags => OS.Process.exit (check flags)
  | "tree"::flags => tree flags
  | "verify"::flags => OS.Process.exit (verify flags)
  | ["update"] =>
      let val m = Man.ensureModules (loadMan ())
          val paths = map Res.installDep (#deps m)
      in Lock.save (Lock.fromProject m paths); U.outln "update lock" end
  | ["fmt"] => (Fmt.formatDir "t"; U.outln "fmt ok")
  | "debug"::flags =>
      let val opts = Opt.applyFlags (baseOpts ()) flags
          val arg = case List.rev flags of a::_ => if U.startsWith "--" a then "" else a | [] => ""
          val text = if arg = "" then let val (_, out) = Build.runTwelf "t" in out end
                     else if U.exists arg then U.readFile arg else arg
      in U.out (Dbg.analyzeText text (#json opts orelse has "--json" flags)) end
  | "doc"::flags =>
      let val fmt = if has "--latex" flags then "latex" else if has "--html" flags then "html" else "both"
          val out = case List.find (fn x => U.startsWith "--out=" x) flags of SOME s => String.extract (s, 6, NONE) | NONE => "docs"
          val m = if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p"
      in Doc.run fmt out (#elfDir (Man.ensureModules m)) end
  | ["latex"] => Doc.run "latex" "paper" "t"
  | ["lsp"] => Lsp.serve ()
  | ["import"] =>
      let val mods = Cfg.fromSources "t/sources.cfg"
          val m = {name="project", version="0.1.0", modules=mods, deps=[], devDeps=[], elfDir="t"}
      in Man.save "twelf.toml" m; U.outln ("import " ^ Int.toString (length mods)) end
  | ["deps"] => app (fn d => U.outln (#name d ^ "@" ^ #version d)) (#deps (loadMan ()))
  | ["order"] => app U.outln (Res.order (Man.ensureModules (loadMan ())))
  | ["hook"] => Hook.install ()
  | ["badge"] => Badge.generate ()
  | ["doctor"] => doctor ()
  | ["outdated"] => Pkg.outdated ()
  | ["info", n] => Pkg.info n
  | ["search", q] => Pkg.search q
  | ["yank", spec] =>
      (case U.split #"@" spec of [n,v] => Pkg.yank n v | _ => Ex.die Ex.config "yank name@version")
  | "graph"::_ => graph ()
  | ["completion", sh] => U.out (Comp.gen sh)
  | ["completion"] => U.out (Comp.gen "bash")
  | ["resolve"] =>
      let val m = loadMan ()
          val (ch, cs) = Solve.resolve m
          val _ = app (fn c => U.outln (#name c ^ "@" ^ #version c ^ " -> " ^ #path c)) ch
          val _ = if null cs then () else U.out (Solve.explain cs)
      in if null cs then () else Ex.die Ex.registry "resolve failed" end
  | ["lint"] => U.out (Lint.run "t" false)
  | "lint"::flags => U.out (Lint.run "t" (List.exists (fn x => x = "--json") flags))
  | ["ci", k] => Ci.gen k
  | ["ci"] => Ci.gen "github"
  | ["update", "--self"] => SelfUp.update ()
  | ["test"] => Self.run ()
  | "test"::flags =>
      let val m = Man.withDev (if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p")
          val _ = Res.installAll m (baseOpts ())
      in Self.run () end
  | ["workspace", "build"] => (if Ws.buildAll false then () else Ex.die Ex.build "workspace build fail")
  | ["workspace", "build", "--force"] => (if Ws.buildAll true then () else Ex.die Ex.build "workspace build fail")
  | ["workspace", "check"] => (if Ws.checkAll () then U.outln "workspace check ok" else Ex.die Ex.build "workspace check fail")
  | ["workspace", "list"] => app U.outln (Ws.members ())
  | ["plugin", "list"] => app U.outln (Plug.list ())
  | "plugin"::name::rest => Plug.run name rest
  | _ => (usage (); Ex.quit Ex.config)
end
