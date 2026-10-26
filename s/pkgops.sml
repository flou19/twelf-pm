structure Pkg = struct
fun yank name ver =
  let val idx = "registry/index.json"
      val es = Reg.loadIndex idx
      val es' = map (fn (e:Reg.entry) =>
        if #name e = name andalso #version e = ver
        then {name= #name e, version= #version e, path= #path e, desc= "YANKED " ^ #desc e}
        else e) es
      val _ = Reg.writeLocalIndex es' idx
  in U.outln ("yank " ^ name ^ "@" ^ ver) end
fun info name =
  case Reg.find name "*" of
    NONE => Ex.die Ex.registry ("not found: " ^ name)
  | SOME (e:Reg.entry) =>
      U.outln ("name: " ^ #name e ^ "\nversion: " ^ #version e ^
               "\npath: " ^ #path e ^ "\ndesc: " ^ #desc e)
fun search q =
  let val es = Reg.allEntries ()
      val hits = List.filter (fn (e:Reg.entry) =>
        String.isSubstring q (#name e) orelse String.isSubstring q (#desc e)) es
  in app (fn (e:Reg.entry) => U.outln (#name e ^ "@" ^ #version e ^ " — " ^ #desc e)) hits end
fun outdated () =
  let val m = if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p"
      fun one (d:Man.dep) =
        case Reg.find (#name d) "*" of
          NONE => NONE
        | SOME (e:Reg.entry) =>
            if Sem.cmp (Sem.parse (#version e), Sem.parse (#version d)) = GREATER
            then SOME (#name d ^ "  " ^ #version d ^ " → " ^ #version e)
            else NONE
      val lines = List.mapPartial one (#deps m)
  in if null lines then U.outln "up to date" else app U.outln lines end
end
