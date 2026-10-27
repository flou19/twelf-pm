structure Solve = struct
type choice = {name:string, version:string, path:string}
type conflict = {pkg:string, need:string, available:string list, reason:string}
fun depsOfPkg path =
  let val toml = OS.Path.joinDirFile {dir=path, file="twelf.toml"}
  in if U.exists toml then #deps (Man.load toml) else [] end
fun resolve (m:Man.t) =
  let val seen = ref ([]:string list)
      val chosen = ref ([]:choice list)
      val conflicts = ref ([]:conflict list)
      fun pick (d:Man.dep) =
        if List.exists (fn n => n = #name d) (!seen) then ()
        else
          (seen := #name d :: !seen;
           case Reg.resolvePkg (#name d) (#version d) of
             NONE => conflicts := {pkg= #name d, need= #version d, available=[],
                                   reason="no candidate in registry/vendor"} :: !conflicts
           | SOME path =>
               let val ver =
                     case Reg.find (#name d) (#version d) of
                       SOME e => #version e
                     | NONE => #version d
                   val ok = Sem.satisfies (#version d) ver orelse #version d = "*" orelse #version d = ver
               in if not ok then
                    conflicts := {pkg= #name d, need= #version d, available=[ver],
                                  reason="semver " ^ ver ^ " does not satisfy " ^ #version d} :: !conflicts
                  else (chosen := {name= #name d, version=ver, path=path} :: !chosen;
                        app pick (depsOfPkg path))
               end)
      val _ = app pick (#deps m)
  in (List.rev (!chosen), List.rev (!conflicts)) end
fun explain cs =
  String.concat (map (fn (c:conflict) =>
    "conflict: " ^ #pkg c ^ " needs " ^ #need c ^ " — " ^ #reason c ^
    (if null (#available c) then "" else " (have " ^ U.join "," (#available c) ^ ")") ^ "\n") cs)
end
