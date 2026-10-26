structure Graph = struct
fun dot (m:Man.t) =
  let val nodes = #name m :: map (fn (d:Man.dep) => #name d) (#deps m)
      val edges = map (fn (d:Man.dep) => "  \"" ^ #name m ^ "\" -> \"" ^ #name d ^ "\";") (#deps m)
  in "digraph deps {\n" ^ U.join "\n" edges ^ "\n}\n" end
fun write format m =
  case format of
    "dot" => (U.out (dot m); OS.Path.joinDirFile {dir=".", file="deps.dot"})
  | _ => (U.writeFile "deps.dot" (dot m); "deps.dot")
end
