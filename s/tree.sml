structure Tree = struct
fun render (m:Man.t) =
  let val root = #name m ^ " v" ^ #version m
      val deps = #deps m
      fun line i last (d:Man.dep) =
        (if last then "`-- " else "|-- ") ^ #name d ^ " v" ^ #version d
      fun go [] = []
        | go [d] = [line 0 true d]
        | go (d::ds) = line 0 false d :: go ds
  in root ^ "\n" ^ (if null deps then "(no deps)\n" else U.join "\n" (go deps) ^ "\n") end
fun toJson (m:Man.t) =
  let fun dep (d:Man.dep) = "{\"name\":\"" ^ #name d ^ "\",\"version\":\"" ^ #version d ^ "\"}"
  in "{\"name\":\"" ^ #name m ^ "\",\"version\":\"" ^ #version m ^
     "\",\"deps\":[" ^ U.join "," (map dep (#deps m)) ^ "]}\n"
  end
end
