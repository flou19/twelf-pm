structure Dbg = struct
datatype kind = Total | Worlds | Error | Other
type issue = {kind:kind, file:string, line:int, msg:string}
datatype tree = Node of string * bool * tree list
fun classify s =
  if String.isSubstring "%total" s orelse String.isSubstring "TOTALITY" s then Total
  else if String.isSubstring "%worlds" s orelse String.isSubstring "worlds" s then Worlds
  else if String.isSubstring "Error" s then Error
  else Other
fun parseLoc s =
  let val s = U.trim s
  in case String.tokens (fn c => c = #":") s of
       f::l::_ => (case Int.fromString l of SOME n => (f, n) | NONE => (f, 0))
     | [f] => (f, 0)
     | _ => ("", 0)
  end
fun parseOutput text =
  let val ls = U.lines text
      fun go [] acc = List.rev acc
        | go (l::ls) acc =
            if String.isSubstring "Error" l orelse String.isSubstring "Warning" l
               orelse String.isSubstring "%% ABORT" l
               orelse String.isSubstring "total" l then
              let val k = classify l
                  val (f, n) = parseLoc l
                  val iss : issue = {kind=k, file=f, line=n, msg=U.trim l}
              in go ls (iss::acc) end
            else go ls acc
  in go ls [] end
fun showKind Total = "TOTAL" | showKind Worlds = "WORLDS" | showKind Error = "ERROR" | showKind Other = "INFO"
fun mark s = "<< " ^ s ^ " >>"
fun buildTree (issues:issue list) =
  let fun leaf (i:issue) =
        let val bad = case #kind i of Error => true | Total => true | Worlds => true | Other => false
            val label = showKind (#kind i) ^ " " ^ #file i ^ ":" ^ Int.toString (#line i)
        in Node (label, bad, [Node (#msg i, bad, [])]) end
      val anyBad = List.exists (fn (i:issue) => case #kind i of Other => false | _ => true) issues
  in Node ("twelf", anyBad, map leaf issues) end
fun renderTree t =
  let fun go pref isLast (Node (label, bad, kids)) =
        let val branch = if pref = "" then "" else if isLast then "`-- " else "|-- "
            val tag = if bad then mark label else label
            val line = pref ^ branch ^ tag ^ "\n"
            val childPref = pref ^ (if pref = "" then "" else if isLast then "    " else "|   ")
            fun children [] = ""
              | children xs =
                  let val n = length xs
                      fun loop i [] = []
                        | loop i (x::rest) = go childPref (i = n - 1) x :: loop (i+1) rest
                  in String.concat (loop 0 xs) end
        in line ^ children kids end
  in go "" true t end
fun escape s =
  let fun e #"\"" = "\\\"" | e #"\\" = "\\\\" | e #"\n" = "\\n" | e c = str c
  in String.concat (map e (String.explode s)) end
fun toJson (issues:issue list) =
  let fun one (i:issue) =
        "{\"kind\":\"" ^ showKind (#kind i) ^
        "\",\"file\":\"" ^ escape (#file i) ^
        "\",\"line\":" ^ Int.toString (#line i) ^
        ",\"msg\":\"" ^ escape (#msg i) ^ "\"}"
  in "[" ^ U.join "," (map one issues) ^ "]\n" end
fun render (issues:issue list) =
  if null issues then "no issues\n"
  else
    let val flat = String.concat (map (fn (i:issue) =>
          "[" ^ showKind (#kind i) ^ "] " ^ #file i ^ ":" ^ Int.toString (#line i) ^ "\n  " ^ #msg i ^ "\n") issues)
    in flat ^ "\n--- tree ---\n" ^ renderTree (buildTree issues) end
fun analyze path json =
  let val text = if U.exists path then U.readFile path else path
      val issues = parseOutput text
  in if json then toJson issues else render issues end
fun analyzeText text json =
  let val issues = parseOutput text
  in if json then toJson issues else render issues end
end
