structure Toml = struct
datatype v = Str of string | Int of int | Bool of bool | Arr of v list | Tbl of (string * v) list
type t = (string * v) list
fun parseLine line =
  let val line = U.trim line
  in if line = "" orelse U.startsWith "#" line then NONE
     else if U.startsWith "[" line andalso U.endsWith "]" line andalso not (String.isSubstring "=" line) then
       SOME ("__section", Str (String.substring (line, 1, size line - 2)))
     else case U.split #"=" line of
            k::rest =>
              let val key = U.trim k
                  val raw = U.trim (U.join "=" rest)
                  val v = if U.startsWith "\"" raw andalso U.endsWith "\"" raw
                          then Str (String.substring (raw, 1, size raw - 2))
                          else if raw = "true" then Bool true
                          else if raw = "false" then Bool false
                          else if U.startsWith "[" raw andalso U.endsWith "]" raw then
                            let val inner = String.substring (raw, 1, size raw - 2)
                                val parts = map U.trim (U.split #"," inner)
                                val parts = List.filter (fn x => x <> "") parts
                            in Arr (map (fn s => if U.startsWith "\"" s andalso U.endsWith "\"" s
                                                 then Str (String.substring (s, 1, size s - 2))
                                                 else Str s) parts)
                            end
                          else case Int.fromString raw of SOME n => Int n | NONE => Str raw
              in SOME (key, v) end
          | _ => NONE
  end
fun parse s =
  let val ls = U.lines s
      fun go [] sec acc = List.rev acc
        | go (l::ls) sec acc =
            case parseLine l of
              NONE => go ls sec acc
            | SOME ("__section", Str name) => go ls name acc
            | SOME (k, v) =>
                let val key = if sec = "" then k else sec ^ "." ^ k
                in go ls sec ((key, v)::acc) end
  in go ls "" [] end
fun get (t:t) (k:string) =
  case List.find (fn (a,_) => a = k) t of SOME (_,v) => SOME v | NONE => NONE
fun getStr t k = case get t k of SOME (Str s) => SOME s | _ => NONE
fun getArr t k = case get t k of SOME (Arr a) => SOME a | _ => NONE
fun getStrList t k =
  case getArr t k of
    SOME a => List.mapPartial (fn Str s => SOME s | _ => NONE) a
  | NONE => []
end
