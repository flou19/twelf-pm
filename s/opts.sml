structure Opt = struct
type t = {strict:bool, rewrite:bool, json:bool, force:bool}
val default = {strict=false, rewrite=true, json=false, force=false}
fun fromToml path =
  if not (U.exists path) then default
  else
    let val t = Toml.parse (U.readFile path)
        fun b k d = case Toml.getStr t k of
                      SOME "true" => true | SOME "false" => false
                    | SOME "1" => true | SOME "0" => false
                    | _ => (case Toml.get t k of SOME (Toml.Bool x) => x | _ => d)
    in {strict = b "package.strict" false orelse b "ns.strict" false,
        rewrite = b "ns.rewrite" true,
        json = false,
        force = false}
    end
fun applyFlags (opts:t) flags =
  let fun go [] acc = acc
        | go ("--strict"::t) acc = go t {strict=true, rewrite= #rewrite acc, json= #json acc, force= #force acc}
        | go ("--no-rewrite"::t) acc = go t {strict= #strict acc, rewrite=false, json= #json acc, force= #force acc}
        | go ("--allow-conflict"::t) acc = go t {strict=false, rewrite=false, json= #json acc, force= #force acc}
        | go ("--json"::t) acc = go t {strict= #strict acc, rewrite= #rewrite acc, json=true, force= #force acc}
        | go ("--force"::t) acc = go t {strict= #strict acc, rewrite= #rewrite acc, json= #json acc, force=true}
        | go (_::t) acc = go t acc
  in go flags opts end
end
