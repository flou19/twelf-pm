structure Lock = struct
type pkg = {name:string, version:string, source:string, checksum:string}
type t = pkg list
val path = "twelf-pm.lock"
fun parse body =
  let val ls = U.lines body
      fun go [] acc cur = (case cur of NONE => List.rev acc | SOME p => List.rev (p::acc))
        | go (l::ls) acc cur =
            let val t = U.trim l
            in if t = "" orelse U.startsWith "#" t then go ls acc cur
               else if t = "[[package]]" then
                 go ls (case cur of NONE => acc | SOME p => p::acc) (SOME {name="", version="", source="", checksum=""})
               else case U.split #"=" t of
                 [k,v] =>
                   let val k = U.trim k val v = U.trim v
                       val v = if U.startsWith "\"" v andalso U.endsWith "\"" v
                               then String.substring (v, 1, size v - 2) else v
                   in case cur of
                        NONE => go ls acc cur
                      | SOME p =>
                          let val p =
                                if k = "name" then {name=v, version= #version p, source= #source p, checksum= #checksum p}
                                else if k = "version" then {name= #name p, version=v, source= #source p, checksum= #checksum p}
                                else if k = "source" then {name= #name p, version= #version p, source=v, checksum= #checksum p}
                                else if k = "checksum" then {name= #name p, version= #version p, source= #source p, checksum=v}
                                else p
                          in go ls acc (SOME p) end
                   end
               | _ => go ls acc cur
            end
  in go ls [] NONE end
fun load () = if U.exists path then parse (U.readFile path) else []
fun save (pkgs:t) =
  let val body = String.concat (map (fn (p:pkg) =>
        "[[package]]\nname = \"" ^ #name p ^ "\"\nversion = \"" ^ #version p ^
        "\"\nsource = \"" ^ #source p ^ "\"\nchecksum = \"" ^ #checksum p ^ "\"\n\n") pkgs)
  in U.writeFile path body end
fun fromDeps (m:Man.t) paths =
  ListPair.map (fn (d:Man.dep, p) =>
    {name= #name d, version= #version d, source=p, checksum=Hash.dir p}) (#deps m, paths)
fun verify (m:Man.t) paths =
  let val lock = load ()
      fun chk (d:Man.dep, p) =
        case List.find (fn (x:pkg) => #name x = #name d) lock of
          NONE => SOME (#name d ^ " missing from lock")
        | SOME x =>
            let val h = Hash.dir p
            in if h <> #checksum x then SOME (#name d ^ " checksum mismatch")
               else if #version d <> "*" andalso not (Sem.satisfies (#version d) (#version x))
                    andalso #version d <> #version x
               then SOME (#name d ^ " version mismatch")
               else NONE
            end
  in List.mapPartial chk (ListPair.zip (#deps m, paths)) end
end
