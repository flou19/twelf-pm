structure Color = struct
val enabled =
  (case OS.Process.getEnv "NO_COLOR" of SOME _ => false | NONE => true)
  andalso
  (case OS.Process.getEnv "TWELF_PM_COLOR" of SOME "0" => false | SOME "false" => false | _ => true)
fun wrap code s = if enabled then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
fun red s = wrap "31" s
fun green s = wrap "32" s
fun yellow s = wrap "33" s
fun dim s = wrap "2" s
end
