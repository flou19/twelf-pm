structure Comp = struct
val cmds = ["init","new","add","vendor","publish","ns","build","check","tree","verify",
            "update","fmt","debug","doc","latex","lsp","import","deps","order","hook",
            "badge","plugin","doctor","outdated","yank","info","search","graph","completion","test","lint","ci","update"]
fun bash () =
  "_twelf_pm() {\n" ^
  "  local cur=${COMP_WORDS[COMP_CWORD]}\n" ^
  "  COMPREPLY=( $(compgen -W \"" ^ U.join " " cmds ^ "\" -- $cur) )\n" ^
  "}\ncomplete -F _twelf_pm twelf-pm\n"
fun zsh () =
  "#compdef twelf-pm\n_arguments '1:command:(" ^ U.join " " cmds ^ ")'\n"
fun fish () =
  String.concat (map (fn c => "complete -c twelf-pm -a '" ^ c ^ "'\n") cmds)
fun gen shell =
  case shell of
    "bash" => bash ()
  | "zsh" => zsh ()
  | "fish" => fish ()
  | _ => bash ()
end
