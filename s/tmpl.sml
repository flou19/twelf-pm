structure Tmpl = struct
fun write path body = (U.mkdir (OS.Path.dir path handle _ => "."); U.writeFile path body)
fun basic name =
  let val _ = U.mkdir "t"
      val _ = Man.save "twelf.toml"
        {name=name, version="0.1.0", modules=["main.elf"], deps=[], elfDir="t"}
      val _ = U.writeFile "t/main.elf"
        "nat : type.\nz : nat.\ns : nat -> nat.\n"
      val _ = Cfg.write "t/sources.cfg" ["main.elf"]
  in U.outln ("new basic " ^ name) end
fun logic name =
  let val _ = U.mkdir "t"
      val _ = Man.save "twelf.toml"
        {name=name, version="0.1.0", modules=["prop.elf","proof.elf"], deps=[], elfDir="t"}
      val _ = U.writeFile "t/prop.elf"
        "prop : type.\ntrue : prop.\nand : prop -> prop -> prop.\nimp : prop -> prop -> prop.\n"
      val _ = U.writeFile "t/proof.elf"
        "pf : prop -> type.\ntrue/i : pf true.\nand/i : pf A -> pf B -> pf (and A B).\nimp/i : (pf A -> pf B) -> pf (imp A B).\n"
      val _ = Cfg.write "t/sources.cfg" ["prop.elf", "proof.elf"]
  in U.outln ("new logic " ^ name) end
fun pl name =
  let val _ = U.mkdir "t"
      val _ = Man.save "twelf.toml"
        {name=name, version="0.1.0", modules=["syn.elf","sem.elf"], deps=[], elfDir="t"}
      val _ = U.writeFile "t/syn.elf"
        "exp : type.\nnum : nat -> exp.\nadd : exp -> exp -> exp.\nnat : type.\nz : nat.\ns : nat -> nat.\n"
      val _ = U.writeFile "t/sem.elf"
        "val : type.\nv/num : nat -> val.\nstep : exp -> exp -> type.\nstep/add : step (add (num N) (num M)) (num N).\n"
      val _ = Cfg.write "t/sources.cfg" ["syn.elf", "sem.elf"]
  in U.outln ("new pl-semantics " ^ name) end
fun paper name =
  let val _ = basic name
      val _ = U.mkdir "paper"
      val _ = U.writeFile "paper/main.tex"
        "\\documentclass{article}\n\\begin{document}\n\\input{../docs/decls.tex}\n\\end{document}\n"
  in U.outln ("new paper " ^ name) end
fun create template name =
  case template of
    "basic" => basic name
  | "logic" => logic name
  | "pl-semantics" => pl name
  | "paper" => paper name
  | _ => U.die ("unknown template " ^ template)
end
