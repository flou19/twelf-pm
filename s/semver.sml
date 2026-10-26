structure Sem = struct
type ver = int * int * int
fun parse s =
  let val s = case String.tokens (fn c => c = #"-" orelse c = #"+") s of x::_ => x | [] => s
      val parts = String.tokens (fn c => c = #".") s
      fun n x = getOpt (Int.fromString x, 0)
  in case parts of
       [a,b,c] => (n a, n b, n c)
     | [a,b] => (n a, n b, 0)
     | [a] => (n a, 0, 0)
     | _ => (0, 0, 0)
  end
fun cmp ((a,b,c), (d,e,f)) =
  if a <> d then Int.compare (a,d)
  else if b <> e then Int.compare (b,e)
  else Int.compare (c,f)
fun ge a b = cmp (a,b) <> LESS
fun lt a b = cmp (a,b) = LESS
fun valid s =
  let val cs = String.explode s
  in List.all (fn c => Char.isDigit c orelse c = #"." orelse c = #"-" orelse c = #"+") cs
     andalso Char.isDigit (hd cs handle _ => #"x")
  end
datatype range = Exact of ver | Caret of ver | Gte of ver | Lt of ver | And of range * range | Any
fun parseRange s =
  let val s = U.trim s
  in if s = "*" orelse s = "" then Any
     else if U.startsWith "^" s then Caret (parse (String.extract (s, 1, NONE)))
     else if U.startsWith ">=" s then Gte (parse (String.extract (s, 2, NONE)))
     else if U.startsWith ">" s then Gte (parse (String.extract (s, 1, NONE)))
     else if U.startsWith "<" s then Lt (parse (String.extract (s, 1, NONE)))
     else Exact (parse s)
  end
fun matchRange range ver =
  case range of
    Any => true
  | Exact v => cmp (ver, v) = EQUAL
  | Caret (maj,min,pat) =>
      ge ver (maj,min,pat) andalso
      (if maj > 0 then lt ver (maj+1, 0, 0)
       else if min > 0 then lt ver (0, min+1, 0)
       else lt ver (0, 0, pat+1))
  | Gte v => ge ver v
  | Lt v => lt ver v
  | And (a,b) => matchRange a ver andalso matchRange b ver
fun satisfies versionSpec candidate =
  matchRange (parseRange versionSpec) (parse candidate)
end
