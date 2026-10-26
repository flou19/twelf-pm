structure Badge = struct
fun svg label value color =
  let val w = 20 + 7 * (size label + size value)
      val lw = 10 + 7 * size label
  in "<?xml version=\"1.0\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ^
     Int.toString w ^ "\" height=\"20\">\n" ^
     "<rect width=\"" ^ Int.toString lw ^ "\" height=\"20\" fill=\"#555\"/>\n" ^
     "<rect x=\"" ^ Int.toString lw ^ "\" width=\"" ^ Int.toString (w-lw) ^
     "\" height=\"20\" fill=\"" ^ color ^ "\"/>\n" ^
     "<text x=\"6\" y=\"14\" fill=\"#fff\" font-family=\"DejaVu Sans,sans-serif\" font-size=\"11\">" ^
     label ^ "</text>\n" ^
     "<text x=\"" ^ Int.toString (lw+6) ^ "\" y=\"14\" fill=\"#fff\" font-family=\"DejaVu Sans,sans-serif\" font-size=\"11\">" ^
     value ^ "</text>\n</svg>\n"
  end
fun generate () =
  let val m = if U.exists "twelf.toml" then Man.load "twelf.toml" else Man.empty "p"
      val deps = Int.toString (length (#deps m))
      val ver = #version m
      val st = if U.exists "t/.ok" then ("passing", "#4c1") else ("unknown", "#9f9f9f")
      val _ = U.mkdir "badges"
      val _ = U.writeFile "badges/build.svg" (svg "build" (#1 st) (#2 st))
      val _ = U.writeFile "badges/version.svg" (svg "twelf-pm" ver "#007ec6")
      val _ = U.writeFile "badges/deps.svg" (svg "deps" deps "#fe7d37")
  in U.outln "badge ok" end
end
