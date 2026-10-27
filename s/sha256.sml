structure Sha = struct
local
  structure W = Word32
  fun rotr (x, n) = W.orb (W.>> (x, n), W.<< (x, Word.- (0w32, n)))
  fun ch (x,y,z) = W.xorb (W.andb (x,y), W.andb (W.notb x, z))
  fun maj (x,y,z) = W.xorb (W.xorb (W.andb (x,y), W.andb (x,z)), W.andb (y,z))
  fun bsig0 x = W.xorb (W.xorb (rotr (x,0w2), rotr (x,0w13)), rotr (x,0w22))
  fun bsig1 x = W.xorb (W.xorb (rotr (x,0w6), rotr (x,0w11)), rotr (x,0w25))
  fun ssig0 x = W.xorb (W.xorb (rotr (x,0w7), rotr (x,0w18)), W.>> (x,0w3))
  fun ssig1 x = W.xorb (W.xorb (rotr (x,0w17), rotr (x,0w19)), W.>> (x,0w10))
  val K = Vector.fromList [
    0wx428a2f98,0wx71374491,0wxb5c0fbcf,0wxe9b5dba5,0wx3956c25b,0wx59f111f1,0wx923f82a4,0wxab1c5ed5,
    0wxd807aa98,0wx12835b01,0wx243185be,0wx550c7dc3,0wx72be5d74,0wx80deb1fe,0wx9bdc06a7,0wxc19bf174,
    0wxe49b69c1,0wxefbe4786,0wx0fc19dc6,0wx240ca1cc,0wx2de92c6f,0wx4a7484aa,0wx5cb0a9dc,0wx76f988da,
    0wx983e5152,0wxa831c66d,0wxb00327c8,0wxbf597fc7,0wxc6e00bf3,0wxd5a79147,0wx06ca6351,0wx14292967,
    0wx27b70a85,0wx2e1b2138,0wx4d2c6dfc,0wx53380d13,0wx650a7354,0wx766a0abb,0wx81c2c92e,0wx92722c85,
    0wxa2bfe8a1,0wxa81a664b,0wxc24b8b70,0wxc76c51a3,0wxd192e819,0wxd6990624,0wxf40e3585,0wx106aa070,
    0wx19a4c116,0wx1e376c08,0wx2748774c,0wx34b0bcb5,0wx391c0cb3,0wx4ed8aa4a,0wx5b9cca4f,0wx682e6ff3,
    0wx748f82ee,0wx78a5636f,0wx84c87814,0wx8cc70208,0wx90befffa,0wxa4506ceb,0wxbef9a3f7,0wxc67178f2] : W.word vector
  fun from4 a b c d =
    W.orb (W.orb (W.<< (W.fromInt a, 0w24), W.<< (W.fromInt b, 0w16)),
           W.orb (W.<< (W.fromInt c, 0w8), W.fromInt d))
  fun strBytes s = List.tabulate (size s, fn i => Char.ord (String.sub (s, i)))
  fun pad bytes =
    let val n = length bytes
        val bitLen = IntInf.fromInt (n * 8)
        val padLen = (55 - n mod 64) mod 64
        val zeros = List.tabulate (padLen, fn _ => 0)
        fun lenBytes () =
          List.tabulate (8, fn i =>
            let val shift = 8 * (7 - i)
            in IntInf.toInt (IntInf.andb (IntInf.~>> (bitLen, Word.fromInt shift), 255)) end)
        val body = bytes @ [0x80] @ zeros @ lenBytes ()
    in Array.fromList body end
  fun h8 w =
    let val s = String.map Char.toLower (W.fmt StringCvt.HEX w)
        val n = 8 - size s
    in if n <= 0 then s else String.implode (List.tabulate (n, fn _ => #"0")) ^ s end
in
fun hashString s =
  let val data = pad (strBytes s)
      val len = Array.length data
      val blocks = len div 64
      val h0 = ref 0wx6a09e667 val h1 = ref 0wxbb67ae85
      val h2 = ref 0wx3c6ef372 val h3 = ref 0wxa54ff53a
      val h4 = ref 0wx510e527f val h5 = ref 0wx9b05688c
      val h6 = ref 0wx1f83d9ab val h7 = ref 0wx5be0cd19
      fun block bi =
        if bi = blocks then ()
        else
          let val off = bi * 64
              val w = Array.array (64, 0w0 : W.word)
              fun fill k =
                if k = 64 then ()
                else if k < 16 then
                  (Array.update (w, k, from4 (Array.sub (data, off+4*k))
                                              (Array.sub (data, off+4*k+1))
                                              (Array.sub (data, off+4*k+2))
                                              (Array.sub (data, off+4*k+3)));
                   fill (k+1))
                else
                  (Array.update (w, k,
                     ssig1 (Array.sub (w, k-2)) + Array.sub (w, k-7) +
                     ssig0 (Array.sub (w, k-15)) + Array.sub (w, k-16));
                   fill (k+1))
              val _ = fill 0
              val a = ref (!h0) val b = ref (!h1) val c = ref (!h2) val d = ref (!h3)
              val e = ref (!h4) val f = ref (!h5) val g = ref (!h6) val h = ref (!h7)
              fun rnd t =
                if t = 64 then ()
                else
                  let val t1 = !h + bsig1 (!e) + ch (!e,!f,!g) + Vector.sub (K,t) + Array.sub (w,t)
                      val t2 = bsig0 (!a) + maj (!a,!b,!c)
                  in h := !g; g := !f; f := !e; e := !d + t1;
                     d := !c; c := !b; b := !a; a := t1 + t2; rnd (t+1) end
              val _ = rnd 0
          in h0 := !h0 + !a; h1 := !h1 + !b; h2 := !h2 + !c; h3 := !h3 + !d;
             h4 := !h4 + !e; h5 := !h5 + !f; h6 := !h6 + !g; h7 := !h7 + !h;
             block (bi+1) end
      val _ = block 0
  in h8 (!h0)^h8 (!h1)^h8 (!h2)^h8 (!h3)^h8 (!h4)^h8 (!h5)^h8 (!h6)^h8 (!h7) end
fun hashFile path = if U.exists path then hashString (U.readFile path)
                    else String.implode (List.tabulate (64, fn _ => #"0"))
end
end
