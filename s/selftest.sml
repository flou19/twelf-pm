structure Self = struct
fun ok true = () | ok false = raise Fail "selftest"
fun run () =
  let val _ = ok (Sem.satisfies "^1.0" "1.2.3")
      val _ = ok (not (Sem.satisfies "^1.0" "2.0.0"))
      val _ = ok (Sem.valid "0.1.0")
      val h1 = Sha.hashString "abc"
      val h2 = Sha.hashString "abc"
      val h3 = Sha.hashString "abd"
      val _ = ok (size h1 = 64)
      val _ = ok (h1 = h2)
      val _ = ok (h1 <> h3)
      val _ = ok (h1 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
      val _ = ok (Toml.parse "a = \"b\"\n" <> [])
  in U.outln "selftest ok" end
end
