structure Self = struct
fun ok true = () | ok false = raise Fail "selftest"
fun run () =
  let val _ = ok (Sem.satisfies "^1.0" "1.2.3")
      val _ = ok (not (Sem.satisfies "^1.0" "2.0.0"))
      val _ = ok (Sem.valid "0.1.0")
      val _ = ok (Hash.fnv1a "abc" <> Hash.fnv1a "abd")
      val _ = ok (Toml.parse "a = \"b\"\n" <> [])
  in U.outln "selftest ok" end
end
