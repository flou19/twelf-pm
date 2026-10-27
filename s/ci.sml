structure Ci = struct
fun github () =
  "name: twelf\n" ^
  "on: [push, pull_request]\n" ^
  "jobs:\n" ^
  "  check:\n" ^
  "    runs-on: ubuntu-latest\n" ^
  "    steps:\n" ^
  "      - uses: actions/checkout@v4\n" ^
  "      - name: setup note\n" ^
  "        run: echo 'Requires SML/NJ and twelf-server on PATH'\n" ^
  "      - name: lint\n" ^
  "        run: twelf-pm lint --json\n" ^
  "      - name: check\n" ^
  "        run: twelf-pm check --json\n" ^
  "      - name: build locked\n" ^
  "        run: twelf-pm build --locked --force\n"
fun gitlab () =
  "stages: [check]\n" ^
  "twelf:\n" ^
  "  stage: check\n" ^
  "  script:\n" ^
  "    - twelf-pm lint --json\n" ^
  "    - twelf-pm check --json\n" ^
  "    - twelf-pm build --locked --force\n"
fun script () =
  "#!/bin/sh\nset -e\ntwelf-pm lint\ntwelf-pm check\ntwelf-pm build --locked --force\n"
fun gen kind =
  case kind of
    "github" =>
      let val _ = U.mkdir ".github"
          val _ = U.mkdir ".github/workflows"
          val p = ".github/workflows/twelf.yml"
          val _ = U.writeFile p (github ())
      in U.outln ("ci " ^ p) end
  | "gitlab" => (U.writeFile ".gitlab-ci.yml" (gitlab ()); U.outln "ci .gitlab-ci.yml")
  | "script" => (U.writeFile "ci.sh" (script ());
                 OS.Process.system "chmod +x ci.sh"; U.outln "ci ci.sh")
  | _ => U.die ("ci: unknown target " ^ kind ^ " (github|gitlab|script)")
end
