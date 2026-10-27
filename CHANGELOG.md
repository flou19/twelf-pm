# Changelog

## 0.2.0

- SHA-256 content-addressed `twelf.lock` (per-module + `cfg-sha256`)
- `--locked` / `--frozen` fail on manifest/lock divergence
- Transitive dependency resolution with conflict explanations (`resolve`)
- Formal LF fragment of version satisfaction / resolve (`formal/`)
- Resolve cache keyed by manifest hash
- EBNF of `twelf.toml` (`docs/twelf.toml.ebnf`)
- Fixtures, golden sources.cfg, benches table
- LSP diagnostics with file:line; hover/def with line numbers; `RENAME`
- Deterministic `sources.cfg` emission (explicit module order)
- `lint`: missing `%total`/`%mode`/`%worlds`, mode arity vs polarities, orphan types/clauses, empty directives, tabs/trailing space
- Trap fixtures under `tests/fixtures/trap-*` (Twelf-OK / lint-fail cases)
- `ci github|gitlab|script` config generator
- `[dev-dependencies]` in `twelf.toml`; pulled for `test`
- `update --self` via git pull (`TWELF_PM_ROOT`)

## 0.1.0

- Initial offline toolchain release
