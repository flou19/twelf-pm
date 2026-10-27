# twelf-pm

**v0.2.0** — content-addressed lockfile, formal resolve fragment, offline-first.

Offline package manager and project toolchain for [Twelf](https://twelf.org).
Requires only **SML/NJ** and **Twelf**. No network needed at runtime.

## Requirements

- SML/NJ (110.x)
- Twelf (`twelf-server` on `PATH`, or `TWELF_SERVER`)

No network access is required after install.

## Install

```bash
export PATH="/path/to/smlnj/bin:$PATH"
export TWELF_SERVER="$(command -v twelf-server)"
bash bin/twelf-pm doctor
```

## Quick start

```bash
twelf-pm init mytheory
cd mytheory
twelf-pm build
twelf-pm check
```

Import an existing `sources.cfg`:

```bash
twelf-pm import
twelf-pm build --force
```

## Commands

| Command | Role |
|---------|------|
| `init` / `new <tmpl>` | create project (`basic`, `logic`, `pl-semantics`, `paper`) |
| `build` | generate `sources.cfg`, run Twelf, write lock + `t/.ok` |
| `check` | fmt + namespace + lock + build |
| `import` | load modules from existing `sources.cfg` into `twelf.toml` |
| `add` / `vendor` / `publish` | dependencies (local paths and registry) |
| `ns` | namespace conflict report (`--strict`, `--no-rewrite`) |
| `tree` / `graph` | dependency tree / Graphviz `deps.dot` |
| `verify` / `update` | lockfile integrity / refresh |
| `fmt` / `doc` / `latex` | format and documentation |
| `debug` / `doctor` / `test` | diagnostics |
| `lsp` | stdio protocol: COMPLETE, DEF, HOVER, SYMBOLS, DIAG |
| `completion` | bash / zsh / fish |
| `hook` / `badge` | git pre-commit, SVG badges |

Flags: `--force` `--locked` `--strict` `--no-rewrite` `--json` `--timings` `--verbose` `--quiet` `--no-color`

Exit codes: `0` ok · `1` build · `2` config · `3` namespace · `4` lock · `5` registry

## Manifest

```toml
[package]
name = "project"
version = "0.1.0"
elf-dir = "t"
modules = ["n.elf", "v.elf"]

[dependencies]
list = ["natlib@^0.1", "./vendor/localpkg"]
```

## Example: neuroasm (30 Twelf modules)

Config used (`twelf.toml`):

```toml
[package]
name = "neuroasm"
version = "0.1.0"
elf-dir = "t"
modules = [
  "n.elf", "v.elf", "c.elf", "m.elf", "f.elf", "w.elf", "d.elf", "a.elf",
  "p.elf", "g.elf", "r.elf", "i.elf", "n2.elf", "c2.elf", "ch.elf", "nn.elf",
  "fs2.elf", "w2.elf", "th.elf", "ev.elf", "sec.elf", "hw.elf", "pr.elf",
  "sm.elf", "lf.elf", "mz.elf", "op.elf", "tr.elf", "eq.elf", "q.elf"
]

[dependencies]
list = []
```

Build:

```bash
cd neuroasm
twelf-pm import          # if only sources.cfg existed
twelf-pm build --force --timings
```

Observed output:

```
cfg 30 modules
Twelf 1.7.1+ (rlocal, built local on local)
%% OK %%
%% OK %%
[Loading file sources.cfg ...]
[Loading file n.elf ...]
[Loading file v.elf ...]
[Loading file c.elf ...]
[Loading file m.elf ...]
[Loading file f.elf ...]
[Loading file w.elf ...]
[Loading file d.elf ...]
[Loading file a.elf ...]
[Loading file p.elf ...]
[Loading file g.elf ...]
[Loading file r.elf ...]
[Loading file i.elf ...]
[Loading file n2.elf ...]
[Loading file c2.elf ...]
[Loading file ch.elf ...]
[Loading file nn.elf ...]
[Loading file fs2.elf ...]
[Loading file w2.elf ...]
[Loading file th.elf ...]
[Loading file ev.elf ...]
[Loading file sec.elf ...]
[Loading file hw.elf ...]
[Loading file pr.elf ...]
[Loading file sm.elf ...]
[Loading file lf.elf ...]
[Loading file mz.elf ...]
[Loading file op.elf ...]
[Loading file tr.elf ...]
[Loading file eq.elf ...]
[Loading file q.elf ...]
%% OK %%
ok
parse/load: 0ms
resolve/ns: 1719ms
lock verify: 0ms
twelf check: 1811ms
total: 3531ms
```

`twelf-pm check --json` → `{"ok":true,"errors":[]}`

## Layout

```
bin/twelf-pm     CLI launcher
s/               SML sources
packages/natlib  sample package
registry/        local package index
man/twelf-pm.1   manual page
```

## License

BSD 2-Clause. See `LICENSE`.
