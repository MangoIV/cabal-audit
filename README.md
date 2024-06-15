<div align="center">
  <a href="https://github.com/mangoiv/cabal-audit/actions">
    <img src="https://github.com/mangoiv/cabal-audit/actions/workflows/cabal-audit.yml/badge.svg" alt="CI">
    <img src="https://github.com/MangoIV/cabal-audit/actions/workflows/haskell-ci.yml/badge.svg" ald="CI">
  </a>
  <h1> <code> cabal-audit </code> </h1>
</div>

`cabal-audit` is a command-line utility that scans Haskell projects for known vulnerabilities based on the 
[security advisories database](https://github.com/haskell/security-advisories). 
It checks project dependencies, reports potential vulnerabilities, and provides details about the vulnerabilities, including links to relevant 
advisories and possible fixes.

## Installation

To install `cabal-audit`, you can use Nix by running the following command:

```bash
nix run github:mangoiv/cabal-audit -- --help
```

If you don't use `nix`, you can also build from source with `cabal`. Just clone the repository and run `cabal install`.

You can also [download a static executable from one of the latest workflow runs](https://github.com/MangoIV/cabal-audit/releases/tag/nightly).

> [!Note]
> We also have a `cachix`. If you trust me (which I do not recommend, never trust anybody!), run `cachix use cabal-audit` to 
> download directly from the cachix and skip building.

## Usage

Run `cabal-audit` to scan your project for known vulnerabilities:

```console
λ cabal-audit --help
Welcome to cabal audit

Usage: cabal-audit [(-p|--file-path FILEPATH) | (-r|--repository REPOSITORY)] 
                   [--verbosity ARG] [-m|--json] [-o|--to-file FILEPATH] 
                   [-b|--no-color|--no-colour] [--fail-on-warning]

  audit your cabal projects for vulnerabilities

Available options:
  -h,--help                Show this help text
  -p,--file-path FILEPATH  the path to the repository containing an advisories
                           directory
  -r,--repository REPOSITORY
                           the url to the repository containing an advisories
                           directory
  -m,--json                whether to format as json mapping package names to
                           osvs that apply
  -o,--to-file FILEPATH    specify a file to write to, instead of stdout
  -b,--no-color,--no-colour
                           don't colour the output
  --fail-on-warning        Exits with an error code if any advisories are found
                           in the build plan
```

```console
λ cabal-audit
trying to clone https://github.com/haskell/security-advisories
Cloning into '/tmp/cabal-audit3119166'...
remote: Enumerating objects: 183, done.
remote: Counting objects: 100% (183/183), done.
remote: Compressing objects: 100% (140/140), done.
remote: Total 183 (delta 5), reused 123 (delta 0), pack-reused 0
Receiving objects: 100% (183/183), 131.50 KiB | 2.19 MiB/s, done.
Resolving deltas: 100% (5/5), done.


Found advisories:

dependency "base" at version 4.19.1.0 is vulnerable for:
  HSEC-2023-0007 "readFloat: memory exhaustion with large exponent"
  published: 2024-06-13 06:04:41 UTC
  https://haskell.github.io/security-advisories/advisory/HSEC-2023-0007
  No fix version available
  toml, parser, dos

dependency "process" at version 1.6.18.0 is vulnerable for:
  HSEC-2024-0003 "process: command injection via argument list on Windows"
  published: 2024-06-13 06:04:41 UTC
  https://haskell.github.io/security-advisories/advisory/HSEC-2024-0003
  Fix available since version 1.6.19.0
  windows
```

> [!Note]
> If you encounter an error related to lock file incompatibility, consider upgrading your Nix version.

## Implemented

- query for vulnerable dependencies in cabal plan
- human readable output
- machine readable output 
- fix version suggestion

## Contributing

Contributions are welcome. 

Building the project in a non-nix environment should be as easy as `cabal build`, the build is tested against multiple ghc versions and operating systems in the CI so it should always work with one of these. If you don't use nix, installing the necessary tooling is as always possible with [ghcup](https://www.haskell.org/ghcup/).

This repo is flake-enabled. To setup a `devShell`, run `nix develop` or `direnv allow`.
If you change dependencies, please run `regen-nix` to regenerate the nix derivations.
