<div align="center">
  <h1> <code> cabal-audit </code> </h1>
  <a href="https://github.com/mangoiv/cabal-audit/actions">
    <img src="https://github.com/mangoiv/cabal-audit/actions/workflows/cabal-audit.yml/badge.svg" alt="CI">
  </a>
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

If you don't use `nix`, you can also build from source with `cabal` or
[download a static executable from one of the latest workflow runs](https://github.com/MangoIV/cabal-audit/releases/tag/nightly).

> [!Note]
> We also have a `cachix`. If you trust me (which I do not recommend, never trust anybody!), run `cachix use cabal-audit` to 
> download directly from the cachix and skip building.

## Usage

Run `cabal-audit` to scan your project for known vulnerabilities:

```console
λ nix run github:mangoiv/security-advisories/mangoiv/cabal-audit-osv#cabal-audit --
trying to clone https://github.com/haskell/security-advisories
Cloning into '/tmp/cabal-audit-726d3e9345b766bc'...
remote: Enumerating objects: 172, done.
remote: Counting objects: 100% (172/172), done.
remote: Compressing objects: 100% (129/129), done.
remote: Total 172 (delta 6), reused 114 (delta 1), pack-reused 0
Receiving objects: 100% (172/172), 116.55 KiB | 1.31 MiB/s, done.
Resolving deltas: 100% (6/6), done.

Found advisories:

dependency "base" at version 4.18.1.0 is vulnerable for:
  HSEC-2023-0007 "readFloat: memory exhaustion with large exponent"
  published: 2024-04-23 12:43:30 +1000
  https://haskell.github.io/security-advisories/advisory/HSEC-2023-0007
  No fix version available
  toml, parser, dos

dependency "process" at version 1.6.17.0 is vulnerable for:
  HSEC-2024-0003 "process: command injection via argument list on Windows"
  published: 2024-04-23 12:43:30 +1000
  https://haskell.github.io/security-advisories/advisory/HSEC-2024-0003
  Fix available since version 1.6.19.0
  windows
```

> [!Note]
> If you encounter an error related to lock file incompatibility, consider upgrading your Nix version.

## Features

- **Query vulnerabilities**: Scans project dependencies for known vulnerabilities.
- **Human-readable output**: Displays human-readable results.
- **Machine-readable output**: Displays machine-readable output as json containing osvs.
- **Cabal solver**: Matches vulnerabilities against project dependencies.
- **Fix version suggestion**: Provides fix version or a link to relevant advisories.

## Open Features

- Provide a range of fix versions instead of just the latest.
- Suggest the latest package version on Hackage that is greater than the fix version.
- Additional Cabal solver options.
- More structured monadic code. (also enables disabling colouring)
- upload package on hackage

## Contributing

Contributions are welcome. This repo is flake-enabled. To setup a `devShell`, run `nix develop` or `direnv allow`.
