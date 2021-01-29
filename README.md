# Fission

![Continuous Integration](https://github.com/fission-suite/fission/workflows/Continuous%20Integration/badge.svg)
![License](https://img.shields.io/github/license/fission-suite/fission)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://fission.codes/discord)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

Seamlessly deploy websites and store secure user data

This project can build multiple binaries. Please refer to the README for the specific package (e.g. fission-cli, fission-core). Here is how the projects are related to each other:

```
           fission-core
                 ^
                 |
          fission-web-api
          ^             ^
          |             |
fission-web-client   fission-web-server
          ^
          |
    fission-cli
```

### `fission-core`

Core data types, helper functions, `Fission.Prelude`

### `fission-web-api`

Declarative definitions of our web API. The contract between the web client and server.

### `fission-web-client`

Web client functions, classes, helper functions

### `fission-cli`

CLI interface to our service

### `fission-server`

Web server, database, 3rd party integrations

# Building

Fission is built inside of a pure Nix shell via the [Stack integration](https://docs.haskellstack.org/en/stable/nix_integration/). This means that you _should_ only need to type `stack build` to do a complete build of all packages.

Unfortunately, the Stack integration doesn't play well with [`lorri`](https://github.com/target/lorri). If you've run `lorri` recently, you will either need to build with `--no-nix` or unset the `STACK_IN_NIX_SHELL` environment variable.

```bash
# bash or zsh
unset STACK_IN_NIX_SHELL
```

```fish
set -e STACK_IN_NIX_SHELL
```
