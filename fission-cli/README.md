# Fission CLI

![Continuous Integration](https://github.com/fission-suite/fission/workflows/Continuous%20Integration/badge.svg)
![License](https://img.shields.io/github/license/fission-suite/fission)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://fission.codes/discord)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

Seamlessly deploy websites and store secure user data

## QuickStart

### MacOS

```shell
# IPFS on MacOS, otherwise https://docs.ipfs.io/introduction/install/
brew install ipfs
brew services start ipfs

brew install fission-suite/fission/fission-cli
```

Or to build from source:

```shell
# Dependencies
brew install stack
brew install postgresql

# Go to the Fission repo
cd $FISSION_REPO

# Build & install
stack install --no-nix fission:fission-cil

# Build only
stack build --no-nix fission:fission-cli 
```

### Binary Releases

Grab the latest binary for your operating system from our [release page](https://github.com/fission-suite/fission/releases).

You'll find the most up to date instructions for [installation](https://guide.fission.codes/hosting/installation) and [getting started](https://guide.fission.codes/hosting/getting-started) in our [Guide](https://guide.fission.codes).

If using Linux, install `libpq-dev`

### Seamless Deployments
Deployments are just one step: `fission app publish`


```
$ fission up hello-universe/
🚀 Now live on the network
👌 QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN
📝 DNS updated! Check out your site at:
🔗 hello-universe.fission.name
```

Simple as that!

If you'd like to redeploy everytime you change a file, use `fission app publish --watch`

## Development

### Setup

Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

Install PostgreSQL database.

On MacOS with Homebrew:

`brew install stack`
`brew install postgresql`

### Build

``` shell
stack install fission-cli
```
