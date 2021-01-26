# Fission CLI

![Continuous Integration](https://github.com/fission-suite/fission/workflows/Continuous%20Integration/badge.svg)
![License](https://img.shields.io/github/license/fission-suite/fission)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://fission.codes/discord)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

Seamlessly deploy websites and store secure user data

## QuickStart

### MacOS

```shell
brew install fission-suite/fission/fission-cli
```

Or to build from source:

```shell
# Dependencies
brew install stack

# Go to the Fission repo
cd $FISSION_REPO

# Build & install
stack install fission-cli:fission

# Build only
stack build fission-cli:fission 
```

Once you've built the CLI, it is installed in `~/.local/bin/fission`. If you already have an existing fission key in `~/.ssh`, copy it as follows:

```shell
cp ~/.ssh/fission ~/.config/fission/key/machine_id.ed25519
```

Now run `fission setup` to install the Fission-controlled IPFS node.

### Binary Releases

Grab the latest binary for your operating system from our [release page](https://github.com/fission-suite/fission/releases).

You'll find the most up to date instructions for [installation](https://guide.fission.codes/hosting/installation) and [getting started](https://guide.fission.codes/hosting/getting-started) in our [Guide](https://guide.fission.codes).

### Seamless Deployments
Deployments are just one step: `fission app publish`


```
$ fission app publish
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

On MacOS with Homebrew:

`brew install stack`

### Build

``` shell
stack install fission-cli
```
