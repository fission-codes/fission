![](https://github.com/fission-suite/ipfs-api/raw/master/assets/logo.png?sanitize=true)

# FISSION IPFS Web API

[![Build Status](https://travis-ci.org/fission-suite/ipfs-api.svg?branch=master)](https://travis-ci.org/fission-suite/ipfs-api)

A library and application to help Web 2.0-style applications leverage IPFS
in a familiar, compatible way

# QuickStart

```shell
# IPFS on MacOS, otherwise https://docs.ipfs.io/introduction/install/
brew install ipfs
brew service start ipfs

# Self-Signed TLS Certificates for Local Dev
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

# Web Server
stack run server

# Local Request
# (`insecure` because of self-seigned certificate)
curl --insecure
  -iX POST \
  -H "Content-Type: text/plain;charset=utf-8" \
  -H "Authorization: Basic Q0hBTkdFTUU6U1VQRVJTRUNSRVQ=" \
  -d"hello world" \
  https://localhost:443/ipfs/
```

# Configuration

## Environment Variables

Environment variables are set via the command line (e.g. `export PORT=80`)

### `PORT`

Default: `443`

The port to run the web server on.

### `TLS_CERT`

Default: `certificate.pem`

Path to the TLS certificate.

### `TLS_KEY`

Default: `key.pem`

Path to the TLS key.
