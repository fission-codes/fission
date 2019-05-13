<img height=200 width=200 src=https://github.com/fission-suite/ipfs-api/raw/master/assets/logo.png?sanitize=true>

# FISSION IPFS Web API

[![Build Status](https://travis-ci.org/fission-suite/ipfs-api.svg?branch=master)](https://travis-ci.org/fission-suite/ipfs-api)

A library and application to help Web 2.0-style applications leverage IPFS
in a familiar, compatible way

## QuickStart

### Docker

```shell
docker build -t expede/fission .
docker run -it -p 8000:8000 expede/fission:latest
```

### Stack

```shell
# IPFS
brew install ipfs # or https://docs.ipfs.io/introduction/install/
brew service start ipfs

# TLS Certificates
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

# Web Server
stack run server

# Make Request
curl --insecure
  -iX POST \
  -H "Content-Type: text/plain;charset=utf-8" \
  -H "Authorization: Basic Q0hBTkdFTUU6U1VQRVJTRUNSRVQ=" \
  -d"hello world" \
  https://localhost:443/ipfs/
```
