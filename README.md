![](https://github.com/fission-suite/ipfs-api/raw/master/assets/logo.png?sanitize=true)

# FISSION IPFS Web API

[![Build Status](https://travis-ci.org/fission-suite/ipfs-api.svg?branch=master)](https://travis-ci.org/fission-suite/ipfs-api)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/fission-suite/blob/master/LICENSE)
[![Maintainability](https://api.codeclimate.com/v1/badges/44fb6a8a0cfd88bc41ef/maintainability)](https://codeclimate.com/github/fission-suite/ipfs-api/maintainability)
[![Built by FISSION](https://img.shields.io/badge/⌘-Built_by_FISSION-purple.svg)](https://fission.codes)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://discord.gg/zAQBDEq)

A library and application to help Web 2.0-style applications leverage IPFS
in a familiar, compatible way

# QuickStart

```shell
# IPFS on MacOS, otherwise https://docs.ipfs.io/introduction/install/
brew install ipfs
brew service start ipfs

# Web Server
stack run server

# Local Request
curl \
  -H "Content-Type: text/plain;charset=utf-8" \
  -H "Authorization: Basic Q0hBTkdFTUU6U1VQRVJTRUNSRVQ=" \
  -d"hello world" \
  http://localhost:1337/ipfs/
```

# Configuration

## Environment Variables

Environment variables are set via the command line (e.g. `export PORT=80`)

### `PORT`

Default: `1337`

The port to run the web server on.

# Live Monitoring

```
export MONITOR=true
stack run server
open http://localhost:9630
```

# Load Test

A very simple local load test:

```shell
# HTTP1.1
ab -n 10000 -c 100 http://localhost:1337/ping/

# HTTP2
brew install nghttp2
h2load -n10000 -c100 -t2 http://localhost:1337/ping/
```

# Manual Workflow

```shell
curl -H "Authorization: Basic 012345generatedfrommanifest==" \
     -H "Content-Type: application/json" \
     -H "Accept: application/vnd.heroku-addons+json; version=3" \
     -X POST http://localhost:1337/heroku/resources/ \
     -d '{
           "name"         : "my-awesome-app",
           "uuid"         : "01234567-89ab-cdef-0123-456789abcdef",
           "plan"         : "free",
           "callback_url" : "http://foo.bar.quux",
           "region"       : "amazon-web-services::ap-northeast-1"
         }' | json_pp

#   % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
#                                  Dload  Upload   Total   Spent    Left  Speed
# 100   563    0   372  100   191  20871  10716 --:--:-- --:--:-- --:--:-- 21882
# {
#    "message" : "Successfully provisioned Interplanetary FISSION!",
#    "id"      : 5,
#    "config"  : {
#       "INTERPLANETARY_FISSION_USERNAME" : "c74bd95b8555275277d4",
#       "INTERPLANETARY_FISSION_PASSWORD" : "GW0SHByPmY0.y+lg)x7De.PNmJvh1",
#       "INTERPLANETARY_FISSION_URL"      : "localhost:1337/ipfs"
#    }
# }
```

Encode basic auth from `INTERPLANETARY_FISSION_USERNAME:INTERPLANETARY_FISSION_PASSWORD`
(e.g. [Basic Authentication Header Generator](https://www.blitter.se/utils/basic-authentication-header-generator/))


```shell
curl -i \
     -X POST \
     -H "Content-Type: application/octet-stream" \
     -H "Authorization: Basic Yzc0YmQ5NWI4NTU1Mjc1Mjc3ZDQ6R1cwU0hCeVBtWTAueStsZyl4N0RlLlBObUp2aDE" \
     -d '{"hi":1}' \
     http://localhost:1337/ipfs

# HTTP/1.1 200 OK
# Transfer-Encoding: chunked
# Date: XXXXXXXXXXXXXXXXXXXXXXXX
# Server: Warp/3.2.27
# Content-Type: text/plain;charset=utf-8
#
# QmSeDGD18CLeUyKrATcaCbmH4Z3gyh3SrdgjoYkrxkEmgx
```

# MIME Types

## Valid Types

* `application/octet-stream`
* `text/plain; charset=UTF-8`
* `multipart/form-data`

## Defaults

* `Content-Type`
  * Must be specified (no default)
  * Typically want `application/octet-stream`
* `Accept: text/plain;charset=utf-8`
* NB: When requesting `text/plain`, the character set must be specified

## Example

```shell
curl -i \
    -X POST \
    -H "Authorization: Basic abcdef==" \
    -H "Content-Type: application/octet-stream" # This line
    -d '{"hi":1}' \
    http://localhost:1337/ipfs

# HTTP/1.1 200 OK
# Transfer-Encoding: chunked
# Date: XXXXXXXXXXXXXXXXXXXX
# Server: Warp/3.2.27
# Content-Type: text/plain;charset=utf-8
#
# QmSeDGD18CLeUyKrATcaCbmH4Z3gyh3SrdgjoYkrxkEmgx
```
