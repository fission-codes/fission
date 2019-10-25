![](https://github.com/fission-suite/web-api/raw/master/assets/logo.png?sanitize=true)

# FISSION IPFS Web API

[![Build Status](https://travis-ci.org/fission-suite/web-api.svg?branch=master)](https://travis-ci.org/fission-suite/web-api)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/fission-suite/blob/master/LICENSE)
[![Built by FISSION](https://img.shields.io/badge/⌘-Built_by_FISSION-purple.svg)](https://fission.codes)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://discord.gg/zAQBDEq)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

A library and application to help Web 2.0-style applications leverage IPFS
in a familiar, compatible way

# QuickStart

## Install dependencies
```shell
# IPFS on MacOS, otherwise https://docs.ipfs.io/introduction/install/
brew install ipfs
brew services start ipfs

# If using Linux, install liblzma-dev
# sudo apt install liblzma-dev
```

## Setup config
```shell
# Enable sample Heroku config
# Note: You will have to edit this file to input dummy data
cp example-config/addon-manifest.json.example addon-manifest.json

# Setup env file
cp example-config/env.yaml.example env.yaml
```

## Run Server
```shell
# Dev Web Server
export RIO_VERBOSE=true
stack run fission-web
```

## Setup your database
```shell
# Create the database
createdb web_api

# Apply the projects database schema
stack repl
λ> import Fission.Storage.PostgreSQL.Migrate
λ> import Database.Selda.PostgreSQL
λ> sequence_ $ mutations "localhost" 5432 "web_api" Nothing Nothing Nothing
```

## Get your FISSION credentials
_note: you will need to fill out your addon-manifest properly to acquire the appropriate credentials_
```shell
curl -u "APP_MANIFEST_ID:APP_MANIFEST_PASSWORD" \
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
```

## Send your first request
```shell
curl \
  -H "Content-Type: text/plain;charset=utf-8" \
  -u "FISSION_USERNAME:FISSION_PASSWORD" \
  -d "hello world" \
  http://localhost:1337/ipfs
```

# Development

There is a `Makefile` filled with helpful commands. The most used in development is `make watch`.

```shell
# Install development tools
make setup

# Watch project for changes, validating types and syntax
make watch

# Run the server and live reload
make live
```

# Configuration

## Environment Variables

Environment variables are set via the command line (e.g. `export PORT=80`)

### `PORT`

Default: `1337`

The port to run the web server on.

### `TLS`

Default: `false`

Run the server with TLS enabled.

_NB: `PORT` needs to be set separately._

### `RIO_VERBOSE`

Default: `false`

Log with colours and more output. Prefixed by `RIO_` to make it compatible with `SimpleApp`.

### `PRETTY_REQS`

Default: `false`

Log HTTP requests in easy-to-read multiline format

### `DB_PATH`

Default: `web-api.sqlite`

Path to the SQLite databse

### `IPFS_PATH`

Default: `/usr/local/bin/ipfs`

Path to the local IPFS binary


### `MONITOR`

Default: `false`

Live monitoring dashboard

```
export MONITOR=true
stack run fission-web
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
#       "INTERPLANETARY_FISSION_URL"      : "localhost:1337"
#    }
# }
```

Encode basic auth from `INTERPLANETARY_FISSION_USERNAME:INTERPLANETARY_FISSION_PASSWORD` -- `curl` using the `-u USER:PASS` will encode and add the header for you automatically:


```shell
curl -i \
     -X POST \
     -H "Content-Type: application/octet-stream" \
     -u 'USER:PASS' \
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

Using the `--data-binary` and a /path/to/file can upload binary file types.

```shell
curl -i \
    -X POST \
    -u 'USER:PASS' \
    -H "Content-Type: application/octet-stream" # This line
    --data-binary @/path/to/file \
    http://localhost:1337/ipfs

# HTTP/1.1 200 OK
# Transfer-Encoding: chunked
# Date: XXXXXXXXXXXXXXXXXXXX
# Server: Warp/3.2.27
# Content-Type: text/plain;charset=utf-8
#
# QmSeDGD18CLeUyKrATcaCbmH4Z3gyh3SrdgjoYkrxkEmgx
```

Note that all of these examples will work with our public api at `hostless.dev/ipfs`. You will need a username and password from our Heroku Add-on.
