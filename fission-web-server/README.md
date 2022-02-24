# Fission Web Server

![Continuous Integration](https://github.com/fission-suite/fission/workflows/Continuous%20Integration/badge.svg)
![License](https://img.shields.io/github/license/fission-suite/fission)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://fission.codes/discord)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

Seamlessly deploy websites and store secure user data

### Binary Releases

Grab the latest binary for your operating system from our [release page](https://github.com/fission-suite/fission/releases).

You'll find the most up to date instructions for [installation](https://guide.fission.codes/hosting/installation) and [getting started](https://guide.fission.codes/hosting/getting-started) in our [Guide](https://guide.fission.codes).

If using Linux, install `libpq-dev`

## Web API Documentation

Available at https://runfission.com/docs

## Development

### Setup

Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

Install PostgreSQL database.

On MacOS with Homebrew:

`brew install stack`
`brew install postgresql`

### Create Database

```shell
$ psql
> CREATE DATABASE web_api;

```

Migrations will be performed automatically when running the server

### Using Docker

To mimic the full "fission stack" for local development, you can use the included `docker-compose.yml`. This assumes that you have Docker installed locally:

1. Copy the env.example file to `.env` (these values are used by docker compose). Edit as desired (defaults will work)
2. Start docker compose: `docker compose up -d`. This will start postgres () and powerdns containers, including a local DNS resolver.
3. Create the necessary zones (specified in `.env`) e.g.: `docker compose exec dns-auth pdnsutil create-zone fissionapp.test`
4. Point your local DNS resolver to localhost (on macOS this is under System Preferences > Network > Advanced).
  
You can now build / run the haskell server. Make sure `env.yaml` values are updated appropriately. The following env.yaml snippet matches the `env.example` included:

```yaml
web:
  host: http://runfission.test
  port: 1337
  tls: false
  environment: localhost
  pretty: true
  monitor: false
  useEKG: false
  zone_id: runfission.test.
-- 
storage:
  stripe_count: 4
  conns_per_stripe: 50
  conn_ttl: 10
  postgresql:
    host: localhost
    database: web_api
    username: postgres
    password: postgres
-- 
pdns:
  api_url: http://localhost:8081
  api_key: changeme
--
fission_file_system:
  base_user_data_root_domain: fissionuser.test
  base_user_data_zone_id: fissionuser.test.
  default_data_cid: Qmc5m94Gu7z62RC8waSKkZUrCCBJPyHbkpmGzEePxy2oXJ # empty string
--
web_app:
  base_domain_name: fissionapp.test
  base_aws_zone_id: fissionapp.test.
```