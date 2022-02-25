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
2. Start docker compose: `docker compose up -d`. This will start postgres, ipfs and powerdns containers, including a local DNS resolver.
3. Create the three necessary zones (specified in `.env`):
   -  `docker compose exec dns-auth pdnsutil create-zone runfission.test`
   -  `docker compose exec dns-auth pdnsutil create-zone fissionuser.test`
   -  `docker compose exec dns-auth pdnsutil create-zone fissionapp.test`
4. Point your local DNS resolver to localhost.
   - on macOS: this is under System Preferences > Network > Advanced.
   - on Linux: Add `nameserver 127.0.0.1` to `/etc/resolv.conf`
  
You can now build / run the haskell server. The included `env.yaml.example` is configured to work with the local docker setup.

#### Local DNS troubleshooting 

To make sure your local DNS setup is working, try running: `dig runfission.test`. You should see a response containing: 

```
runfission.test.        3600    IN      SOA     a.misconfigured.dns.server.invalid. hostmaster.runfission.test. 0 10800 3600 604800 3600
```

If you don't see that, you can try the following steps:
1. Ensure the local DNS server is set up for the zone, e.g. `dig runfission.test -p 5300 @127.0.0.1`. If that fails, make sure the zone is created (see above).
2. Ensure the local resolver is working, e.g. `dig runfission.test @127.0.0.1`. If that fails, make sure the zone exists in `.env`.
3. If `dig runfission.test` still fails, ensure your system is set to use the local resolver. Also, try disabling any VPN software (Tailscale, etc) as they may conflict with DNS resolution.