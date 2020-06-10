package = fission

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

##########################################################################################

init:
	cp addon-manifest.json.example addon-manifest.json && cp env.yaml.example env.yaml

setup:
	stack install ghcid && stack install yesod-bin

##########################################################################################

serve:
	$(stack) run fission-server

build:
	$(stack) build --fast $(package):lib

build-cli:
	$(stack) build --fast $(package):fission-cli

build-web:
	$(stack) build --fast $(package):fission-server

##########################################################################################

release:
	$(stack) build

release-cli:
	$(stack) build fission:fission-cli

release-cli-ubuntu:
	 $(stack) build fission:fission-cli --docker --docker-image=fpco/stack-build:lts-15

release-server:
	$(stack) build fission:fission-server

##########################################################################################

install-dev:
	$(stack) install --fast

install:
	$(stack) install
##########################################################################################

dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

profile:
	$(stack) --work-dir .stack-work-profiling --profile build --fast

##########################################################################################

ghci:
	$(stack) repl $(package):lib --no-build --no-load --ghci-options='-j6 +RTS -A128m'

quality:
	make test && make lint

lint:
	$(stack) test :fission-lint --fast

docs:
	$(stack) haddock $(package) --open

docserver:
	http-server ./.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/doc/html/$(package) -p 1313 & \
    open http://localhost:1313

doctest:
	$(stack) test :fission-doctest --fast

unit-test:
	$(stack) test :fission-test --fast

test:
	make unit-test && make doctest

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j6 +RTS -A128m'

##########################################################################################

bench:
	$(stack) build --bench $(package)

bench_http1:
	ab -n 10000 -c 100 http://localhost:1337/ping/

bench_http2:
	h2load -n10000 -c100 -t2 http://localhost:1337/ping/

##########################################################################################

dev:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test"

live:
	$(stack) exec -- yesod devel

##########################################################################################

hash-cli:
	openssl dgst -sha256 ~/.local/bin/fission-cli

##########################################################################################

.PHONY : build dirty run install ghci test test-ghci watch doctest lint
