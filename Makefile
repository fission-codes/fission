package = fission-web-api

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

dev:
	$(stack) build --fast $(package):lib

release:
	$(stack) build

dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

profile:
	$(stack) --work-dir .stack-work-profiling --profile build --fast

install:
	$(stack) install --fast

ghci:
	$(stack) repl $(package):lib --no-build --no-load --ghci-options='-j6 +RTS -A128m'

test:
	$(stack) build --test --fast $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j6 +RTS -A128m'

bench:
	$(stack) build --fast --bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --main-is $(package):fission-web"

setup:
	stack install ghcid

.PHONY : build dirty run install ghci test test-ghci ghcid
