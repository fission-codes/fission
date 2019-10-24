package = fission-web-api

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build $(package):lib

dev:
	$(stack) build --fast $(package):lib

# live-dev: ## Run the server in fast development mode. See DevelMain for details.
# 	ghcid \
# 	    --command "stack repl $(package):fission-web" \
# 	    --test "DevelMain.update"

dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

profile:
	$(stack) --work-dir .stack-work-profiling --profile build

run:
	$(stack) build --fast && $(stack) exec -- $(package)

install:
	$(stack) install --fast

release:
	$(stack) install

ghci:
	$(stack) repl $(package):lib --no-build --no-load --ghci-options='-j6 +RTS -A128m'

test:
	$(stack) build --test --fast $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j6 +RTS -A128m'

bench:
	$(stack) build --fast --bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind -j4 +RTS -A128m' --main-is $(package):fission-web"

prep:
	stack install ghcid

.PHONY : build dirty run install ghci test test-ghci ghcid
