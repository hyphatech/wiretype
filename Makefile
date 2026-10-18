SWITCH ?= hypha-wiretype
OPAM = opam exec --switch=$(SWITCH) --

.DEFAULT_GOAL := help
.PHONY: help setup build test test-zod lint fmt doc

help: ## list targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

setup: ## create the switch and install dependencies
	@command -v bun >/dev/null 2>&1 || echo "The Zod rows need Bun 1.3 or later."
	@opam switch list --short | grep -qx $(SWITCH) || opam switch create $(SWITCH) --empty -y
	opam install . --switch=$(SWITCH) --deps-only --with-test --with-dev-setup -y
	cd test/zod && bun install --frozen-lockfile

build: ## build everything
	$(OPAM) dune build @all

test: test-zod ## run the tests
	$(OPAM) dune test --force

# The kinds' rows as Zod reads them, beside the OCaml suite's.
test-zod: ## run the kinds' rows through Zod
	cd test/zod && bun test

# Release is the profile opam installs with.
lint: ## check formatting, docs and the release build
	$(OPAM) dune build @all @fmt @doc
	$(OPAM) dune build --profile release @all

fmt: ## format the code
	$(OPAM) dune fmt

doc: ## build the API docs into _build/default/_doc/_html
	$(OPAM) dune build @doc
