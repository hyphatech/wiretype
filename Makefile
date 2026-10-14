.DEFAULT_GOAL := help
.PHONY: help setup build test test-zod lint fmt doc

help: ## list targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

# dune builds the dependencies as it needs them (dune-workspace); setup
# brings dune's tools and the Zod rows' packages.
setup: ## install the formatter, odoc, the language server and Merlin, and Zod
	@command -v node >/dev/null 2>&1 || echo "The Zod rows need Node 24 or later."
	dune tools install ocamlformat
	dune tools install odoc
	dune tools install ocamllsp
	dune tools install ocamlmerlin
	cd test/zod && npm ci

build: ## build everything
	dune build @all

test: test-zod ## run the tests
	dune test --force

# The kinds' rows as Zod reads them, beside the OCaml suite's.
test-zod: ## run the kinds' rows through Zod
	cd test/zod && node --test

# Release is the profile opam installs with.
lint: ## check formatting, docs and the release build
	dune build @all @fmt @doc
	dune build --profile release @all

fmt: ## format the code
	dune fmt

doc: ## build the API docs into _build/default/_doc/_html
	dune build @doc
