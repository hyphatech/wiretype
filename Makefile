SWITCH ?= wiretype
OPAM = opam exec --switch=$(SWITCH) --

DOCS_PORT ?= 4321
# The reference is odoc's, for wiretype's own packages and nothing under them.
REFERENCE = wiretype ppx_wiretype odoc.support

.DEFAULT_GOAL := help
.PHONY: help setup build test test-zod lint fmt docs docs-serve

help: ## list targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

# A switch is this repository's when its variable repository names this
# repository's git directory, which setup sets as it makes it: a switch of the
# same name made for anything else is refused, never used.
setup: ## create the switch and install dependencies
	@command -v bun >/dev/null 2>&1 || echo "The Zod rows and the documentation site need Bun 1.3 or later."
	@repository=$$(git rev-parse --path-format=absolute --git-common-dir); \
	if opam switch list --short | grep -qx '$(SWITCH)'; then \
	  made_for=$$(opam var --switch='$(SWITCH)' repository 2>/dev/null); \
	  [ -n "$$repository" ] && [ "$$made_for" = "$$repository" ] || { echo "The switch $(SWITCH) is not this repository's. Remove it, or name another with SWITCH=<name>." >&2; exit 2; }; \
	else \
	  opam switch create '$(SWITCH)' --empty --no-switch -y >/dev/null && \
	  opam var --switch='$(SWITCH)' "repository=$$repository" >/dev/null; \
	fi
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

docs/node_modules: docs/package.json docs/bun.lock
	cd docs && bun install --frozen-lockfile
	@touch docs/node_modules

# odoc's reference from the .mli files is copied into public/, which
# Starlight serves as it is, since it cannot read an .mli; then the site,
# whose pages import each example with ?raw into Starlight's <Code>.
docs: docs/node_modules ## build the documentation site
	$(OPAM) dune build @doc
	rm -rf docs/public/reference
	mkdir -p docs/public/reference
	cd _build/default/_doc/_html && cp -R $(REFERENCE) $(CURDIR)/docs/public/reference/
	chmod -R u+w docs/public/reference
	cd docs && bun --bun astro build

# The built site, as Pages serves it: Pagefind's index exists only in a
# build, and a page served as it changes loads its styles after its text.
docs-serve: docs ## serve the built documentation site
	cd docs && bun --bun astro preview --open --port $(DOCS_PORT)
