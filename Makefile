.DEFAULT_GOAL = help

ACCOUNT_ID := 975050288432
AWS := PYTHONPATH= aws

run: ## run
	spago run

buildc: ## build
	spago build --watch

build: ## build
	spago build

test: ## test
	spago test

bundle: ## bundle
	spago bundle-app

repl: ## repl
	spago repl

update: ## update packages
	spago install

# call make run first
install-dev: bundle ## copy index.[html, js] to ami/static
	cp -p index.js ./static/.
	cp -p ./static/index.js ../ami/static/.

.PHONY: static
static: ## creat the static dir
	rsync \
	--archive \
	--delete \
	--exclude=./index.js \
	--verbose \
	../ami/static/ ./static/

clean: ## clean
	find . -name \*~ -or -name \#\*\# | xargs rm -f

clobber: clean ## clobber
	rm -rf output/*
	rm -rf node_modules/*

dev: ## nix develop
	nix develop

clobber: clean ## clobber dev env

help: ## help
	-@grep --extended-regexp '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed 's/^Makefile://1' \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

init: install ## init project
	npm init
	[ -e "spago.dhall" ] || spago init

install: ## install npm pagkes
	for i in npm spago; do echo $$i; npm install $$i; done
