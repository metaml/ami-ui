.DEFAULT_GOAL = help

ACCOUNT_ID := 975050288432
AWS := PYTHONPATH= aws

run: ## run
	spago run

build: ## build 
	spago build

test: ## test
	spago test

bundle: ## bundle
	spago bundle-app --platform node

clean: ## clean
	find . -name \*~ | xargs rm -f

dev: ## nix develop
	nix develop

clobber: clean ## clobber dev env

dev: ## nix develop

install: ## install npm pagkes
	for i in $$(cat packages.npm); do echo $$i; npm install $$i; done

help: ## help
	-@grep --extended-regexp '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed 's/^Makefile://1' \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

init: ## init project
	spago init
	npm init
