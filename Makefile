.DEFAULT_GOAL = help

ACCOUNT_ID := 975050288432
AWS := PYTHONPATH= aws

build: ## build 
	spago build

test: ## test
	spago build

clean: ## clean
	find . -name \*~ | xargs rm -f

clobber: clean ## clobber dev env

dev: ## nix develop


help: ## help
	-@grep --extended-regexp '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed 's/^Makefile://1' \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

