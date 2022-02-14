help:
	@cat Makefile

DOCKER_TAG?=latest
DOCKER_IMAGE:=tdf
#DOCKER_IMAGE:=gibiansky/ihaskell

DOCKER:=docker
DOCKER_URI:=$(DOCKER_IMAGE):$(DOCKER_TAG)
PYTHON3=python3
PIP=pip

IHASKELL_HOST_PORT=$(IHASKELL_CONTAINER_PORT)
# Don't change IHASKELL_CONTAINER_PORT unless you know what you are doing
IHASKELL_CONTAINER_PORT=8888

PASSWORD?=""

PWD_MOUNT?=$(PWD)

.PHONY: test

# To activate the python virtual env (after appropriate steps below)
#
#   source env/bin/activate
#
# in your shell.

install-virtualenv:
	$(PYTHON3) -m pip install --user virtualenv

create-env:
	$(PYTHON3) -m venv env

build-ihaskell:
	docker build . -t $(DOCKER_URI)

ihaskell:
	$(DOCKER) run \
		--rm \
		-p $(IHASKELL_HOST_PORT):$(IHASKELL_CONTAINER_PORT) \
		-v $(PWD_MOUNT):/home/jovyan/src \
		--name ihaskell_notebook \
		$(DOCKER_URI) jupyter lab \
		--ServerApp.token=${PASSWORD}

install-pandas:
	$(PIP) install pandas

haddock:
	stack haddock

hlint:
	hlint ./src

test:
	stack test --fast --file-watch

watch:
	stack build --fast --file-watch

hl: hlint
t: test
w: watch
