help:
	@cat Makefile

PYTHON3=python3
PIP=pip

# To activate the python virtual env (after appropriate steps below)
#
#   source env/bin/activate
#
# in your shell.

install-virtualenv:
	$(PYTHON3) -m pip install --user virtualenv

create-env:
	$(PYTHON3) -m venv env

install-pandas:
	$(PIP) install pandas

watch:
	stack build --fast --file-watch

w: watch
