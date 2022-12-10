SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: ci clean package install compile test checkdoc lint

ci: clean package install compile checkdoc lint

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) install-deps --dev
	$(EASK) ert ./test/*.el

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Linting..."
	$(EASK) lint package

clean:
	$(EASK) clean all
