# Makefile -*- mode: makefile -*-
#
EASK ?= eask

all: compile

compile:
	$(EASK) compile

package:
	$(EASK) package
