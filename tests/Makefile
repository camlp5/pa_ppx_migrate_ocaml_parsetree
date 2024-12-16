# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGEPREFIX=pa_ppx_

DESTDIR=

PACKAGES := $(PACKAGES),oUnit,fmt,rresult,compiler-libs.common,sexplib0,uint,pa_ppx.utils,pa_ppx.testutils,pa_ppx.import,pa_ppx_migrate,camlp5.pa_r.link,pa_ppx.base.link

ML=\
	test1.ml \

MLI=\


ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

TESTS= \
	test1.byte \

all: $(TESTS)

test: $(TESTS:.byte=)

% : %.byte
	mkdir -p _build && ./$<

test1.byte: test1.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

clean::
	rm -f  *.ppo.*
