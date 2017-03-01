PKG_VERSION = $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME    = $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

RSCRIPT      = Rscript
RSCRIPT_OPTS = --vanilla

VIGNETTE_RMD = $(wildcard vignettes/*.Rmd)
VIGNETTE_R   = $(VIGNETTE_RMD:.Rmd=.R)

R_FILES   := $(wildcard R/*.R)
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(SRC_FILES)

.PHONY: list doc vignettes tarball check install build clean realclean

%.R: %.Rmd
	${RSCRIPT} ${RSCRIPT_OPTS} -e 'library(knitr);purl("$<", out="$@")'	

%.run: %.R
	cd $(shell dirname "$<");\
	${RSCRIPT} ${RSCRIPT_OPTS} -e 'source("$(shell basename $<)")'	

$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build .

list:
	@echo -e "PKG_NAME:    $(PKG_NAME)"
	@echo -e "PKG_VERSION: $(PKG_VERSION)"
	@echo -e "R_FILES:     $(R_FILES)"
	@echo -e "SRC_FILES:   $(SRC_FILES)"

doc:
	R -e 'devtools::document()'

vignettes: $(VIGNETTE_R)

tarball: doc $(PKG_NAME)_$(PKG_VERSION).tar.gz

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL -build $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

NAMESPACE: $(R_FILES)
	Rscript -e "library(roxygen2);roxygenize('.')"

clean:
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f man/*
	-rm -r -f NAMESPACE

realclean: clean
	rm -f $(VIGNETTE_R)
