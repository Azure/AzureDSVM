RSCRIPT      = Rscript
RSCRIPT_OPTS = --vanilla

%.R: %.Rmd
	${RSCRIPT} ${RSCRIPT_OPTS} -e 'library(knitr);purl("$<", out="$@")'	

%.run: %.R
	cd $(shell dirname "$<"); ${RSCRIPT} ${RSCRIPT_OPTS} -e 'source("$(shell basename $<)")'	


check: 
	R CMD check --check-subdirs=yes .

build: 
	R CMD build .

install: build
	R CMD INSTALL $(PKG)_$(VER).tar.gz

