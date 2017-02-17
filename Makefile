VER=$(shell grep Version: DESCRIPTION | cut -d" " -f2)
PKG=$(shell basename '${PWD}')

# R Specific

include r.mk

# GIT Specific

include git.mk

# Cleanup

