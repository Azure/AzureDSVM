VER=$(shell grep Version: DESCRIPTION | cut -d" " -f2)
PKG=$(shell basename '${PWD}')

# R Specific

include r.mk

# GIT Specific

include git.mk

# Utilities for Testing

vtest: vignettes
	(cd vignettes; Rscript DeployDSVM.R)

deploy: 
	(cd test; Rscript deployDSVM.R)

delete: 
	(cd test; Rscript deleteRG.R)

ping:
	ssh -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
	testdsvm.southeastasia.cloudapp.azure.com uptime
