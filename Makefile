VER=$(shell grep Version: DESCRIPTION | cut -d" " -f2)
PKG=$(shell basename '${PWD}')

# R Specific

include r.mk

# GIT Specific

include git.mk

# Utilities

deploy: scripts
	(cd vignettes; Rscript DeployDSVM.R)

delete: scripts
	(cd vignettes; Rscript DeleteRG.R)


ping:
	ssh -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null myldsvm.southeastasia.cloudapp.azure.com uptime
