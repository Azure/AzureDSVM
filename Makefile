########################################################################
# DOCUMENTATION

.PHONY: help
help:
	@echo -e "Manage AzureDSR package\n\
	=======================\n\
	Package\n\
	\tinstall     \tBuild and install to local machine\n\
	\n\
	Vignettes\n\
	\tvdeploy     \tRun 10Deploy vignette (single Linux DSVM, cleanup).\n\
	\tvmulti      \tRun 20Multi  vignette (multipl Linux DSVMs, cleanup).\n\
	\tvcompute    \tRun 30Compute vignette.\n\
	\n\
	Testing (RG my_)\n\
	\tlist        \tList all resources in each resource group.\n\
	\tdeploy      \tDeploy single Linux DSVM with new resource group.\n\
	\tdelete      \tDelete resource groups beginning with my_.\n\
	\n\
	Version Control\n\
	\tstatus      \t.\n\
	\tpull        \t.\n\
	\tpush        \t.\n\
	\tdiff        \t.\n\
	"

########################################################################
# R Specific

include r.mk

########################################################################
# GIT Specific

include git.mk

########################################################################
# Utilities for Testing

.PHONY: vdeploy vmulti vcompute

vdeploy: vignettes
	(cd vignettes; Rscript 10Deploy.R)

vmulti: vignettes
	(cd vignettes; Rscript 20Multi.R)

vcompute: vignettes
	(cd vignettes; Rscript 30Compute.R)

.PHONY: resources deploy delete ping

list:
	(cd test; Rscript listRG.R)

deploy: 
	(cd test; Rscript deployDSVM.R)

delete: 
	(cd test; Rscript deleteRG.R)

ping:
	ssh -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
	testdsvm.southeastasia.cloudapp.azure.com uptime
