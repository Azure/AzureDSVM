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
	\tvdeploy     \tRun DeployDSVM vignette (single Linux DSVM, cleanup).\n\
	\tvcluster    \tRun ClusterDSVM vignette (cluster Linux DSVMs, cleanup).\n\
	\n\
	Testing (RG my_)\n\
	\tresources   \tList all resources in each resource group.\n\
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

.PHONY: vdeploy vcluster

vdeploy: vignettes
	(cd vignettes; Rscript DeployDSVM.R)

vcluster: vignettes
	(cd vignettes; Rscript ClusterDSVM.R)

.PHONY: resources deploy delete ping

resources:
	(cd test; Rscript resources.R)

deploy: 
	(cd test; Rscript deployDSVM.R)

delete: 
	(cd test; Rscript deleteRG.R)

ping:
	ssh -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
	testdsvm.southeastasia.cloudapp.azure.com uptime
