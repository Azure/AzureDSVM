# R Specific

include r.mk

# GIT Specific

include git.mk

# Utilities for Testing

vdeploy: vignettes
	(cd vignettes; Rscript DeployDSVM.R)

vcluster: vignettes
	(cd vignettes; Rscript ClusterDSVM.R)

deploy: 
	(cd test; Rscript deployDSVM.R)

delete: 
	(cd test; Rscript deleteRG.R)

ping:
	ssh -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
	testdsvm.southeastasia.cloudapp.azure.com uptime
