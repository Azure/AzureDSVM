# Delete a Resource Group
#
# 2017-02-27 09:10:09 Graham Williams

source("common.R")

# Check if the resource group already exists. Take note this script
# will not remove the resource group if it pre-existed.

rg_pre_exists <- existsRG(context, RG, LOC) %T>% print()

if (rg_pre_exists)
{
  # Delete the resource group RG.

  # Note that to delete a resource group can take some time, like 10 minutes.

  azureDeleteResourceGroup(context, RG)

}
