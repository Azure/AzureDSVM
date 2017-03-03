# Delete all my_* Resource Groups
#
# 2017-02-27 09:10:09 Graham Williams

library(stringr)
source("common.R")

# List the resource groups.

context %>%
  azureListRG() %>%
  extract2("name") %>%
  grep(pattern="^my_", value=TRUE) %T>%
  print() ->
rgs

# Check if the resource group already exists. Take note this script
# will not remove the resource group if it pre-existed.

for (r in rgs)
{
  # Note that to delete a resource group can take some time, like 10 minutes.

  cat(r, "\n")

  if (existsRG(context, r, LOC))
  {
    azureDeleteResourceGroup(context, r)
  }
}
