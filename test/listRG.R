# List all my_* Resource Groups
#
# Time-stamp: "2017-03-10 11:51:44 Graham Williams"

library(stringr)
source("common.R")

printTestSummary(FALSE)

# List the resource groups.

cat("List the test resource groups.\n\n")

context %>%
  azureListRG() %>%
  extract2("name") %>%
  grep(pattern="^my_", value=TRUE) %T>%
  print() ->
rgs

for (r in rgs)
{
  # Note that to delete a resource group can take some time, like 10 minutes.

  cat("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
  cat(r, "\n")

  azureListAllResources(context, r) %>%
    select(name, type, location) %>%
    print()
}
