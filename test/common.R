# Install the packages if required.

# devtools::install_github("Microsoft/AzureSMR")
# devtools::install_github("Azure/AzureDSR", auth_token=GIT_TOKEN)

# Load the required packages.

library(AzureSMR)    # Support for managing Azure resources.
library(AzureDSR)    # Further support for the Data Scientist.
library(magrittr)
library(dplyr, warn.conflicts=FALSE)

# Load the required subscription resources: TID, CID, and KEY.

USER <- Sys.info()[['user']]

source(paste0(USER, "_credentials.R"))

# Parameters for this script: the name for the new resource group and
# its location across the Azure cloud. The resource name is used to
# name the resource group that we will create transiently for the
# purposes of this script.

BASE <- 
  runif(4, 1, 26) %>%
  round() %>%
  letters[.] %>%
  paste(collapse="")

RG <- paste0("my_dsvm_", BASE,"_rg_sea")

# Choose a data centre location.

LOC <- "southeastasia"

# Include the random BASE in the hostname to reducely likelihood of
# conflict.

HOST <- paste0("my", BASE)

# Connect to the Azure subscription and use this as the context for
# our activities.

context <- createAzureContext(tenantID=TID, clientID=CID, authKey=KEY)

printTestSummary <- function(all=TRUE)
{
  cat("\n")
  if (all)
  {
    sprintf("Base name:\t\t%s", BASE) %>% cat("\n")
    sprintf("Resource group:\t\t%s", RG) %>% cat("\n")
    sprintf("Data centre location:\t%s", LOC) %>% cat("\n")
    sprintf("Hostname:\t\t%s", HOST) %>% cat("\n")
    cat("\n")
  }
  print(context)
  cat("\n")
}
