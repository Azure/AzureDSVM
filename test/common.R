# Install the packages if required.

# devtools::install_github("Microsoft/AzureSMR")
# devtools::install_github("Azure/AzureDSR", auth_token=GIT_TOKEN)

# Load the required packages.

library(AzureSMR)    # Support for managing Azure resources.
library(AzureDSR)    # Further support for the Data Scientist.
library(magrittr)
library(dplyr)

# Load the required subscription resources: TID, CID, and KEY.

USER <- Sys.getenv("USER")

source(paste0(USER, "_credentials.R"))

# Parameters for this script: the name for the new resource group and
# its location across the Azure cloud. The resource name is used to
# name the resource group that we will create transiently for the
# purposes of this script.

BASE <- 
  runif(4, 1, 26) %>%
  round() %>%
  letters[.] %>%
  paste(collapse="") %T>%
  {sprintf("Base name:\t\t%s", .) %>% cat("\n")}

RG <-
  paste0("my_dsvm_", BASE,"_rg_sea") %T>%
  {sprintf("Resource group:\t\t%s", .) %>% cat("\n")}

# Choose a data centre location.

LOC <-
  "southeastasia"  %T>%
  {sprintf("Data centre location:\t%s", .) %>% cat("\n")}

# Include the random BASE in the hostname to reducely likelihood of
# conflict.

HOST <-
  paste0("my", BASE) %T>%
  {sprintf("Hostname:\t\t%s", .) %>% cat("\n")}

cat("\n")

# Connect to the Azure subscription and use this as the context for
# our activities.

context <- createAzureContext(tenantID=TID, clientID=CID, authKey=KEY)

