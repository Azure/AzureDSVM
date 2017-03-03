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
#BASE <- XXX
RG    <- "my_dsvm_%s_rg_sea" %T>% print() # Created if needed then kill.
LOC   <- "southeastasia"     %T>% print() # Data centre location.

# Connect to the Azure subscription and use this as the context for
# our activities.

context <- createAzureContext(tenantID=TID, clientID=CID, authKey=KEY)

