# DSVM extensions.

library(jsonlite)

x <- readLines("inst/etc/template_linux_key_ubuntu.json")
y <- readLines("inst/etc/extensions.json")

length(x) <- length(x) - 3

z <- paste0(x,
            ",",
            y, 
            "]",
            "}",
            collapse="")

library(AzureDSVM)
library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

asc <- createAzureContext()

with(config,
     setAzureContext(asc, tenantID=tenantID, clientID=clientID, authKey=authKey)
)
azureAuthenticate(asc)

resourceGroup_name <- paste0("AzureDSVMtest_", timestamp)
location           <- "southeastasia"

res <- azureCreateResourceGroup(asc, 
                                location=location, 
                                resourceGroup=resourceGroup_name)

dsvm_size     <- "Standard_D4_v2"
dsvm_os       <- "Ubuntu"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_password <- "AzureDSVM_test123"
dsvm_username <- "dsvmuser"

res <- deployDSVM(asc, 
                  resource.group=resourceGroup_name,
                  location=location,
                  hostname=dsvm_name,
                  username=dsvm_username,
                  size=dsvm_size,
                  os=dsvm_os,
                  authen="Password",
                  pubkey="",
                  password=dsvm_password,
                  mode="Sync")