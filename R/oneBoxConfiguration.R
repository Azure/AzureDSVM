# One-box configuration for MRS based virtual machine.

# deploy an MRS VM.

library(AzureSMR)
library(AzureDSVM)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

context("DSVM deployment (this test may last 10 to 15 minutes)")

asc <- createAzureContext()

with(config,
     setAzureContext(asc, tenantID=tenantID, clientID=clientID, authKey=authKey)
)
azureAuthenticate(asc)

azureCreateResourceGroup(asc, 
                         resourceGroup = "mrstest",
                         location = "southeastasia")

deployDSVM(context = asc, 
           resource.group = "mrstest", 
           location = "southeastasia", 
           hostname = "mrsle",
           username = "zhle", 
           size = "Standard_D4_v2", 
           os = "RServer", 
           authen = "Key",
           pubkey = config$PUBKEY)

mrsOneBoxConfiguration <- function(azureActiveContext, 
                                   vmName) {
  assert_that(AzureSMR:::is_vm_name(vmName))
  
  status <- operateDSVM(context=azureActiveContext,
                        names = 
                        operation="Check")
  
  if (status != )
}