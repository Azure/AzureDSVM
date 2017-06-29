# test for extensions.

if(interactive()) library("testthat")

library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

context("DSVM extensions")

asc <- createAzureContext()

with(config,
     setAzureContext(asc, tenantID=tenantID, clientID=clientID, authKey=authKey)
)
azureAuthenticate(asc)

resourceGroup_name <- paste0("AzureDSVMtest_", timestamp)
location           <- "southeastasia"

azureCreateResourceGroup(asc, 
                         location=location, 
                         resourceGroup=resourceGroup_name)

dsvm_size     <- "Standard_D4_v2"
dsvm_os       <- "Ubuntu"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_pubkey   <- pubkey_gen()
dsvm_username <- "dsvmuser"

deployDSVM(asc, 
           resource.group=resourceGroup_name,
           location=location,
           hostname=dsvm_name,
           username=dsvm_username,
           size=dsvm_size,
           os=dsvm_os,
           authen="Key",
           pubkey=dsvm_pubkey,
           mode="Sync")

context(" - Can add extension to a Ubuntu DSVM")

test_that("add an extension", {
  # add an extension to the DSVM.
  # script to execute for extension is from DataScienceVM github repo. It is 
  # to add new users to the DSVM and update CNTK from 2.0 to 2.0RC2.
  
  file_url <- "https://raw.githubusercontent.com/Azure/DataScienceVM/master/Extensions/General/create-user-and-updatecntk.sh"
  command  <- "./create-user-and-updatecntk.sh"
  
  res <- addExtensionDSVM(asc,
                          location=location,
                          resource.group=resourceGroup_name, 
                          hostname=dsvm_name, 
                          fileurl=file_url, 
                          command=command)
  
  expect_true(res)
})

context(" - Succesfully added extension")

test_that("check info of extension", {
  res <- checkExtensionDSVM(asc,
                            resource.group = resourceGroup_name, 
                            hostname = dsvm_name)
  
  expect_is(res, "list")
  expect_identical(res$properties$provisioningState, "Succeeded")
})

azureDeleteResourceGroup(asc, resourceGroup=resourceGroup_name)