# test deployment of DSVM.

if(interactive()) library("testthat")

library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

context("One box configuration setup for MRS VM")

asc <- createAzureContext()

with(config,
     setAzureContext(asc, tenantID=tenantID, clientID=clientID, authKey=authKey)
)
azureAuthenticate(asc)

# create a new resource group.

context(" - create a new resource group")

resourceGroup_name <- paste0("AzureDSVMtest_", timestamp)
location           <- "southeastasia"

test_that("Can create resource group", {
  skip_if_missing_config(settingsfile)
  
  res <- azureCreateResourceGroup(asc, 
                                  location=location, 
                                  resourceGroup=resourceGroup_name)
  expect_equal(res, TRUE)
  
  AzureSMR:::wait_for_azure(
    resourceGroup_name %in% azureListRG(asc)$resourceGroup
  )
  expect_true(resourceGroup_name %in% azureListRG(asc)$resourceGroup)
})

context(" - Deploy a Microsoft R Server VM")

dsvm_size     <- "Standard_D4_v2"
dsvm_os       <- "RServer"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_pubkey   <- pubkey_gen()
dsvm_username <- "dsvmuser"

test_that("Deploy a Microsoft R Server VM", {
  skip_if_missing_config(settingsfile)
  
  res <- deployDSVM(asc, 
                    resource.group=resourceGroup_name,
                    location=location,
                    hostname=dsvm_name,
                    username=dsvm_username,
                    size=dsvm_size,
                    os=dsvm_os,
                    authen="Key",
                    pubkey=dsvm_pubkey,
                    mode="Sync")
  
  expect_equal(object=res, expected=paste(dsvm_name, 
                                          location, 
                                          "cloudapp.azure.com", 
                                          sep="."))
})

context(" - one-box configuration for the MRS VM.")

test_that("one-box configuration", {
  onebox_password <- "OneBox@dsvm123"
  
  res <- mrsOneBoxConfiguration(asc,
                                resource.group=resourceGroup_name,
                                location=location,
                                hostname=dsvm_name,
                                username=dsvm_username,
                                password=onebox_password)
  
  expect_true(res)
})

context(" - delete resource group")
test_that("Can delete resource group", {
  skip_if_missing_config(settingsfile)
  
  expect_message({
    res <- azureDeleteResourceGroup(asc, resourceGroup = resourceGroup_name)
  }, "Delete Request Submitted"
  )
})