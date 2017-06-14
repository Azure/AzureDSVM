# test deployment of DSVM.

if(interactive()) library("testthat")

library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

context("DSVM deployment")

asc <- createAzureContext()

with(config,
     setAzureContext(asc, tenantID=tenantID, clientID=clientID, authKey=authKey)
)
azureAuthenticate(asc)

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

resourceGroup_name <- paste0("AzureDSVMtest_", timestamp)
location           <- "southeastasia"
dsvm_name          <- paste0("azuredsvm", timestamp)
dsvm_size          <- "Standard_D4_v2"
dsvm_os            <- "CentOS"
dsvm_password      <- "AzureDSVM_test123"
dsvm_username      <- "dsvmuser"

# create a new resource group.

context(" - create a new resource group")

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

context(" - Deploy a DSVM")

test_that("Deploy a DSVM with custom specifications", {
  skip_if_missing_config(settingsfile)
  
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
  
  expect_equal(object=res, expected=paste(dsvm_name, location, "cloudapp.azure.com", sep="."))
})

context(" - delete resource group")
test_that("Can delete resource group", {
  skip_if_missing_config(settingsfile)
  
  expect_message({
    res <- azureDeleteResourceGroup(asc, resourceGroup = resourceGroup_name)
  }, "Delete Request Submitted"
  )
})