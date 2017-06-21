# test operation of DSVM.

if(interactive()) library("testthat")

library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

context("DSVM operation")

asc <- createAzureContext()

with(config,
     setAzureContext(asc, tenantID=tenantID, clientID=clientID, authKey=authKey)
)
azureAuthenticate(asc)

# create a new resource group.

resourceGroup_name <- paste0("AzureDSVMtest_", timestamp)
location           <- "southeastasia"

res <- azureCreateResourceGroup(asc, 
                                location=location, 
                                resourceGroup=resourceGroup_name)

dsvm_size     <- "Standard_D4_v2"
dsvm_os       <- "CentOS"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_password <- "AzureDSVM_test123"
dsvm_username <- "dsvmuser"

context("Operate a DSVM")

test_that("- check status of a DSVM", {
  deployDSVM(asc, 
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

  res <- operateDSVM(asc, 
                     resource.group=resourceGroup_name, 
                     hostname=dsvm_name, 
                     operation="Check")
  
  expect_equal(res, "Provisioning succeeded, VM running")
  
})

test_that("- stop a DSVM", {
  res <- operateDSVM(asc, 
                     resource.group=resourceGroup_name, 
                     hostname=dsvm_name, 
                     operation="Stop")
  
  expect_equal(res, "Provisioning succeeded, VM deallocated")
}) 

test_that("- delete a DSVM", {
  res <- operateDSVM(asc, 
                     resource.group=resourceGroup_name, 
                     hostname=dsvm_name, 
                     operation="Delete")
  
  expect_true(res)
}) 

azureDeleteResourceGroup(asc, resourceGroup = resourceGroup_name)