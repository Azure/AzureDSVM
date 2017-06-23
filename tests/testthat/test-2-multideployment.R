# test deployment of DSVM.

if(interactive()) library("testthat")

library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

context("DSVM deployment (this test may last 10 to 15 minutes)")

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

context(" - Deploy a set of identical DSVMs")

dsvm_size     <- "Standard_D4_v2"
dsvm_os       <- "Ubuntu"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_authen   <- "Password"
dsvm_password <- "AzureDSVM_test123"
dsvm_username <- "dsvmuser"

test_that("Deploy 3 identical DSVMs", {
  skip_if_missing_config(settingsfile)
  
  res <- deployDSVMCluster(asc, 
                           resource.group=resourceGroup_name,
                           location=location,
                           hostname=dsvm_name,
                           username=dsvm_username,
                           authen=dsvm_authen,
                           password=dsvm_password,
                           os=dsvm_os,
                           size=dsvm_size,
                           count=3)
  
  expect_true(object=res) 
})

context(" - Deploy and form a cluster of DSVMs")

dsvm_size     <- "Standard_D1_v2"
dsvm_os       <- "Ubuntu"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_authen   <- "Key"
dsvm_username <- "dsvmuser"

test_that("Deploy and form a cluster of DSVMs", {
  skip_if_missing_config(settingsfile)
  
  dsvm_pubkey <- pubkey_gen()
  
  res <- deployDSVMCluster(asc, 
                           resource.group=resourceGroup_name,
                           location=location,
                           hostname=dsvm_name,
                           username=dsvm_username,
                           authen=dsvm_authen,
                           pubkey=dsvm_pubkey,
                           os=dsvm_os,
                           size=dsvm_size,
                           count=3)
  expect_true(res)
})

context(" - Deploy a set of heterogeneous DSVMs")

dsvm_size     <- c("Standard_D4_v2", "Basic_A1", "Standard_D12_v2")
dsvm_os       <- c("Ubuntu", "Windows", "RServer")
dsvm_name     <- paste0("dsvm", paste(sample(letters, 3)))
dsvm_authen   <- rep("Password", 3)
dsvm_password <- rep("AzureDSVM_test123", 3)
dsvm_username <- paste0("user", paste(sample(letters, 3)))

test_that("Deploy 3 different DSVMs", {
  skip_if_missing_config(settingsfile)
  
  res <- deployDSVMCluster(asc, 
                           resource.group=resourceGroup_name,
                           location=location,
                           hostname=dsvm_name,
                           username=dsvm_username,
                           size=dsvm_size,
                           os=dsvm_os,
                           authen="Password",
                           password=dsvm_password)
  
  expect_true(object=res)
})

context(" - delete resource group")
test_that("Can delete resource group", {
  skip_if_missing_config(settingsfile)
  
  expect_message({
    res <- azureDeleteResourceGroup(asc, resourceGroup = resourceGroup_name)
  }, "Delete Request Submitted"
  )
})