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

context(" - Deploy a CentOS DSVM")

dsvm_size     <- "Standard_D4_v2"
dsvm_os       <- "CentOS"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_password <- "AzureDSVM_test123"
dsvm_username <- "dsvmuser"

test_that("Get VM sizes", {
  skip_if_missing_config(settingsfile)
  
  res <- getVMSizes(asc, 
                    location=location)
  
  expect_is(res, class="data.frame")
  expect_equal(names(res), c("VM Size",
                             "Number of cores",
                             "Disk size (GB)",
                             "RAM size (GB)",
                             "Max number of disks"))
})

test_that("Deploy a CentOS DSVM", {
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
  
  expect_equal(object=res, expected=paste(dsvm_name, 
                                          location, 
                                          "cloudapp.azure.com", 
                                          sep="."))
})

context(" - Deploy a Windows DSVM")

dsvm_size     <- "Standard_D12_v2"
dsvm_os       <- "Windows"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_password <- "AzureDSVM_test123"
dsvm_username <- "dsvmuser"

test_that("Deploy a Windows DSVM", {
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
  
  expect_equal(object=res, expected=paste(dsvm_name, 
                                          location, 
                                          "cloudapp.azure.com", 
                                          sep="."))
})

context(" - Deploy a Microsoft R Server VM")

dsvm_size     <- "Standard_D4_v2"
dsvm_os       <- "RServer"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_password <- "AzureDSVM_test123"
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
                    authen="Password",
                    pubkey="",
                    password=dsvm_password,
                    mode="Sync")
  
  expect_equal(object=res, expected=paste(dsvm_name, 
                                          location, 
                                          "cloudapp.azure.com", 
                                          sep="."))
})

context(" - delete resource group")
test_that("Can delete resource group", {
  skip_if_missing_config(settingsfile)
  
  expect_message({
    res <- azureDeleteResourceGroup(asc, resourceGroup = resourceGroup_name)
  }, "Delete Request Submitted"
  )
})