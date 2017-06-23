# test remote execution on a Linux DSVM with specified computing context.

if(interactive()) library("testthat")

library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

context("Remote execution")

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

dsvm_size     <- "Standard_D1_v2"
dsvm_name     <- paste0("dsvm", 
                        paste(sample(letters, 3), collapse=""))
dsvm_password <- "AzureDSVM_test123"
dsvm_username <- "dsvmuser"

message("Remote execution is via SSH which relies on public key cryptograph.
          The test presumes that there is a private key in the user /home/.ssh/ 
          directory. A public key is derived from that private key by using SSH
          for authentication purpose.")

# pubkey key extraction.

dsvm_pubkey <- pubkey_gen()

# code to execute.

code <- "x <- seq(1, 500); y <- x * rnorm(length(x), 0, 0.1); print(y)"

temp_script <- tempfile("AzureDSVM_test_execute_", fileext=".R")
temp_script <- gsub("\\\\", "/", temp_script)
file.create(temp_script)
writeLines(code, temp_script)

context("- Remote execution on a single Linux DSVM.")

test_that("remote execution on a single Linux DSVM", {
  
  deployDSVM(asc, 
             resource.group=resourceGroup_name,
             location=location,
             hostname=dsvm_name,
             username=dsvm_username,
             size=dsvm_size,
             authen="Key",
             pubkey=dsvm_pubkey,
             mode="Sync")
  
  res <- executeScript(asc,
                       resource.group=resourceGroup_name,
                       hostname=dsvm_name,
                       remote=paste(dsvm_name,
                                    location,
                                    "cloudapp.azure.com",
                                    sep="."),
                       username=dsvm_username,
                       script=temp_script,
                       compute.context="localSequential")
  
  expect_true(res)
  
  res <- executeScript(asc,
                       resource.group=resourceGroup_name,
                       hostname=dsvm_name,
                       remote=paste(dsvm_name,
                                    location,
                                    "cloudapp.azure.com",
                                    sep="."),
                       username=dsvm_username,
                       script=temp_script,
                       compute.context="localParallel")
  
  expect_true(res)
  
  operateDSVM(asc,
              resource.group=resourceGroup_name,
              hostname=dsvm_name, 
              operation="Delete")
})

context("- Remote execution on a cluster of Linux DSVMs.")

test_that("remote execution on a cluster of Linux DSVMs", {
  message("Remote execution is via SSH which relies on public key cryptograph.
          The test presumes that there is a private key in the user /home/.ssh/ 
          directory. A public key is derived from that private key by using SSH
          for authentication purpose.")
  
  deployDSVMCluster(asc, 
                    resource.group=resourceGroup_name,
                    location=location,
                    hostname=dsvm_name,
                    username=dsvm_username,
                    size=dsvm_size,
                    authen="Key",
                    pubkey=dsvm_pubkey,
                    count=3)
  
  dsvms <- azureListVM(asc, 
                       resourceGroup=resourceGroup_name, 
                       location=location)
  
  dsvm_names <- dsvms$name
  dsvm_fqdns <- paste(dsvm_names, 
                      location,
                      "cloudapp.azure.com",
                      sep=".")
  
  res <- executeScript(asc,
                       resource.group=resourceGroup_name,
                       hostname=dsvm_names,
                       remote=dsvm_fqdns[1],
                       master=dsvm_fqdns[1],
                       slaves=dsvm_fqdns[-1],
                       username=dsvm_username,
                       script=temp_script,
                       compute.context="clusterParallel")
  
  expect_true(res)
})

azureDeleteResourceGroup(asc, resourceGroup = resourceGroup_name)
