# test data consumption and cost of DSVM.

if(interactive()) library("testthat")

library(AzureSMR)

settingsfile <- getOption("AzureSMR.config")
config <- read.AzureSMR.config()

timestamp <- format(Sys.time(), format="%y%m%d%H%M")

context("Data consumption and cost")

asc <- createAzureContext()

with(config,
     setAzureContext(asc, tenantID=tenantID, clientID=clientID, authKey=authKey)
)
azureAuthenticate(asc)

message("The test requires user provide information of Azure subscription
        offer ID, locale, region, currency, etc. Otherwise the test fails.")

context("- Data consumption of Azure susbscription.")

test_that("retrieve data consumption of Azure subscription", {
  
  time_end   <- paste0(as.Date(Sys.Date()), "00:00:00")
  time_start <- paste0(as.Date(Sys.Date() - 365), "00:00:00")
  
  res <- dataConsumptionDSVM(context=asc,
                             timeStart=time_start,
                             timeEnd=time_end,
                             granularity="Daily")
  
  expect_is(res, class="data.frame")
  expect_identical(object=names(res), expected=c("usageStartTime", 
                                                 "usageEndTime",
                                                 "meterName", 
                                                 "meterCategory",
                                                 "meterSubCategory",
                                                 "unit",
                                                 "meterId",
                                                 "quantity",
                                                 "meterRegion"))
  
})

context("- Cost of Azure subscription.")

test_that("Cost of Azure subscription", {
  skip_if_missing_config(settingsfile)
  
  time_end   <- paste0(as.Date(Sys.Date()), "00:00:00")
  time_start <- paste0(as.Date(Sys.Date() - 365), "00:00:00")
  
  res <- costDSVM(context=asc,
                  timeStart=time_start,
                  timeEnd=time_end,
                  granularity="Daily",
                  currency=config$CURRENCY,
                  locale=config$LOCALE,
                  offerId=config$OFFER,
                  region=config$REGION)
  
  expect_is(res, class="data.frame")
  expect_identical(object=names(res), expected=c("meterName",
                                                 "meterCategory",
                                                 "meterSubCategory",
                                                 "quantity",
                                                 "unit",
                                                 "meterRate",
                                                 "cost"))
})

context("- Cost of a single DSVM")

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

test_that("cost of a single DSVM", {
  deployDSVM(asc, 
             resource.group=resourceGroup_name,
             location=location,
             hostname=dsvm_name,
             username=dsvm_username,
             size=dsvm_size,
             authen="Password",
             password=dsvm_password,
             mode="Sync")
  
  Sys.sleep(60)
  
  time_end   <- Sys.time()
  time_start <- Sys.time() - 3600
  
  res <- costDSVM(context=asc,
                  hostname=dsvm_name,
                  timeStart=time_start,
                  timeEnd=time_end,
                  granularity="Hourly",
                  currency=config$CURRENCY,
                  locale=config$LOCALE,
                  offerId=config$OFFER,
                  region=config$REGION)
  
  expect_is(res, class="data.frame")
  expect_identical(object=names(res), expected=c("meterName",
                                                 "meterCategory",
                                                 "meterSubCategory",
                                                 "quantity",
                                                 "unit",
                                                 "meterRate",
                                                 "cost"))
})

azureDeleteResourceGroup(asc, resourceGroup = resourceGroup_name)