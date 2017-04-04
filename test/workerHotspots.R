# ---------------------------------------------------------------------------
# THIS IS A HEADER ADDED BY COMPUTE INTERFACE
# ---------------------------------------------------------------------------
CI_MACHINES <- c( "wlke001", "wlke002", "wlke003", "wlke004", "wlke005", "wlke006" )
CI_DNS <- c( "wlke001.southeastasia.cloudapp.azure.com", "wlke002.southeastasia.cloudapp.azure.com", "wlke003.southeastasia.cloudapp.azure.com", "wlke004.southeastasia.cloudapp.azure.com", "wlke005.southeastasia.cloudapp.azure.com", "wlke006.southeastasia.cloudapp.azure.com" )
CI_VMUSER <- c( "zhle" )
CI_MASTER <- c( "wlke001.southeastasia.cloudapp.azure.com" )
CI_SLAVES <- c( "wlke002.southeastasia.cloudapp.azure.com", "wlke003.southeastasia.cloudapp.azure.com", "wlke004.southeastasia.cloudapp.azure.com", "wlke005.southeastasia.cloudapp.azure.com", "wlke006.southeastasia.cloudapp.azure.com" )
CI_DATA <- ""
CI_CONTEXT <- "clusterParallel"

library(RevoScaleR)
# library(readr)
library(doParallel)
# --------- Set compute context
cl <- makePSOCKcluster(names=CI_SLAVES, master=CI_MASTER, user=CI_VMUSER)
registerDoParallel(cl)
rxSetComputeContext(RxForeachDoPar())
# --------- Load data.
# ciData <- ifelse(CI_DATA != '', read_csv(CI_DATA), data.frame(0))
# ---------------------------------------------------------------------------
# END OF THE HEADER ADDED BY COMPUTE INTERFACE
# ---------------------------------------------------------------------------

# source the script to load functions used for the analysis.

source("workerHotspotsSetup.R")
source("workerHotspotsFuncs.R")
source("workerHotspotsTrain.R")
source("workerHotspotsTest.R")
source("workerHotspotsProcess.R")

# source("./test/workerHotspotsSetup.R")
# source("./test/workerHotspotsFuncs.R")
# source("./test/workerHotspotsTrain.R")
# source("./test/workerHotspotsTest.R")
# source("./test/workerHotspotsProcess.R")

# initial parameter definition.

number_of_clust <- 2:20 
train_ratio     <- 0.7

# lib  <- "home/zhle/R/x86_64-pc-linux-gnu-library/3.3" # install packages on a personal lib. Note this merely works for Linux machine.
lib  <- "~/lib" # install packages on a personal lib. Note this merely works for Linux machine.
pkgs <- c("dplyr", "stringr", "stringi", "magrittr", "readr", "rattle", "ggplot2", "DMwR")

data_url <- "https://zhledata.blob.core.windows.net/mldata/creditcard.xdf"

download.file(data_url,
              destfile="./data.xdf",
              mode="wb")

# install packages on master node.

installPkgs(list_of_pkgs=pkgs, lib=lib)

if (rxGetComputeContext()@description == "dopar") {
  # download data to nodes.
  
  clusterCall(cl,
              download.file,
              url=data_url,
              destfile="./data.xdf",
              mode="wb")
  
  # install packages on nodes.
  
  clusterCall(cl,
              installPkgs,
              list_of_pkgs=pkgs,
              lib=lib)
} 

# load packages.

sapply(pkgs, require, character.only=TRUE)

# Hotspots analysis.

time_1 <- Sys.time()

# eval <- hotSpotsProcess(data=RxXdfData("./data.xdf"),
#                         number.of.clust=number_of_clust)

eval <- rxExec(FUN=hotSpotsProcess,
               data="./data.xdf",
               timesToRun=2)

time_2 <- Sys.time()

cat(eval)

# save results.

save(eval, file="./results.RData")
