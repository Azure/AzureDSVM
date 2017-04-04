# source the script to load functions used for the analysis.

source("workerHotspotsSetup.R")
source("workerHotspotsFuncs.R")
source("workerHotspotsTrain.R")
source("workerHotspotsTest.R")
source("workerHotspotsProcess.R")

# initial parameter definition.

number_of_clust <- 2:10 
train_ratio     <- 0.7

lib  <- "~/lib" # install packages on a personal lib. Note this merely works for Linux machine.
pkgs <- c("dplyr", "stringr", "stringi", "magrittr", "readr", "rattle", "ggplot2", "DMwR")

data_url <- "https://zhledata.blob.core.windows.net/mldata/creditcard.xdf"

download.file(data_url,
              destfile="./data.xdf",
              mode="wb")

# install and load packages.

installPkgs(list_of_pkgs=pkgs, lib=lib)

sapply(pkgs, require, character.only=TRUE)

# Hotspots analysis.

eval <- hotSpotsProcess(data=RxXdfData("./data.xdf"),
                        number.of.clust=number_of_clust,
                        train.ratio=train_ratio)

# save results.

save(eval, file="./results.RData")
