# ---------------------------------------------------------------------------
# THIS IS A HEADER ADDED BY COMPUTE INTERFACE
# ---------------------------------------------------------------------------
CI_MACHINES <- c( "llxi001", "llxi002", "llxi003", "llxi004" )
CI_DNS <- c( "llxi001.southeastasia.cloudapp.azure.com", "llxi002.southeastasia.cloudapp.azure.com", "llxi003.southeastasia.cloudapp.azure.com", "llxi004.southeastasia.cloudapp.azure.com" )
CI_VMUSER <- c( "zhle" )
CI_MASTER <- c( "llxi001.southeastasia.cloudapp.azure.com" )
CI_SLAVES <- c( "llxi002.southeastasia.cloudapp.azure.com", "llxi003.southeastasia.cloudapp.azure.com", "llxi004.southeastasia.cloudapp.azure.com" )
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
# This is to run parallel work across nodes for clustering analysis.

# get data from remote blob.

DATA_URL <- "https://zhledata.blob.core.windows.net/mldata/creditcard.xdf"

download.file(DATA_URL,
              destfile="./data.xdf",
              mode="wb")

# download data to all nodes if it is cluster parallel.

if (rxGetComputeContext()@description == "dopar") {
  clusterCall(cl,
              download.file,
              url=DATA_URL,
              destfile="./data.xdf",
              mode="wb")
}

# make a function to do clustering of given data set.

clusterAnalysis <- function(data, numClusters) {
  
  xdf <- RxXdfData(data)
  
  # create formula.
  
  names <- rxGetVarNames(data=xdf)
  names <- names[!(names %in% c("Class", "Time"))] # the original data set is labelled so remove the label.
  formula <- as.formula(paste0("~", paste(names, collapse="+")))
  
  # to scale data.
  
  df <- rxImport(xdf,
                 varsToDrop=c("Time", "Class"))
  df <- as.data.frame(scale(df))
  
  clusters <- rxKmeans(formula,
                       df,
                       numClusters=numClusters)
  
  clusters$cluster
}

# do kmeans clustering with rxExec parallelization.

results <- rxExec(FUN=clusterAnalysis,
                  data="data.xdf",
                  numClusters=rxElemArg(c(2:5)))

save(results, file="./results.RData")
