#' @title Update a worker script with R interface object configuration.
#' @param object R interface object.
updateScript <- function(object) {
  if (!file.exists(object$script) || length(object$script) == 0)
  {
    stop(paste("The script does not exist or is not specified!",
               "Consider create a new one using riNewScript."))
  }

  codes.body <- readLines(con=object$script)

  # Remove the header.

  if (codes.body[2] == "# THIS IS A HEADER ADDED BY R INTERFACE")
  {
    head.start <- which(codes.body == "# THIS IS A HEADER ADDED BY R INTERFACE")
    head.end <- which(codes.body == "# END OF THE HEADER ADDED BY R INTERFACE")
    codes.body <- codes.body[-((head.start - 1):(head.end + 1))]
  }

  # Add context-specific info into header.

  codes.head <- paste(
    "# ---------------------------------------------------------------------------",
    "# THIS IS A HEADER ADDED BY R INTERFACE",
    "# ---------------------------------------------------------------------------",
    sep="\n"
  )

  if (object$config$RI_CONTEXT == "clusterParallel")
  {
    codes.head <- paste(
      codes.head,
      paste("RI_MACHINES <-", "c(", paste(shQuote(unlist(object$config$RI_MACHINES)), collapse=", "), ")"),
      paste("RI_DNS <-", "c(", paste(shQuote(unlist(object$config$RI_DNS)), collapse=", "), ")"),
      paste("RI_VMUSER <-", "c(", paste(shQuote(unlist(object$config$RI_VMUSER)), collapse=", "), ")"),
      paste("RI_MASTER <-", "c(", paste(shQuote(unlist(object$config$RI_MASTER)), collapse=", "), ")"),
      paste("RI_SLAVES <-", "c(", paste(shQuote(unlist(object$config$RI_SLAVES)), collapse=", "), ")"),
      paste("RI_DATA <-", paste(shQuote(unlist(object$config$RI_DATA)), collapse=", ")),
      paste("RI_CONTEXT <-", paste(shQuote(unlist(object$config$RI_CONTEXT)), collapse=", ")),
      "\nlibrary(RevoScaleR)",
      "# --------- Set compute context",
      "cl <- makePSOCKcluster(names=RI_SLAVES, master=RI_MASTER, user=RI_VMUSER)",
      "registerDoParallel(cl)",
      "rxSetComputeContext(RxForeachDoPar())",
      "# --------- Load data.",
      "download.file(url=RI_DATA, destfile='./data.csv')",
      "riData <- read.csv('./data.csv', header=T, sep=',', stringAsFactor=F)",
      "# ---------------------------------------------------------------------------",
      "# END OF THE HEADER ADDED BY R INTERFACE",
      "# ---------------------------------------------------------------------------\n",
      sep="\n"
    )
  } else if (object$config$RI_CONTEXT == "Hadoop")
  {
    codes.head <- paste(
      codes.head,
      "This is for Hadoop.",
      "\n"
    )
  } else if (object$config$RI_CONTEXT == "Spark")
  {
    codes.head <- paste(
      codes.head,
      paste("RI_DNS <-", "c(", paste(shQuote(unlist(object$config$RI_DNS)), collapse=", "), ")"),
      paste("RI_VMUSER <-", "c(", paste(shQuote(unlist(object$config$RI_VMUSER)), collapse=", "), ")"),
      paste("RI_MASTER <-", "c(", paste(shQuote(unlist(object$config$RI_MASTER)), collapse=", "), ")"),
      paste("RI_SLAVES <-", "c(", paste(shQuote(unlist(object$config$RI_SLAVES)), collapse=", "), ")"),
      paste("RI_DATA <-", paste(shQuote(unlist(object$config$RI_DATA)), collapse=", ")),
      paste("RI_CONTEXT <-", paste(shQuote(unlist(object$config$RI_CONTEXT)), collapse=", ")),
      "\nlibrary(RevoScaleR)",
      "# --------- Set compute context",
      "myHadoopCluster <- RxSpark(persistentRun=TRUE, idleTimeOut=600)",
      "rxSetComputeContext(myHadoopCluster)",
      "# --------- Load data.",
      "download.file(url=RI_DATA, destfile='./data.csv')",
      "riData <- read.csv('./data.csv', header=T, sep=',', stringAsFactor=F)",
      "bigDataDirRoot <- '/share'",
      "inputDir <- file.path(bigDataDirRoot, 'riBigData')",
      "rxHadoopMakeDir(inputDir)",
      "rxHadoopCopyFromLocal('./data.csv', inputDir)",
      "hdfsFS <- RxHdfsFileSystem()",
      "riTextData <- RxTextData(file=inputDir, fileSystem=hdfsFS)",
      "# ---------------------------------------------------------------------------",
      "# END OF THE HEADER ADDED BY R INTERFACE",
      "# ---------------------------------------------------------------------------\n",
      sep="\n"
    )
  } else if (object$config$RI_CONTEXT == "Teradata")
  {
    codes.head <- paste(
      codes.head,
      "This is for Teradata.",
      "\n"
    )
  } else if (object$config$RI_CONTEXT == "localParallel")
  {
    codes.head <- paste(
      codes.head,
      paste("RI_MACHINES <-", "c(", paste(shQuote(unlist(object$config$RI_MACHINES)), collapse=", "), ")"),
      paste("RI_DNS <-", "c(", paste(shQuote(unlist(object$config$RI_DNS)), collapse=", "), ")"),
      paste("RI_VMUSER <-", "c(", paste(shQuote(unlist(object$config$RI_VMUSER)), collapse=", "), ")"),
      paste("RI_MASTER <-", "c(", paste(shQuote(unlist(object$config$RI_MASTER)), collapse=", "), ")"),
      paste("RI_SLAVES <-", "c(", paste(shQuote(unlist(object$config$RI_SLAVES)), collapse=", "), ")"),
      paste("RI_DATA <-", paste(shQuote(unlist(object$config$RI_DATA)), collapse=", ")),
      paste("RI_CONTEXT <-", paste(shQuote(unlist(object$config$RI_CONTEXT)), collapse=", ")),
      "\nlibrary(RevoScaleR)",
      "library(doParallel)",
      "# --------- Set compute context",
      "rxSetComputeContext(RxLocalParallel())",
      "# --------- Load data.",
      "download.file(url=RI_DATA, destfile='./data.csv')",
      "riData <- read.csv('./data.csv', header=T, sep=',', stringAsFactor=F)",
      "# ---------------------------------------------------------------------------",
      "# END OF THE HEADER ADDED BY R INTERFACE",
      "# ---------------------------------------------------------------------------\n",
      sep="\n"
    )
  } else {
    stop(paste("Specify a context from \"localParallel\", \"clusterParallel\",",
               "\"Hadoop\", \"Spark\", or \"Teradata\"."))
  }

  cat(codes.head, file=object$script)
  cat(codes.body, file=object$script, sep="\n", append=TRUE)
}
