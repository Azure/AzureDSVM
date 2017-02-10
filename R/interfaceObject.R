#' @title Create an R interface object that handles interaction with remote instance.
#' @param remote URL of remote instance.
#' @param user User name.
#' @param script R script with full path for execution at remote instance.
#' @param config Configuration for remote execution. Settings include computing context, data reference, etc.
#' @return An S3 R interface object.
createRInterface <- function(remote,
                             user,
                             script,
                             config){
  ri_env <- new.env(parent=globalenv())
  ri_env <- as.RInterface(azEnv)

  # initialize an R interface object.

  if(!missing(remote)) {
    ri_env$remote <- remote
  } else {
    ri_env$remote <- character(0)
  }

  if(!missing(user)) {
    ri_env$user <- user
  } else {
    ri_env$user <- character(0)
  }

  if(!missing(script)) {
    ri_env$script <- script
  } else {
    ri_env$script <- character(0)
  }

  if(!missing(config)) {
    ri_env$config <- config
  } else {
    ri_env$config <- NULL
  }
}

#' @title Set values of the R interface object.
#' @param object An S3 R interface object.
#' @param remote URL of remote instance.
#' @param user User name.
#' @param script R script with full path for execution at remote instance.
#' @param config Configuration for remote execution. Settings include computing context, data reference, etc.
#' @return The updated R interface object.
setRInterface <- function(object,
                          remote,
                          user,
                          script,
                          config) {
  if(!missing(remote)) object$remote <- remote
  if(!missing(user)) object$user <- user
  if(!missing(script) && file.exists(script))
  {
    object$script <- script
  }
  if(!missing(config)) object$config <- config

  return(object)
}

#' @title Set configuration for the R interface object.
#' @param object An S3 R interface object.
#' @param machine_list List of remote instances that execute R scripts.
#' @param dns_list DNS of the remote instances.
#' @param machine_user User name of the remote instances.
#' @param master Master node of the machine.
#' @param slaves Slave nodes of the machine.
#' @param data Reference to data used in the analytics.
#' @param context Computing context available in Microsoft R Server for running the analytics.
setConfig <- function(object,
                      machine_list,
                      dns_list,
                      machine_user,
                      master,
                      slaves,
                      data,
                      context) {
  object$config <- list(
    RI_MACHINES = ifelse(!missing(machine_list), list(machine_list), ""),
    RI_DNS      = ifelse(!missing(dns_list), list(dns_list), ""),
    RI_VMUSER   = ifelse(!missing(machine_user), list(machine_user), ""),
    RI_MASTER   = ifelse(!missing(master), list(master), ""),
    RI_SLAVES   = ifelse(!missing(slaves), list(slaves), ""),
    RI_DATA     = ifelse(!missing(data), list(data), ""),
    RI_CONTEXT  = ifelse(!missing(context), list(context), "")
  )

  return(object)
}

dumpObject <- function(object) {
  cat(
    sprintf("---------------------------------------------------------------------------"),
    sprintf("r Interface information"),
    sprintf("---------------------------------------------------------------------------"),
    sprintf("The R script to be executed:\t%s.", shQuote(object$script)),
    sprintf("The remote host:\t\t%s.", shQuote(object$remote)),
    sprintf("The login user name:\t\t%s.", shQuote(object$user)),
    sprintf("---------------------------------------------------------------------------"),
    sprintf("The configuration of the interface is:"),
    # sprintf("virtual machines: %s", ifelse(!is.na(object$config$RI_MACHINES), object$config$RI_MACHINES, "N/A")),
    sprintf("virtual machines\t\t %s", unlist(object$config$RI_MACHINES)),
    sprintf("dns list\t\t\t %s", unlist(object$config$RI_DNS)),
    sprintf("user to these machines\t\t %s", unlist(object$config$RI_VMUSER)),
    sprintf("the master node\t\t\t %s", unlist(object$config$RI_MASTER)),
    sprintf("the slave nodes\t\t\t %s", unlist(object$config$RI_SLAVES)),
    sprintf("the data source\t\t\t %s", unlist(object$config$RI_DATA)),
    sprintf("the computing context\t\t %s", unlist(object$config$RI_CONTEXT)),
    sprintf("---------------------------------------------------------------------------"),
    sprintf("# End of information session"),
    sprintf("---------------------------------------------------------------------------"),
    sep = "\n"
  )
}

#' @title Generate a new worker script which is run on the remote instance with specifications in R interface object configuration.
#' @param path Path to the script.
#' @param title Title of the script.
newScript <- function(path=".",
                      title=paste0("worker_new_", Sys.time(), ".R")) {
  notes <-
    sprintf(
      paste(
        "\n# ---------------------------------------------------------------------------",
        "# Your worker script starts from here ... ",
        "# ---------------------------------------------------------------------------\n",
        sep="\n"
      )
    )
  if (missing(path) || missing(title))
  {
    stop(sprintf("A default script named %s located at %s is created.", title, path))
  }

  cat(notes, file=file.path(path, title))
  writeLines(
    sprintf("Worker script %s is created at location %s.",
            title, ifelse(path == ".", "work directory", path))
  )
}

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
