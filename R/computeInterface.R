#' @title Create an compute interface object that handles interaction with remote instance.
#' @param remote URL of remote instance.
#' @param user User name.
#' @param script R script with full path for execution at remote instance.
#' @param config Configuration for remote execution. Settings include computing context, data reference, etc.
#' @return An S3 compute interface object.
#' @export
createComputeInterface <- function(remote,
                                   user,
                                   script,
                                   config){
  ci_env <- new.env(parent=globalenv())

  # initialize an compute interface object.

  if(!missing(remote)) {
    ci_env$remote <- remote
  } else {
    ci_env$remote <- character(0)
  }

  if(!missing(user)) {
    ci_env$user <- user
  } else {
    ci_env$user <- character(0)
  }

  if(!missing(script)) {
    ci_env$script <- script
  } else {
    ci_env$script <- character(0)
  }

  if(!missing(config)) {
    ci_env$config <- config
  } else {
    ci_env$config <- NULL
  }

  return(ci_env)
}

#' @title Set configuration for the compute interface object.
#' @param object An S3 compute interface object.
#' @param machine_list List of remote instances that execute R scripts.
#' @param dns_list DNS of the remote instances.
#' @param machine_user User name of the remote instances.
#' @param master Master node of the machine.
#' @param slaves Slave nodes of the machine.
#' @param data Reference to data used in the analytics.
#' @param context Computing context available in Microsoft R Server for running the analytics.
#' @export
setConfig <- function(object,
                      machine_list,
                      dns_list,
                      machine_user,
                      master,
                      slaves,
                      data,
                      context) {
  object$config <- list(
    CI_MACHINES = ifelse(!missing(machine_list), list(machine_list), ""),
    CI_DNS      = ifelse(!missing(dns_list), list(dns_list), ""),
    CI_VMUSER   = ifelse(!missing(machine_user), list(machine_user), ""),
    CI_MASTER   = ifelse(!missing(master), list(master), ""),
    CI_SLAVES   = ifelse(!missing(slaves), list(slaves), ""),
    CI_DATA     = ifelse(!missing(data), list(data), ""),
    CI_CONTEXT  = ifelse(!missing(context), list(context), "")
  )

  return(object)
}

#' @title Dump the interface configuration.
#' @param object The compute interface object.
#' @return No return. Print compute interface object information.
#' @export
dumpInterface <- function(object) {
  cat(
    sprintf("---------------------------------------------------------------------------"),
    sprintf("Compute interface information"),
    sprintf("---------------------------------------------------------------------------"),
    sprintf("The R script to be executed:\t%s.", shQuote(object$script)),
    sprintf("The remote host:\t\t%s.", shQuote(object$remote)),
    sprintf("The login user name:\t\t%s.", shQuote(object$user)),
    sprintf("---------------------------------------------------------------------------"),
    sprintf("The configuration of the interface is:"),
    # sprintf("virtual machines: %s", ifelse(!is.na(object$config$CI_MACHINES), object$config$CI_MACHINES, "N/A")),
    sprintf("virtual machines\t\t %s", unlist(object$config$CI_MACHINES)),
    sprintf("dns list\t\t\t %s", unlist(object$config$CI_DNS)),
    sprintf("user to these machines\t\t %s", unlist(object$config$CI_VMUSER)),
    sprintf("the master node\t\t\t %s", unlist(object$config$CI_MASTER)),
    sprintf("the slave nodes\t\t\t %s", unlist(object$config$CI_SLAVES)),
    sprintf("the data source\t\t\t %s", unlist(object$config$CI_DATA)),
    sprintf("the computing context\t\t %s", unlist(object$config$CI_CONTEXT)),
    sprintf("---------------------------------------------------------------------------"),
    sprintf("# End of information session"),
    sprintf("---------------------------------------------------------------------------"),
    sep = "\n"
  )
}

#' @title Update a worker script with compute interface object configuration.
#' @param object compute interface object.
#' @export
updateScript <- function(object) {
  if (!file.exists(object$script) || length(object$script) == 0)
  {
    stop("The script does not exist or is not specified!")
  }

  codes_body <- readLines(con=object$script)

  # Remove the header.

  if (!is.na(codes_body[2]) && codes_body[2] == "# THIS IS A HEADER ADDED BY COMPUTE INTERFACE")
  {
    head.start <- which(codes_body == "# THIS IS A HEADER ADDED BY COMPUTE INTERFACE")
    head.end <- which(codes_body == "# END OF THE HEADER ADDED BY COMPUTE INTERFACE")
    codes_body <- codes_body[-((head.start - 1):(head.end + 1))]
  }

  # Add context-specific info into header.

  codes_head <- paste(
    "# ---------------------------------------------------------------------------",
    "# THIS IS A HEADER ADDED BY COMPUTE INTERFACE",
    "# ---------------------------------------------------------------------------",
    sep="\n"
  )

  if (object$config$CI_CONTEXT == "clusterParallel")
  {
    codes_head <- paste(
      codes_head,
      paste("CI_MACHINES <-", "c(", paste(shQuote(unlist(object$config$CI_MACHINES)), collapse=", "), ")"),
      paste("CI_DNS <-", "c(", paste(shQuote(unlist(object$config$CI_DNS)), collapse=", "), ")"),
      paste("CI_VMUSER <-", "c(", paste(shQuote(unlist(object$config$CI_VMUSER)), collapse=", "), ")"),
      paste("CI_MASTER <-", "c(", paste(shQuote(unlist(object$config$CI_MASTER)), collapse=", "), ")"),
      paste("CI_SLAVES <-", "c(", paste(shQuote(unlist(object$config$CI_SLAVES)), collapse=", "), ")"),
      paste("CI_DATA <-", paste(shQuote(unlist(object$config$CI_DATA)), collapse=", ")),
      paste("CI_CONTEXT <-", paste(shQuote(unlist(object$config$CI_CONTEXT)), collapse=", ")),
      "\nlibrary(RevoScaleR)",
      "library(readr)",
      "library(doParallel)",
      "# --------- Set compute context",
      "cl <- makePSOCKcluster(names=CI_SLAVES, master=CI_MASTER, user=CI_VMUSER)",
      "registerDoParallel(cl)",
      "rxSetComputeContext(RxForeachDoPar())",
      "# --------- Load data.",
      "ciData <- ifelse(CI_DATA != '', read_csv(CI_DATA), data.frame(0))",
      "# ---------------------------------------------------------------------------",
      "# END OF THE HEADER ADDED BY COMPUTE INTERFACE",
      "# ---------------------------------------------------------------------------\n",
      sep="\n"
    )
  } else if (object$config$CI_CONTEXT == "Hadoop")
  {
    codes_head <- paste(
      codes_head,
      "This is for Hadoop.",
      "\n"
    )
  } else if (object$config$CI_CONTEXT == "Spark")
  {
    codes_head <- paste(
      codes_head,
      paste("CI_DNS <-", "c(", paste(shQuote(unlist(object$config$CI_DNS)), collapse=", "), ")"),
      paste("CI_VMUSER <-", "c(", paste(shQuote(unlist(object$config$CI_VMUSER)), collapse=", "), ")"),
      paste("CI_MASTER <-", "c(", paste(shQuote(unlist(object$config$CI_MASTER)), collapse=", "), ")"),
      paste("CI_SLAVES <-", "c(", paste(shQuote(unlist(object$config$CI_SLAVES)), collapse=", "), ")"),
      paste("CI_DATA <-", paste(shQuote(unlist(object$config$CI_DATA)), collapse=", ")),
      paste("CI_CONTEXT <-", paste(shQuote(unlist(object$config$CI_CONTEXT)), collapse=", ")),
      "\nlibrary(RevoScaleR)",
      "library(readr)",
      "# --------- Set compute context",
      "myHadoopCluster <- RxSpark(persistentRun=TRUE, idleTimeOut=600)",
      "rxSetComputeContext(myHadoopCluster)",
      "# --------- Load data.",
      "ciData <- ifelse(CI_DATA != '', read_csv(CI_DATA), data.frame(0))",
      "bigDataDirRoot <- '/share'",
      "inputDir <- file.path(bigDataDirRoot, 'riBigData')",
      "rxHadoopMakeDir(inputDir)",
      "rxHadoopCopyFromLocal('./data.csv', inputDir)",
      "hdfsFS <- RxHdfsFileSystem()",
      "riTextData <- RxTextData(file=inputDir, fileSystem=hdfsFS)",
      "# ---------------------------------------------------------------------------",
      "# END OF THE HEADER ADDED BY COMPUTE INTERFACE",
      "# ---------------------------------------------------------------------------\n",
      sep="\n"
    )
  } else if (object$config$CI_CONTEXT == "Teradata")
  {
    codes_head <- paste(
      codes_head,
      "This is for Teradata.",
      "\n"
    )
  } else if (object$config$CI_CONTEXT == "localParallel")
  {
    codes_head <- paste(
      codes_head,
      paste("CI_MACHINES <-", "c(", paste(shQuote(unlist(object$config$CI_MACHINES)), collapse=", "), ")"),
      paste("CI_DNS <-", "c(", paste(shQuote(unlist(object$config$CI_DNS)), collapse=", "), ")"),
      paste("CI_VMUSER <-", "c(", paste(shQuote(unlist(object$config$CI_VMUSER)), collapse=", "), ")"),
      paste("CI_MASTER <-", "c(", paste(shQuote(unlist(object$config$CI_MASTER)), collapse=", "), ")"),
      paste("CI_SLAVES <-", "c(", paste(shQuote(unlist(object$config$CI_SLAVES)), collapse=", "), ")"),
      paste("CI_DATA <-", paste(shQuote(unlist(object$config$CI_DATA)), collapse=", ")),
      paste("CI_CONTEXT <-", paste(shQuote(unlist(object$config$CI_CONTEXT)), collapse=", ")),
      "\nlibrary(RevoScaleR)",
      "library(doParallel)",
      "library(readr)",
      "# --------- Set compute context",
      "rxSetComputeContext(RxLocalParallel())",
      "# --------- Load data.",
      "ciData <- ifelse(CI_DATA != '', read_csv(CI_DATA), data.frame(0))",
      "# ---------------------------------------------------------------------------",
      "# END OF THE HEADER ADDED BY COMPUTE INTERFACE",
      "# ---------------------------------------------------------------------------\n",
      sep="\n"
    )
  } else {
    stop(paste("Specify a context from \"localParallel\", \"clusterParallel\",",
               "\"Hadoop\", \"Spark\", or \"Teradata\"."))
  }

  cat(codes_head, file=object$script)
  cat(codes_body, file=object$script, sep="\n", append=TRUE)
}
