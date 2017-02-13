#' @title Dump out the object configuration.
#' @param object The R interface object.
#' @return No return. Print R interface object information.
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
