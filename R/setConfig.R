#' @title Set configuration for the R interface object.
#' @param object An S3 R interface object.
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
