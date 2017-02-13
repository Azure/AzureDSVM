#' @title Operations on a data science virtual machine. Available operations are "Check", "Start", "Stop", and "Delete".
#' @param context AzureSMR context.
#' @param resource.group Resource group.
#' @param vmname Name of the DSVM.
#' @param operation Operations on the DSVM. Available operations are "Check", "Start", "Stop", "Delete", which check the status of, start running, stop running, and delete a DSVM, respectively.
operateDSVM <- function(context,
                        resource.group,
                        vmname,
                        operation) {
  # check input arguments.

  if (missing(context)) stop("Please specify AzureSMR context.")
  if (missing(resource.group)) stop("Please specify resource group.")
  if (missing(vmname)) stop("Please specify DSVM name.")
  if (missing(operation)) stop("Please specify an operation on the DSVM")

  # check if input operations are available.

  if (!(operation %in% c("Check", "Start", "Stop", "Delete"))) stop("Please use an allowed operation, i.e., 'Check', 'Start', 'Stop', or 'Delete', for the DSVM.")

  if (operation == "Check") {
    AzureSMR::azureVMStatus(azureActiveContext=context,
                            resourceGroup=resource.group,
                            vmName=vmname,
                            verbose=FALSE)
  } else if (operation == "Start") {
    AzureSMR::azureStartVM(azureActiveContext=context,
                           resourceGroup=resource.group,
                           vmName=vmname,
                           verbose=FALSE)
  } else if (operation == "Stop") {
    AzureSMR::azureStopVM(azureActiveContext=context,
                          resourceGroup=resource.group,
                          vmName=vmname,
                          verbose=FALSE)
  } else {
    AzureSMR::azureDeleteVM(azureActiveContext=context,
                            resourceGroup=resource.group,
                            vmName=vmname,
                            verbose=FALSE)
  }
}
