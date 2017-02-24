#' @title Operations on a data science virtual machine. Available operations are "Check", "Start", "Stop", and "Delete".
#' @param context AzureSMR context.
#' @param resource.group Resource group.
#' @param name Name(s) of the DSVM(s).
#' @param operation Operations on the DSVM. Available operations are "Check", "Start", "Stop", "Delete", which check the status of, start running, stop running, and delete a DSVM, respectively.
#' @export
operateDSVM <- function(context,
                        resource.group,
                        names,
                        operation="Check") {
  # check if token is valid.

  AzureSMR::azureCheckToken(context)

  # check input arguments.

  if (missing(context)) stop("Please specify AzureSMR context.")
  if (missing(resource.group)) stop("Please specify resource group.")
  if (missing(names)) stop("Please specify DSVM name.")
  if (missing(operation)) stop("Please specify an operation on the DSVM")

  # check if input operations are available.

  if (!(operation %in% c("Check", "Start", "Stop", "Delete"))) stop("Please use an allowed operation, i.e., 'Check', 'Start', 'Stop', or 'Delete', for the DSVM.")

  # check if vm exists.

  vm_names <- AzureSMR::azureListVM(context,
                                    resourceGroup=resource.group,
                                    verbose=FALSE)

  if(!all(names %in% unlist(vm_names$name)))
    stop("Given DSVM(s) does not exist in resource group.")

  for (name in names) {
    status <- AzureSMR::azureVMStatus(azureActiveContext=context,
                                      resourceGroup=resource.group,
                                      vmName=name,
                                      verbose=FALSE)

    if (operation == "Check") {
      print(status)
    } else if (operation == "Start") {
      if(status == "Provisioning succeeded, VM running")
        return("The DSVM has already been started.")

      AzureSMR::azureStartVM(azureActiveContext=context,
                             resourceGroup=resource.group,
                             vmName=name,
                             verbose=FALSE)
    } else if (operation == "Stop") {
      if(status == "Provisioning succeeded, VM deallocated")
        return("The DSVM has already been stopped.")

      AzureSMR::azureStopVM(azureActiveContext=context,
                            resourceGroup=resource.group,
                            vmName=name,
                            verbose=FALSE)
    } else {
      AzureSMR::azureDeleteVM(azureActiveContext=context,
                              resourceGroup=resource.group,
                              vmName=name,
                              verbose=FALSE)
    }
  }
}
