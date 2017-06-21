#' Operations on a data science virtual machine. 
#' Available operations are "Check", "Start", "Stop", and "Delete".
#' 
#' @inheritParams deployDSVM
#' 
#' @param operation Operations on the DSVM. Available operations are 
#' "Check", "Start", "Stop", "Delete", which check the status of, 
#' start running, stop running, and delete a DSVM, respectively.
#' 
#' @export
operateDSVM <- function(context,
                        resource.group,
                        hostname,
                        operation="Check") {
  # check if token is valid.

  AzureSMR::azureCheckToken(context)

  # check input arguments.

  if (missing(context)) stop("Please specify AzureSMR context.")
  assert_that(AzureSMR::is.azureActiveContext(context))
  
  if (missing(resource.group)) stop("Please specify resource group.")
  assert_that(AzureSMR:::is_resource_group(resource.group))
  
  if (missing(hostname)) stop("Please specify DSVM name.")
  assert_that(AzureSMR:::is_vm_name(hostname))
  
  if (missing(operation)) stop("Please specify an operation on the DSVM")
  operation <- match.arg(operation, c("Check", "Start", "Stop", "Delete"))

  # check if vm exists.

  vm_names <- AzureSMR::azureListVM(context,
                                    resourceGroup=resource.group,
                                    verbose=FALSE)

  if(!all(hostname %in% unlist(vm_names$name)))
    stop("Given DSVM(s) does not exist in resource group.")

  for (name in hostname) {
    status <- AzureSMR::azureVMStatus(azureActiveContext=context,
                                      resourceGroup=resource.group,
                                      vmName=name,
                                      verbose=FALSE)

    if (operation == "Check") {
      print(status)
    } else if (operation == "Start") {
      if(status == "Provisioning succeeded, VM running") {
        message("The DSVM has already been started.")
      } else {
        AzureSMR::azureStartVM(azureActiveContext=context,
                               resourceGroup=resource.group,
                               vmName=name,
                               verbose=FALSE)
      }
    } else if (operation == "Stop") {
      if(status == "Provisioning succeeded, VM deallocated") {
        message("The DSVM has already been stopped.")
      } else {
        AzureSMR::azureStopVM(azureActiveContext=context,
                              resourceGroup=resource.group,
                              vmName=name,
                              verbose=FALSE)
      }
    } else {
      AzureSMR::azureDeleteVM(azureActiveContext=context,
                              resourceGroup=resource.group,
                              vmName=name,
                              verbose=FALSE)
    }
  }
  
  return(TRUE)
}
