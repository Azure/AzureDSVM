#' One box configuration for VM with Microsoft R Server (>= 9.1)
#' 
#' @inheritParams deployDSVM
#' 
#' @param password The password used for the one-box configuration. Default 
#' username for the configuration is 'admin'.
#' 
#' @references More can be found at 
#' https://msdn.microsoft.com/en-us/microsoft-r/operationalize/configuration-initial
#' 
#' @export

mrsOneBoxConfiguration <- function(context, 
                                   resource.group,
                                   hostname,
                                   location,
                                   username,
                                   password) {
  assert_that(AzureSMR:::is_vm_name(hostname))
  
  status <- operateDSVM(context=context,
                        resource.group=resource.group,
                        hostname=hostname, 
                        operation="Check")
  
  if (status != "Provisioning succeeded, VM running") {
    operateDSVM(context=context, 
                resource.group=resource.group,
                hostname=hostname, 
                operation="Start")
  } else {
    # do the one-box configuration.
    # TODO: check the OS to be Microsoft R Server.
    
    string <- paste0("sudo ",
                     "/usr/bin/dotnet ",
                     "/usr/lib64/microsoft-r/rserver/o16n/9.1.0/",
                     "Microsoft.RServer.Utils.AdminUtil/",
                     "Microsoft.RServer.Utils.AdminUtil.dll",
                     " -silentoneboxinstall", 
                     " ",
                     password)
    
    dns <- paste(hostname,
                 location,
                 "cloudapp.azure.com",
                 sep=".")
    
    option <- "-q -o StrictHostKeyChecking=no"
    
    exe <- system(sprintf("ssh %s -l %s %s %s",
                          option,
                          username,
                          dns,
                          string), 
                  show.output.on.console=FALSE)
    
    if (is.null(attributes(exe)))
    {
      writeLines("Command has been successfully executed.")
    } else {
      writeLines("Something must be wrong....... See warning message.")
    }
  }
  
  return(TRUE)
}