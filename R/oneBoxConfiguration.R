#' One box configuration for VM with Microsoft R Server (>= 9.1)
#' 
#' @inheritParams deployDSVM
#' 
#' @param password The password used for the one-box configuration. Default 
#' username for the configuration is 'admin'.
#' 
#' @description The function will perform one-box configuration for a MRS VM.
#' A successful configuration will allow one to operationalize MRS analytics
#' on a VM with mrsdeploy functions.
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
  
  if (missing(resource.group)) 
    stop("Please specify a resource group.")
  assert_that(AzureSMR:::is_resource_group(resource.group))
  
  if(missing(location))
    stop("Please specify a data centre location.")
  assert_that(AzureSMR:::is_location(location))
  
  if(missing(hostname))
    stop("Please specify a virtual machine hostname.")
  assert_that(AzureSMR:::is_vm_name(hostname))
  
  if(missing(username))
    stop("Please specify a virtual machine user name.")
  assert_that(AzureSMR:::is_admin_user(username))
  
  if(missing(password))
    stop("Please specify a password.")
  assertion <- 
    nchar(x) <= 16 &&
    nchar(x) >= 8 && 
    grepl("[A-Z]", x) && 
    grepl("[a-z]", x) && 
    grepl("[0-9]", x)
  if(!assertion) stop("Please specify a valid password - length between 8 to
                      16, containing at leaset one upper case letter, one
                      lower case letter, one number, and one special character.")
  
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