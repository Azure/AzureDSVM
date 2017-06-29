#' Add DSVM extension.
#' 
#' @inheritParams deployDSVM
#' 
#' @param fileurl The URL of script to execute.
#' 
#' @param command Command to run.
#' 
#' @export
addExtensionDSVM <- function(context,
                             location,
                             resource.group,
                             hostname,
                             fileurl,
                             command) {
  
  assert_that(is.azureActiveContext(context))
  
  AzureSMR::azureCheckToken(context)
  
  if (missing(resource.group)) {
    stop("Please specify a resource group.")
    assert_that(AzureSMR:::is_resource_group(resource.group))
  }
  
  if(missing(hostname)) {
    stop("Please specify a virtual machine hostname.")
    assert_that(AzureSMR:::is_vm_name(hostname))
  }
  
  api_version    <- "2016-04-30-preview"
  extension_name <- paste0(hostname, "extension")
  
  # based on the VM, use different types. Windows will be CustomScriptExtension.
  # Currently only Linux is supported. 
  type <- "CustomScriptForLinux"
  
  # this is the default name of storage account on the DSVM.
  storage_account <- paste0(hostname, "sa")
  existing_storage_accounts <- azureListSA(context, resource.group)
  if(!storage_account %in% existing_storage_accounts) 
    stop("No default storage account associated with the DSVM. You need to 
         manually create one before trying again.")
  
  storage_key <- azureSAGetKey(context,
                               storage_account,
                               resource.group)
  
  url <- paste0("https://management.azure.com/subscriptions/",
                context$subscriptionID,
                "/resourceGroups/",
                resource.group,
                "/providers/Microsoft.Compute/virtualMachines/",
                hostname,
                "/extensions/",
                extension_name,
                "?api-version=",
                api_version)
  
  body <- sprintf('{"location":"%s","properties":{"publisher":"Microsoft.OSTCExtensions","type":"%s","typeHandlerVersion":"1.5","autoUpgradeMinorVersion":true,"forceUpdateTag":"RerunExtension","settings":{"fileUris":["%s"],"commandToExecute":"%s"},"protectedSettings":{"StorageAccountName":"%s","StorageaccountKey":"{%s}"}}}',
                 location,
                 type,
                 fileurl,
                 command,
                 storage_account,
                 storage_key)
                
  r <- AzureSMR:::call_azure_sm(context,
                                uri=url,
                                body=body,
                                verb="PUT")
  
  AzureSMR:::stopWithAzureError(r)
  
  rl <- content(r, "text", encoding="UTF-8")
  
  provision_state <- ""
  
  cat("Creating extensions")
  while(provision_state != "succeeded") {
    cat(".")
    info <- checkExtensionDSVM(context,
                               resource.group,
                               hostname)
    provision_state <- info$properties$provisioningState
    
    Sys.sleep(2)
  }
  
  return(TRUE)
}

#' Get information from a VM extension.

#' @inheritParams addExtensionDSVM
#' 
#' @export
#' 
checkExtensionDSVM <- function(context,
                               resource.group,
                               hostname) {
  assert_that(is.azureActiveContext(context))
  
  AzureSMR::azureCheckToken(context)
  
  if (missing(resource.group)) {
    stop("Please specify a resource group.")
    assert_that(AzureSMR:::is_resource_group(resource.group))
  }
  
  if(missing(hostname)) {
    stop("Please specify a virtual machine hostname.")
    assert_that(AzureSMR:::is_vm_name(hostname))
  }
  
  api_version    <- "2016-04-30-preview"
  extension_name <- paste0(hostname, "extension")
  
  url <- paste0("https://management.azure.com/subscriptions/",
                context$subscriptionID,
                "/resourceGroups/",
                resource.group, 
                "/providers/Microsoft.Compute/virtualMachines/",
                hostname, 
                "/extensions/",
                extension_name,
                "?api-version=",
                api_version)
  
  r <- AzureSMR:::call_azure_sm(context,
                                uri=url,
                                verb="GET")
  
  AzureSMR:::stopWithAzureError(r)
  
  rl <- content(r, "text", encoding="UTF-8")
  
  df <- fromJSON(rl)
  
  return(df)
}
