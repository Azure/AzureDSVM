#' @title Create new Linux Data Science Virtual Machine (DSVM).
#'
#' @param context Authentication context of AzureSMR encapsulating the
#'   TID, CID, and key obtained from Azure Actrive Directory.
#' @param resource.group The Azure resource group where the DSVM is
#'   created.
#' @param location Location of the data centre to host the DSVM.
#' @param vmname Name of the DSVM.  Lowercase characters or numbers
#'   only. Special characters are not permitted.
#' @param vmusername User name of the DSVM. It should be different
#'   from `vmname`.
#' @param vmsize Size of the DSVM. The default is
#'   "Standard_D1_v2". All available sizes can be obtained by function
#'   `getVMSizes`.
#' @param vmauthen Either "Key" or "Pass", meaning public-key based or
#'   password based authentication, respectively.
#' @param vmpubkey Public key for the DSVM. Only applicable for
#'   public-key based authentication.
#' @param mode Mode of virtual machine deployment. Default is "Sync".
newLinuxDSVM <- function(context,
                         resource.group,
                         location,
                         vmname,
                         vmusername,
                         vmsize="Standard_D3_v2",
                         vmos,
                         vmauthen="Key",
                         vmpubkey,
                         vmpassword="",
                         vmdns=paste0(vmname, "_dns"),
                         mode="Sync")
{
  # Preconditions.

  if(missing(context))
    stop("Please specify a context (contains TID, CID, KEY).")

  if(missing(resource.group))
    stop("Please specify an Azure resouce group.")

  if(missing(location))
    stop("Please specify a data centre location.")

  if(missing(vmname))
    stop("Please specify a virtual machine name.")

  if(missing(vmauthen)) # Never missing since has a default value of Key.
    stop(paste("Please specify authentication method:",
               "'Key' for public key or 'Pass' for password."))

  if(!(vmsize %in% getVMSizes()$Sizes))
    stop("Unknown vmsize - see getVMSizes() for allowed options.")

  # Incorrect naming of a vm may lead to an unsuccessful deployment of
  # the DSVM - normally it returns a 400 error from REST call. Check
  # the name here to ensure it is valid.

  #  if (REGEXP OF lowercase AND digits to MATCH vmname)
  #    stop("Invalid vmname - only lowercase and digits permitted.")

  # Specify the JSON for the parameters and template of a Linux Data
  # Science Virtual Machine.


  # Template and parameter JSON files are put in inst/etc. They are loaded when deploying an Azure instance.

  para_path <- system.file("etc", "parameter.json", package="AzureDSR")

  if(vmos == "Windows") {

    # Windows DSVM does not support public key based authentication.

    if(vmauthent != "Pass") {
      stop("Please use password based authentication, i.e., 'Pass'.")
    }

    temp_path <- system.file("etc", "windows.json", package="AzureDSR")
  } else if(vmos == "Linux") {
    if(vmauthen == "Key") {
      temp_path <- system.file("etc", "linux_key.json", package="AzureDSR")
    } else if(vmauthen == "Password") {
      temp_path <- system.file("etc", "linux.json", package="AzureDSR")
    } else {
      stop("Please specific a valid authentication method, i.e., either 'Key' for public key based or 'Password' for password based, for Linux OS based DSVM")
    }
  } else {
    stop("Please specify a valid OS type, i.e., either 'Windows' or 'Linux'.")
  }

  # Update the parameter JSON with the virtual machine name.

  param <-
    readLines(para_path) %>%
    gsub("<LOCATION>", location, .) %>%
    gsub("<DEFAULT>", vmname, .) %>%
    gsub("<USERNAME>", vmusername, .) %>%
    gsub("<VMSIZE>", vmsize, .) %>%
    gsub("<PWD>", vmpassword, .) %>%
    gsub("<PUBKEY>", vmpubkey, .) %>%
    gsub("<DNS_LABEL>", vmdns, .)

  # Update the template JSON with the appropriate parameters.

  templ <-
    readLines(temp_path) %>%
    paste0(collapse="")

  dname <- paste0(vmname, "_dpl")

  AzureSMR::azureDeployTemplate(context,
                                deplname=dname,
                                templateJSON=templ,
                                paramJSON=param,
                                resourceGroup=resource.group,
                                mode=mode)

  fqdn <- paste0(vmname, ".", location, ".cloudapp.azure.com")

  if (tolower(mode) == "sync")
    attr(fqdn, "ip") <-
      system(paste("dig", fqdn, "+short"), intern=TRUE) # Get from the VM meta data?

  return(fqdn)
}
