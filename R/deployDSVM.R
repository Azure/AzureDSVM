#' @title Deploy a new Data Science Virtual Machine (DSVM).
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
#' @param vmos Operating system of DSVM. Permitted values are "Linux" and "Windows" for Linux based and Windows based operating systems, respectively.
#' @param vmauthen Either "Key" or "Password", meaning public-key based or
#'   password based authentication, respectively. Note Windows DSVM by default uses password based authentication and this argument can be left unset.
#' @param vmpubkey Public key for the DSVM. Only applicable for
#'   public-key based authentication of Linux based DSVM.
#' @param vmpassword Pass word for the DSVM.
#' @param vmdns DNS label for the VM address. The URL for accessing the deployed DSVM will be "<dns_label>.<location>.cloudapp.azure.com
#' @param mode Mode of virtual machine deployment. Default is "Sync".
deployDSVM <- function(context,
                       resource.group,
                       location,
                       vmname,
                       vmusername,
                       vmsize="Standard_D3_v2",
                       vmos,
                       vmauthen="",
                       vmpubkey="",
                       vmpassword="",
                       vmdns=paste0(vmname, "dns"),
                       mode="Sync")
{
  # check if required arguments are present.

  if(missing(context))
    stop("Please specify a context (contains TID, CID, KEY).")

  if(missing(resource.group))
    stop("Please specify an Azure resouce group.")

  if(missing(location))
    stop("Please specify a data centre location.")

  if(missing(vmname))
    stop("Please specify a virtual machine name.")

  if(missing(vmusername))
    stop("Please specify a virtual machine user name.")

  if(missing(vmos))
    stop("Please specify a virtual machine OS.")

  if(vmos == "Linux" && missing(vmauthen))
    stop("Please specify an authentication method for Linux DSVM.")

  if(vmos == "Windows" && missing(vmpassword))
    stop("Please specify a password for Windows DSVM.")

  if(vmauthen == "Key" && missing(vmpubkey))
    stop("Please specify a public key.")

  if(vmauthen == "Password" && missing(vmpassword))
    stop("Please specify a password.")

  # Other preconditions.

  # check if AzureSMR context is valid.

  if(!is.azureActiveContext(context))
    stop("Please use a valid AzureSMR context.")

  # check if resource group exists.

  rg_exist <-
    context %>%
    azureListRG() %>%
    filter(name == RG) %>%
    select(name, location) %>%
    nrow() %>%
    equals(0) %>%
    not()

  if(!rg_exist)
    stop("The specified resource group does not exist in the current region.")

  # check if location is available.

  # if(location %in% c("location_code_1", ...))

  # check if vm

  if(!(vmsize %in% getVMSizes()$Sizes))
    stop("Unknown vmsize - see getVMSizes() for allowed options.")

  # Incorrect naming of a vm may lead to an unsuccessful deployment of
  # the DSVM - normally it returns a 400 error from REST call. Check
  # the name here to ensure it is valid.

  if(length(vmname) > 15)
    stop("Name of virtual machine is too long.")

  if(grepl("[[:upper:]]|[[:punct:]]", vmname))
    stop("Name of virtual machine is not valid - only lowercase and digits permitted.")

  # check if password is valid.

  # if(!grepl("^(?=.*[[A-Za-z]])(?=.*\\d)(?=.*[[$@$!%*#?&]])[[A-Za-z\\d$@$!%*#?&]]{8,}$", vmpassword))
  #   stop("Password not valid - minimum 8 characters with at least one digit and one special character.")

  # Load template and parameter JSON files for deployment

  if(vmos == "Windows") {
    temp_path <- system.file("etc", "template_windows.json", package="AzureDSR")
    para_path <- system.file("etc", "parameter_windows.json", package="AzureDSR")
  } else if(vmos == "Linux") {
    if(vmauthen == "Key") {
      temp_path <- system.file("etc", "template_linux_key.json", package="AzureDSR")
      para_path <- system.file("etc", "parameter_linux_key.json", package="AzureDSR")
    } else if(vmauthen == "Password") {
      temp_path <- system.file("etc", "template_linux.json", package="AzureDSR")
      para_path <- system.file("etc", "parameter_linux.json", package="AzureDSR")
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
    gsub("<USER>", vmusername, .) %>%
    gsub("<VMSIZE>", vmsize, .) %>%
    gsub("<PWD>", vmpassword, .) %>%
    gsub("<PUBKEY>", vmpubkey, .) %>%
    paste0(collapse="")

  # Update the template JSON with the appropriate parameters.

  templ <-
    readLines(temp_path) %>%
    gsub("<DNS_LABEL>", vmdns, .) %>%
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
