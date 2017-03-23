#' @title Deploy a new Data Science Virtual Machine (DSVM).
#'
#' @param context Authentication context of AzureSMR encapsulating the
#'   TID, CID, and key obtained from Azure Actrive Directory.
#'
#' @param resource.group The Azure resource group where the DSVM is
#'   created.
#'
#' @param location Location of the data centre to host the DSVM.
#'
#' @param hostname Name of the DSVM.  Lowercase characters or numbers
#'   only. Special characters are not permitted.
#'
#' @param username User name of the DSVM. It should be different from
#'   `name`.
#'
#' @param size Size of the DSVM. The default is "Standard_D1_v2". All
#'   available sizes can be obtained by function `getVMSizes`.
#'
#' @param os Operating system of DSVM. Permitted values are "Linux"
#'   and "Windows". The default is to deploy a Linux Data Science
#'   Virtual Machine.
#'
#' @param authen Either "Key" for public-key based authentication
#'   (with Linux) or "Password" for a password based authentication
#'   (Linux or Windows). Default is to use public key authentication
#'   for Linux and password based authentication for Windows.
#'
#' @param pubkey Public key for the DSVM. Only applicable for
#'   public-key based authentication of Linux based DSVM.
#'
#' @param password Pass word for the DSVM.
#'
#' @param dns.label DNS label for the VM address. The URL for
#'   accessing the deployed DSVM will be
#'   "<dns_label>.<location>.cloudapp.azure.com
#'
#' @param mode Mode of virtual machine deployment. Default is "Sync".
#'
#' @details
#'
#' Usually an error message will be returned if the deployment is unsuccessful. If the deployment fails without explicit error message returned visit the Azure portal
#' https://ms.portal.azure.com and browse to the resource group and
#' click on the failed deployment link to view the failure
#' message. Typical errors include DnsRecordInUse or
#' StorageAccountAlreadyTaken. If so then choose a different hostname.
#'
#' @export
deployDSVM <- function(context,
                       resource.group,
                       location,
                       hostname,
                       username,
                       size="Standard_D1_v2",
                       os="Linux",
                       authen=ifelse(os=="Linux", "Key", "Password"),
                       pubkey="",
                       password="",
                       dns.label=hostname,
                       mode="Sync")
{
  # Check if token is valid.

  AzureSMR::azureCheckToken(context)

  # Check if required arguments are present.

  if(missing(context))
    stop("Please specify a context (contains TID, CID, KEY).")

  if(missing(resource.group))
    stop("Please specify an Azure resouce group.")

  if(missing(location))
    stop("Please specify a data centre location.")

  if(missing(hostname))
    stop("Please specify a virtual machine hostname.")

  if(missing(username))
    stop("Please specify a virtual machine user name.")

  if(authen == "Key" && missing(pubkey))
    stop("Please specify a public key.")

  if(authen == "Password" && missing(password))
    stop("Please specify a password.")

  ## Other preconditions.

  # Check if AzureSMR context is valid.

  if(!AzureSMR::is.azureActiveContext(context))
    stop("Please use a valid AzureSMR context.")

  # Check if resource group exists.

  rg_exist <- existsRG(context, RG, LOC)

  if(!rg_exist)
    stop("The specified resource group does not exist in the current region.")

  # Check if vm size is available.

  vm_available <- getVMSizes(context, location)

  if(!(size %in% unlist(select(vm_available, name))))
    stop("Unknown size - see getVMSizes() for allowed options.")

  # Incorrect naming of a vm may lead to an unsuccessful deployment of
  # the DSVM - normally it returns a 400 error from REST call. Check
  # the name here to ensure it is valid.

  if(length(hostname) > 15)
    stop("Name of virtual machine is too long at ", length(hostname),
         "characters. At most it can be 15 characters.")

  if(grepl("[[:upper:]]|[[:punct:]]", hostname))
    stop("Name of virtual machine is not valid - only lowercase and digits permitted.")

  # check if password is valid.

  # if(!grepl("^(?=.*[[A-Za-z]])(?=.*\\d)(?=.*[[$@$!%*#?&]])[[A-Za-z\\d$@$!%*#?&]]{8,}$", password))
  #   stop("Password not valid - minimum 8 characters with at least one digit and one special character.")

  # Load template and parameter JSON files for deployment

  if(os == "Windows")
  {
    temp_path <- system.file("etc", "template_windows.json", package="AzureDSVM")
    para_path <- system.file("etc", "parameter_windows.json", package="AzureDSVM")
  } else if(os == "Linux")
  {
    if(authen == "Key")
    {
      temp_path <- system.file("etc", "template_linux_key.json", package="AzureDSVM")
      para_path <- system.file("etc", "parameter_linux_key.json", package="AzureDSVM")
    } else if(authen == "Password")
    {
      temp_path <- system.file("etc", "template_linux.json", package="AzureDSVM")
      para_path <- system.file("etc", "parameter_linux.json", package="AzureDSVM")
    } else
    {
      stop("Please specific a valid authentication method, i.e., either 'Key' for public key based or 'Password' for password based, for Linux OS based DSVM")
    }
  } else
  {
    stop("Please specify a valid OS type, i.e., either 'Windows' or 'Linux'.")
  }

  # Update the parameter JSON with the virtual machine hostname.

  para_path %>%
    readLines() %>%
    gsub("<LOCATION>", location, .) %>%
    gsub("<DEFAULT>", hostname, .) %>%
    gsub("<USER>", username, .) %>%
    gsub("<VMSIZE>", size, .) %>%
    gsub("<PWD>", password, .) %>%
    gsub("<PUBKEY>", pubkey, .) %>%
    paste0(collapse="") ->
  param

  # Update the template JSON with the appropriate parameters.

  temp_path %>%
    readLines() %>%
    gsub("<DNS_LABEL>", dns.label, .) %>%
    paste0(collapse="") ->
  templ

  dname <- paste0(hostname, "_dpl")

  AzureSMR::azureDeployTemplate(context,
                                deplname=dname,
                                templateJSON=templ,
                                paramJSON=param,
                                resourceGroup=resource.group,
                                mode=mode)

  fqdn <- paste0(dns.label, ".", location, ".cloudapp.azure.com")

  # Use the command line to obtain the IP if in sync model. Should be
  # a way to get this data from Azure. If mode is async then the
  # machine will not be available yet and so no IP to be found from
  # DNS.

  if (Sys.which("dig") != "" && tolower(mode) == "sync")
    attr(fqdn, "ip") <-
      system(paste("dig", fqdn, "+short"), intern=TRUE)

  return(fqdn)
}
