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
#' @param os Operating system of DSVM. Permitted values are "Ubuntu",
#'   "CentOS", "Windows", "DeepLearning", and "MRS". The default is to deploy
#'   a Ubuntu Linux Data Science Virtual Machine. NOTE previously Windows
#'   based DSVM for deep learning toolkit is using a separated package but
#'   now it is merged into one. 
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
#' Usually an error message will be returned if the deployment is
#' unsuccessful. If the deployment fails without explicit error
#' message returned visit the Azure portal https://ms.portal.azure.com
#' and browse to the resource group and click on the failed deployment
#' link to view the failure message. Typical errors include
#' DnsRecordInUse or StorageAccountAlreadyTaken. If so then choose a
#' different hostname.
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' # The following deploys a Ubuntu DSVM with public key based authentication
#' deployDSVM(context, resource.group="<resource_group>", 
#' location="<location>", hostname="<machine_name>", username="<user_name>", 
#' os="Ubuntu", pubkey="<a_valid_public_key_string_in_SSH_format>")
#' 
#' # The following deploys a Windows DSVM with password based authentication. 
#' The VM size is selected from all the available A-series machines that have 
#' the maximum number of computing cores.
#' 
#' vm <- getVMSizes(context, "<location>")
#' 
#' names(vm)
#' 
#' vm_sub <- vm[grep(vm[, 1], pattern="Basic_A"), ]
#' vm_name <- vm_sub[vm_sub[, 2] == max(vm_sub[, 2]), 1]
#' 
#' deployDSVM(context,
#'            resource.group="<resource_group>",
#'            location="<location>",
#'            hostname="<machine_name>",
#'            username="<user_name>",
#'            os="Windows",
#'            password="<a_valid_password>")}
deployDSVM <- function(context,
                       resource.group,
                       location,
                       hostname,
                       username,
                       size="Standard_D1_v2",
                       os="Ubuntu",
                       authen="Key",
                       pubkey="",
                       password="",
                       dns.label=hostname,
                       mode="Sync")
{
  # Check if token is valid.

  AzureSMR::azureCheckToken(context)

  # Check if required arguments are present.
  
  assert_that(is.azureActiveContext(context))

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

  if(authen == "Key" && missing(pubkey))
    stop("Please specify a valid public key.")
  
  if(authen == "Password" && 
     (missing(password) || !AzureSMR:::is_valid_admin_password(password)))
    stop("Please specify a password.")

  ## Other preconditions.

  # Check if resource group exists.

  rg_exist <- existsRG(context, resource.group, location)

  if(!rg_exist)
    stop("The specified resource group does not exist in the current region.")

  # Check if vm size is available.

  vm_available <- getVMSizes(context, location)

  if(!(size %in% unlist(vm_available[, 1])))
    stop("Unknown size - see getVMSizes() for allowed options.")

  # Load template and parameter JSON files for deployment

  if(os == "Windows")
  {
    temp_path <- system.file("etc", 
                             "template_windows.json", 
                             package="AzureDSVM")
    para_path <- system.file("etc", 
                             "parameter_windows.json", 
                             package="AzureDSVM")
  } else if(os == "DeepLearning") 
  {
    temp_path <- system.file("etc", 
                             "template_deeplearning.json", 
                             package="AzureDSVM")
    para_path <- system.file("etc", 
                             "parameter_windows.json", 
                             package="AzureDSVM")
  }else if(os == "Ubuntu")
  {
    if(authen == "Key")
    {
      temp_path <- system.file("etc", 
                               "template_linux_key_ubuntu.json", 
                               package="AzureDSVM")
      para_path <- system.file("etc", 
                               "parameter_linux_key.json", 
                               package="AzureDSVM")
    } else if(authen == "Password")
    {
      temp_path <- system.file("etc", 
                               "template_linux_ubuntu.json", 
                               package="AzureDSVM")
      para_path <- system.file("etc", 
                               "parameter_linux.json", 
                               package="AzureDSVM")
    } else
    {
      stop("Please specific a valid authentication method, i.e., ",
           "either 'Key' for public key based or 'Password' ",
           "for password based, for Linux OS based DSVM")
    }
  } else if(os == "CentOS")
  {
    if(authen == "Key")
    {
      temp_path <- system.file("etc", 
                               "template_linux_key.json", 
                               package="AzureDSVM")
      para_path <- system.file("etc", 
                               "parameter_linux_key.json", 
                               package="AzureDSVM")
    } else if(authen == "Password")
    {
      temp_path <- system.file("etc", 
                               "template_linux.json", 
                               package="AzureDSVM")
      para_path <- system.file("etc", 
                               "parameter_linux.json", 
                               package="AzureDSVM")
    } else
    {
      stop("Please specific a valid authentication method, ",
           "i.e., either 'Key' for public key based or ",
           "'Password' for password based, for Linux OS based DSVM")
    }
  } else if(os == "RServer") 
  {
    if(authen == "Key")
    {
      temp_path <- system.file("etc", 
                               "template_mrs_key_ubuntu.json", 
                               package="AzureDSVM")
      para_path <- system.file("etc", 
                               "parameter_linux_key.json", 
                               package="AzureDSVM")
    } else if(authen == "Password")
    {
      temp_path <- system.file("etc", 
                               "template_mrs_ubuntu.json", 
                               package="AzureDSVM")
      para_path <- system.file("etc", 
                               "parameter_linux.json", 
                               package="AzureDSVM")
    } else
    {
      stop("Please specific a valid authentication method, 
           i.e., either 'Key' for public key based or 'Password' 
           for password based, for Linux OS based DSVM")
    }
  } else
  {
    stop("Please specify a valid OS type, ",
         "i.e., either 'Windows', 'DeepLearning', ",
         "'CentOS', or 'Ubuntu'.")
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

  # Use the command line to obtain the IP if in sync model. *Should be
  # a way to get this data from Azure.* If mode is async then the
  # machine will not be available yet and so no IP to be found from
  # DNS. Note that as of 20170614 dig fails with the newly created
  # Ubuntu server so let's drop this optional functionality for now.

  #if (Sys.which("dig") != "" && tolower(mode) == "sync")
  #  attr(fqdn, "ip") <-
  #    system(paste("dig", fqdn, "+short"), intern=TRUE)

  return(fqdn)
}
