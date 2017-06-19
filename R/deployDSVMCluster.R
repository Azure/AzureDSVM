#' Deploy a cluster of Linux Data Science Virtual Machines on Azure.
#'
#' Creates a cluster of Data Science Virtual Machines and enables the
#' DSVMs to communicate acros the cluster via public key based
#' credentials for high performance computation. All DSVMs in the
#' cluster are based on Linux OS and use public key cryptogrphy for
#' log in.
#'
#' @inheritParams deployDSVM
#'   
#' @param count If provided this is the number of DSVM instances to be
#'   created. If not provided the number of DSVMs created will be
#'   either the number of names provided or the number of username
#'   provided.
#'
#' @details
#'
#' Note clustering of DSVMs for high performance computing will be 
#' deprecated. Users are encouraged to use azureDoParallel package 
#' for the same purpose with Azure Batch Services. 
#'
#' The current function supports for deployment of identical DSVMs 
#' (i.e., size, OS, etc.) or heterogeneous DSVMs with differerent 
#' specifications.
#'
#' @export
#'
#' @examples 
#' \dontrun{
#' # The following deploys a cluster of 5 Linux DSVMs.
#' 
#' deployDSVMCluster(context, resource.group="<resource_group>", 
#' location="<location>", hostname="<machine_name>", username="<user_name>", 
#' os="Windows", pubkey="<a_valid_public_key_string_in_SSH_format>", count=5)
#' 
#' # The following deploys a collection of 3 Linux DSVMs with different 
#' names and public keys.
#' 
#' deployDSVMCluster(context, resource.group="<resource_group>", 
#' location="<location>", hostname=c("<machine_name_1>", "<machine_name_2>", 
#' "<machine_name_3>", username=c("<user_name_1>", "<user_name_2>", 
#' "<user_name_3>"), os="Windows", 
#' pubkey="<a_valid_public_key_string_in_SSH_format>"}
deployDSVMCluster <- function(context,
                              resource.group,
                              location,
                              hostname,
                              username,
                              authen="Key",
                              pubkey="",
                              password="",
                              os="Ubuntu",
                              size="Standard_D1_v2",
                              dns.label=hostname,
                              count)
{

  # Check if token is valid.

  AzureSMR::azureCheckToken(context)

  # Check argument pre-conditions.

  if(missing(context))
    stop("Please specify a context (contains TID, CID, KEY).")
  assert_that(AzureSMR:::is.azureActiveContext(context))

  if(missing(resource.group))
    stop("Please specify an Azure resouce group.")
  assert_that(AzureSMR:::is_resource_group(resource.group))

  if(missing(location))
    stop("Please specify a data centre location.")
  assert_that(AzureSMR:::is_location(location))

  if(missing(hostname))
    stop("Please specify virtual machine hostname(s).")
  assert_that(AzureSMR:::is_vm_name(hostname))

  if(missing(username))
    stop("Please specify virtual machine username(s).")
  assert_that(AzureSMR:::is_admin_user(username))
  
  if(missing(authen))
    stop("Please specify authentication method(s).")
  
  if(authen == "Password" && missing(password))
    stop("Please specify virtual machine password(s).")
  
  if(authen == "Key" && missing(pubkey))
    stop("Please specify virtual machine public key(s).")
  
  # length of hostname, username, pubkey, os, size, and dns.label
  # should always be the same.
  
  input_args_number <- c(length(hostname),
                         length(username),
                         length(dns.label))
  
  if (!identical(input_args_number, 
                 rep(input_args_number[1], length(input_args_number))))
    stop("Input host names, user names, and dns.label 
         should all have the same length.")

  # If no count is provided then set it to the number of hostname or
  # username supplied. 

  if (missing(count))
    count <- ifelse(length(hostname) == 1,
                    ifelse(length(username) == 1,
                           1, length(username)),
                    length(hostname))
  
  if (count == 1) 
    stop("If 'count' is specified, it should be greater than 1.")

  # If the count is greater than 1 then ensure we have the right
  # lengths of hostname, username, and public keys.

  if (length(hostname) == 1)
    hostname <- sprintf("%s%03d", hostname, 1:count)
  
  if (length(username) == 1)
    username <- rep(username, count)
  
  if (length(authen) == 1)
    authen <- rep(authen, count)
  
  if (length(pubkey) == 1)
    pubkey <- rep(pubkey, count)
  
  if (length(password) == 1)
    password <- rep(password, count)
  
  if (length(os) == 1)
    os <- rep(os, count)
  
  if (length(size) == 1)
    size <- rep(size, count)
  
  if (length(dns.label) == 1)
    dns.label <- rep(dns.label, count)

  # Check the number of DSVM deployments to be a reasonable number but
  # allow the user to override. This is only useful if interactive.

  if(count > 10)
  {
    ans <- readline(paste("More than 10 DSVMs have been requested.",
                          "Continue? (y/n)"))
    if(tolower(ans) == "n")
      stop("The deployment is aborted.")
  }

  # Deploy the DSVMs. All but the last one are deployed asynchronosly
  # and the last is deployed synchonously so that the function returns
  # once the last has successfully deployed. TODO maybe we have a mode
  # argument defaults to "Sync" so user could choose all to be "AScyn"
  # if desired.

  for (i in 1:count)
  {
      deployDSVM(context=context,
                 resource.group=resource.group,
                 location=location,
                 hostname=hostname[i],
                 username=username[i],
                 size=size[i],
                 os=os[i],
                 authen=authen[i],
                 pubkey=pubkey[i],
                 password=password[i],
                 dns.label=hostname[i],
                 mode=ifelse(i == count, "Sync", "Async"))
  }

  # For a cluster set up public credentials for the DSVM cluster to
  # allow DSVMs to communicate with each other. This is required if
  # one wants to execute analytics on the cluster with parallel
  # compute context in ScaleR.
  
  # NOTE: this will be soon deprecated. The users are encouraged to use
  # doAzureParallel package for high-performance computation.

  if (length(unique(username)) == 1 && 
      all(authen == "Key") &&
      all(os == "Ubuntu" ||
          os == "CentOS" ||
          os == "RServer"))
  {
    # sleep for a while as ssh to a ubuntu LDSVM cannot be immediately 
    # executed after deployment.
    Sys.sleep(20)
    
    df <- keyDistribution(location=location,
                          hostname=hostname,
                          username=username,
                          count=count,
                          dns.label=dns.label)
    
    df
  }
}
