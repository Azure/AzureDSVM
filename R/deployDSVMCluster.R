#' Deploy a cluster of Linux Data Science Virtual Machines on Azure.
#'
#' Creates a cluster of Data Science Virtual Machines and enables the
#' DSVMs to communicate across the cluster via public key based
#' credentials for high performance computation. All DSVMs in the
#' cluster are based on Linux OS and use public key cryptogrphy for
#' log in.
#'
#' @inheritParams deployDSVM
#'
#' @param hostnames Hostnames for the DSVMs. Lowercase characters or
#'   numbers only. If a single hostname is supplied and count > 1 then
#'   the hostname will be used as a prefix for a sequential count of
#'   hostnames.
#'
#' @param usernames Usernames for the admin account created on the
#'   DSVM. If a single username is supplied then that username is used
#'   as the admin user on each host. Otherwise a username is provided
#'   for each of the DSVMs.
#'
#' @param pubkeys Public keys for the DSVM. This is only applicable
#'   for public-key based authentication of Linux based DSVM. One or a
#'   vector of public keys can be provided, depending on the count or
#'   the number of hostnames or usernames.
#'
#' @param count If provided this is the number of DSVM instances to be
#'   created. If not provided the number of DSVMs created will be
#'   either the number of names provided or the number of usernames
#'   provided.
#'
#' @param sizes The size of the DSVMs. Each DSVM is the same size.
#'
#' @param dns.labels DNS labels for the VM. By default this is the
#'   hostnames but is not required to be. The fully qualified domain
#'   name for accessing the deployed DSVM will then be
#'   "<dns.label_label>.<location>.cloudapp.azure.com".
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
#' location="<location>", hostnames="<machine_name>", usernames="<user_name>", 
#' os="Windows", pubkeys="<a_valid_public_key_string_in_SSH_format>", count=5)
#' 
#' # The following deploys a collection of 3 Linux DSVMs with different 
#' names and public keys.
#' 
#' deployDSVMCluster(context, resource.group="<resource_group>", 
#' location="<location>", hostnames=c("<machine_name_1>", "<machine_name_2>", 
#' "<machine_name_3>", usernames=c("<user_name_1>", "<user_name_2>", 
#' "<user_name_3>"), os="Windows", 
#' pubkeys="<a_valid_public_key_string_in_SSH_format>"}
deployDSVMCluster <- function(context,
                              resource.group,
                              location,
                              hostnames,
                              usernames,
                              pubkeys,
                              count,
                              oss="Ubuntu",
                              sizes="Standard_D1_v2",
                              dns.labels=hostnames)
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

  if(missing(hostnames))
    stop("Please specify virtual machine hostname(s).")
  assert_that(AzureSMR:::is_vm_name(hostnames))

  if(missing(usernames))
    stop("Please specify virtual machine username(s).")
  assert_that(AzureSMR:::is_admin_user(usernames))
  
  # length of hostnames, usernames, pubkeys, oss, sizes, and dns labels
  # should always be the same.
  
  input_args_number <- c(length(hostnames),
                         length(usernames),
                         length(pubkeys),
                         length(oss),
                         length(sizes),
                         length(dns.labels))
  
  if (!identical(input_args_number, 
                 rep(input_args_number[1], length(input_args_number))))
    stop("Input host names, user names, public keys, operating systems, 
         VM sizes, and DNS labels should all have the same length.")

  # If no count is provided then set it to the number of hostnames or
  # usernames supplied. 

  if (missing(count))
    count <- ifelse(length(hostnames) == 1,
                    ifelse(length(usernames) == 1,
                           1, length(usernames)),
                    length(hostnames))

  # If the count is greater than 1 then ensure we have the right
  # lengths of hostnames, usernames, and public keys.

  if (count > 1)
  {
    if (length(hostnames) == 1)
      hostnames <- sprintf("%s%03d", hostnames, 1:count)
    if (length(usernames) == 1)
      usernames <- rep(usernames, count)
    if (length(pubkeys) == 1)
      pubkeys <- rep(pubkeys, count)
    if (length(oss) == 1)
      pubkeys <- rep(oss, count)
    if (length(sizes) == 1)
      pubkeys <- rep(sizes, count)
    if (length(dns.labels) == 1)
      pubkeys <- rep(dns.labels, count)
  }

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
                 hostname=hostnames[i],
                 username=usernames[i],
                 size=size[i],
                 os=oss[i],
                 authen="Key",
                 pubkey=pubkeys[i],
                 dns.label=hostnames[i],
                 mode=ifelse(i == count, "Sync", "ASync"))
  }

  # For a cluster set up public credentials for the DSVM cluster to
  # allow DSVMs to communicate with each other. This is required if
  # one wants to execute analytics on the cluster with parallel
  # compute context in ScaleR.

  if (length(unique(usernames)) == 1)
  {
    # sleep for a while as ssh to a ubuntu LDSVM cannot be immediately 
    executed after deployment.
    Sys.sleep(20)
    
    df <- keyDistribution(location=location,
                          hostnames=hostnames,
                          usernames=usernames,
                          count=count,
                          dns.labels=dns.labels)
    
    df
  }
}
