#' Deploy a cluster of Linux Data Science Virtual Machines on Azure.
#'
#' Creates a cluster of Data Science Virtual Machines and enables the
#' DSVMs to communicate across the cluster via public key based
#' credentials for high performance computation. All DSVMs in the
#' cluster are based on Linux OS and use public key cryptogrphy for
#' log in.
#'
#' @param context AzureSMR active context.
#'
#' @param resource.group The Azure resource group where the DSVM is
#'   allocated.
#'
#' @param location Location of the data centre to host the DSVM.
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
#' @param size The size of the DSVMs. Each DSVM is the same size.
#'
#' @param dns DNS label for the VM. By default this is the hostname
#'   but is not required to be. The fully qualified domain name for
#'   accessing the deployed DSVM will then be
#'   "<dns_label>.<location>.cloudapp.azure.com".
#' 
#' @details
#'
#' We identify two specific use cases but recognise there are many
#' that are supported by this function.
#'
#' A cluster is intended as a High Performance Compute engine across
#' the deployed DSVMs supporting a parallel computing context as is
#' available with Microsoft R Server ScaleR package. A cluster is a
#' deployment of multiple identitical DSVMs. A single admin username
#' and public key will be used across the cluster. The individual
#' machine names will be constructed from the provided name with
#' sequential numbers. The data scientist will typically connect to
#' the cluster from their local desktop/laptop running R locally with
#' remote execution for computation. A cluster is typcially created by
#' the data scientist when needed and the resource group deleted on
#' completion of the activity.
#'
#' A collection is a deployment with different usernames and public
#' keys for each of the DSVMS. A vector of usernames must be
#' provided. A colleciton is often used in creating multiple DSVMs for
#' a group of data scientists or for training. A colleciton is often
#' longer lasting than a cluster.
#' 
#' @export
#' 
deployDSVMCluster <- function(context,
                              resource.group,
                              location,
                              hostnames,
                              usernames,
                              pubkeys,
                              count,
                              size="Standard_D1_v2",
                              dns=hostnames)
{
  
  # Check argument pre-conditions.

  if(missing(context))
    stop("Please specify a context (contains TID, CID, KEY).")

  if(missing(resource.group))
    stop("Please specify an Azure resouce group.")

  if(missing(location))
    stop("Please specify a data centre location.")

  if(missing(hostnames))
    stop("Please specify virtual machine hostname(s).")

  if(missing(usernames))
    stop("Please specify virtual machine username(s).")

  # Other preconditions and setup.

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
                 name=hostnames[i],
                 username=usernames[i],
                 size=size,
                 os="Linux",
                 authen="Key",
                 pubkey=pubkeys[i],
                 dns=hostnames[i],
                 mode=ifelse(i == count, "Sync", "ASync"))
  }

  IF CLUSTER single username
  
  # Set up public credentials for the DSVM cluster to allow DSVMs to
  # communicate with each other. This is required if one wants to
  # execute analytics on the cluster with parallel compute context in
  # ScaleR.

  # Do key gen in each node.  Propagate pub keys of each node back
  # to local.  Put the pub keys in authorized_keys and distribute
  # onto nodes.

  fqdns <- paste(hostnames, location, "cloudapp.azure.com", sep=".")

  auth_keys <- character(0)
  tmpkeys   <- tempfile(paste0("AzureDSR_pubkeys_", hostnames[i], "_"))
  
  for (i in 1:count)
  {
    
    # Add an option to switch off host key checking - for the purposes
    # of avoiding pop up. Also do not clog up the user's known_hosts
    # file with all the servers created.

    options <- "-q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
    
    tmpkey <- tempfile(paste0("AzureDSR_pubkey_", hostnames[i], "_"))

    # Generate key pairs in the VM

    cmd <- sprintf("ssh %s -l %s %s %s",
                   options, usernames[i], fqdns[i],
                   "'ssh-keygen -t rsa -N \"\" -f ~/.ssh/id_rsa'")
    system(cmd, intern=TRUE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=FALSE)

    # Copy the public key and append it to the local machine.

    cmd <- sprintf("scp %s %s@%s:.ssh/id_rsa.pub %s",
                   options, usernames[i], fqdns[i], tmpkey)

    system(cmd)

    # Append the public keys into authorized_key.

    auth_keys <- paste0(auth_keys, readLines(tmpkey), "\n")

    writeLines(auth_keys, tmpkeys)

    # Clean up the temp pub key file

    file.remove(tmpkey)
  }

  # Create a config file. To avoid any prompt when nodes are
  # communicating with each other.

  tmpscript <- tempfile(paste0("AzureDSR_script_", hostnames[i], "_"))

  sh <- writeChar(paste0("cat .ssh/pub_keys >> .ssh/authorized_keys\n",
                         "echo Host *.", location,
                         ".cloudapp.azure.com >> ~/.ssh/config\n",
                         "echo StrictHostKeyChecking no >> .ssh/config\n",
                         "echo UserKnownHostsFile /dev/null >> .ssh/config\n",
                         "chmod 600 .ssh/config\n"),
                  con=tmpscript)

  # Distribute the public keys and config files to nodes.

  for (i in 1:count)
  {
    # Copy the pub_keys onto node.

    system(sprintf("scp %s %s %s@%s:.ssh/pub_keys",
                   options, tmpkeys, usernames[i], fqdns[i]))

    # Copy the config onto node and run it.

    system(sprintf("scp %s %s %s@%s:.ssh/shell_script",
                   options, tmpscript, usernames[i], fqdns[i]))

    system(sprintf("ssh %s -l %s %s 'chmod +x .ssh/shell_script'",
                   options, usernames[i], fqdns[i]))
    
    system(sprintf("ssh %s -l %s %s '.ssh/shell_script'",
                   options, usernames[i], fqdns[i]))
  }
  
  # Clean up.

  file.remove(tmpkeys, tmpscript)

  # Return results for reference.

  data.frame(hostname = hostnames,
             username = usernames,
             fqdn     = fqdns,
             size     = rep(size, count),
             stringsAsFactors=FALSE)
}
