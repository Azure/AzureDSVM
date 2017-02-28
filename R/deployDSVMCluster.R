#' Deploy a cluster of Data Science Virtual Machines on Azure.
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
#' @param count If provided this is the number of DSVM instances to be
#'   created. If not provided the number of DSVMs created will be
#'   either the number of names provided or the number of usernames
#'   provided.
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
#' @param size The size of the DSVMs. Each DSVM is the same size.
#'
#' @param dns DNS label for the VM address. The fully qualified domain
#'   name for accessing the deployed DSVM will be
#'   "<dns_label>.<location>.cloudapp.azure.com". The deafult is to
#'   use the hostname as the dns label.
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
                              dns=name)
{
  
  # Check argument pre-conditions.

  if(missing(context))
    stop("Please specify a context (contains TID, CID, KEY).")

  if(missing(resource.group))
    stop("Please specify an Azure resouce group.")

  if(missing(location))
    stop("Please specify a data centre location.")

  if(missing(name))
    stop("Please specify virtual machine name(s).")

  if(missing(username))
    stop("Please specify virtual machine user name(s).")

  # Other preconditions.

  # Limit the number of DSVM deployments to a reasonable number but
  # allow the user to override. This is only useful if interactive.

  if(count > 10)
  {
    ans <- readline(paste("More than 10 DSVMs are going to be created and",
                          "that may take a long time to finish.",
                          # DOES IT REALLY TAKE A LONG TIME? WON'T IT
                          # BE DONE ASYNC EXCEPT FOR LAST SO TIME
                          # TAKEN IS TIME OF THE LAST?
                          "Continue? (y/n)"))
    if(ans == "n" || ans == "N")
      return("The deployment is aborted.")
  }

  # Deploy the DSVMs.

  #TODO BREAK INTO TWO SMALLER FUNCTIONS - ONE FOR CLUSTER AND OTHER
  #FOR COLLECTION

  
  if(!cluster)
  {

    name <- ifelse(length(name) > 1, name[1], name)

    # append serial no. to name base to form a full name.

    names <- paste0(name, sprintf("%03d", 1:count))

    for (inc in 1:count) {
      deployDSVM(context=context,
                 resource.group=resource.group,
                 location=location,
                 name=names[inc],
                 username=username,
                 size=size,
                 os="Linux",
                 authen="Key",
                 pubkey=pubkey,
                 dns=names[inc],
                 mode=ifelse(inc==count, "Sync", "ASync"))
    }

    # set up public credentials for DSVM cluster, to allow DSVMs to communicate with each other. This is required if one wants to execute analytics on the cluster with parallel compute context in ScaleR.

    # TODO: transmitting private key is not good practice! Seeking a better method...

    # do key gen in each node.
    # propagate pub keys of each node back to local.
    # put the pub keys in authorized_keys and distribute onto nodes.

    dns_name_list <- paste(names,
                           location,
                           "cloudapp.azure.com",
                           sep=".")

    auth_keys <- character(0)

    for (vm in dns_name_list) {
      # add an option to switch off host key checking - for the purpose of avoiding pop up.

      option <- "-q -o StrictHostKeyChecking=no"
      pubkey_name <- str_c("pubkey", names[which(dns_name_list == vm)])

      # generate key pairs in vm

      system(sprintf("ssh %s -l %s %s %s",
                     option,
                     username,
                     vm,
                     "'ssh-keygen -t rsa -N \"\" -f ~/.ssh/id_rsa'"),
             intern=TRUE,
             ignore.stdout=FALSE,
             ignore.stderr=FALSE,
             wait=FALSE,
             show.output.on.console=FALSE)

      # copy the public key and append it into local machine.

      system(sprintf("scp %s %s@%s:.ssh/id_rsa.pub %s",
                     option,
                     username,
                     vm,
                     file.path(".", pubkey_name)))

      # append the public keys into authorized_key.

      auth_keys <- paste0(auth_keys,
                          readLines(file.path(".", pubkey_name)),
                          "\n")

      writeLines(auth_keys, file.path(".", "pub_keys"))

      # clean up the temp pub key file

      file.remove(pubkey_name)
    }

    # create a config file. To avoid any prompt up when nodes are communicating with each other.

    sh <- writeChar(paste0("cat .ssh/pub_keys >> .ssh/authorized_keys\n",
                           paste0("echo Host *.", location, ".cloudapp.azure.com >> ~/.ssh/config\n"),
                           paste0("echo StrictHostKeyChecking no >> .ssh/config\n"),
                           paste0("echo UserKnownHostsFile /dev/null >> .ssh/config\n"),
                           "chmod 600 .ssh/config",
                           "\n"),
                    con="./shell_script")

    # distribute the public keys and config files to nodes.

    for (vm in dns_name_list) {

      # copy the pub_keys onto node.

      system(sprintf("scp %s ./pub_keys %s@%s:.ssh",
                     option,
                     username,
                     vm))

      # copy the config onto node and run it.

      system(sprintf("scp %s ./shell_script %s@%s:.ssh",
                     option,
                     username,
                     vm))

      system(sprintf("ssh %s -l %s %s 'chmod +x .ssh/shell_script'",
                     option,
                     username,
                     vm),
             show.output.on.console=TRUE)

      system(sprintf("ssh %s -l %s %s '.ssh/shell_script'",
                     option,
                     username,
                     vm),
             show.output.on.console=TRUE)
    }

    # clean up.

    file.remove("./pub_keys", "./shell_script")

  } else {

    # check whether the input arguments are valid for the multi-instance deployment-
    # 1. VM names, usernames, and public key should character vectors that have the same length as the number of DSVMs.
    # 2. The name and username vectors should not contain duplicated elements.

    if(all(length(name) == count,
           length(username) == count,
           length(pubkey) == count) &&
       !any(c(duplicated(name), duplicated(username)))) {

         names <- name
         dns_name_list <- paste(names,
                                location,
                                "cloudapp.azure.com",
                                sep=".")

         for (inc in 1:count) {
           # TODO - COLLECT THE RETURN VALUES AND RETURN THAT
           deployDSVM(context=context,
                      resource.group=resource.group,
                      location=location,
                      name=name[inc],
                      username=username[inc],
                      size=size,
                      os="Linux",
                      authen="Key",
                      pubkey=pubkey[inc],
                      dns=name[inc],
                      mode=ifelse(inc==count, "Sync", "ASync"))
         }
       } else {
         stop("Please check input DSVM names, usernames, and public key. The input vectors should have the same length with count of DSVM and elements of each input vectot should be distinct from each other.")
       }
  }

  # return results for reference.

  df <- data.frame(Names = names,
                   Usernames = rep(username, count),
                   URL = dns_name_list,
                   Sizes = rep(size, count))

  writeLines("Summary of DSVMs deployed")
  print(df)
}
