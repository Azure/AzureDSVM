#' @title This function creates a cluster of Data Science Virtual Machines and enable the DSVMs to communicate across the cluster via public key based credentials for high performance computation. All DSVMs in the cluster are based on Linux OS and use public key cryptogrphy for log in.
#' @param context AzureSMR active context.
#' @param resource.group The Azure resource group where the DSVM is allocated.
#' @param location Location of the data centre to host the DSVM.
#' @param count Number of DSVM instances to be created. Note deploying multiple DSVMs may consume some time.
#' @param name Names of the DSVMs. Lowercase characters or numbers only. Special characters are not permitted.
#' @param username User name of the DSVM. It should be different from name of the DSVM.
#' @param size Size of the DSVM cluster is identical.
#' @param pubkey Public key for the DSVM. Only applicable for
#'   public-key based authentication of Linux based DSVM.
#' @param dns DNS label for the VM address. The URL for accessing the deployed DSVM will be "<dns_label>.<location>.cloudapp.azure.com
#' @param cluster A logical value of TRUE or FALSE to indicate whether the deployed DSVMs form a cluster. If not, the deployment will assign the vectors of name, username, and public key as given in the input arguments to the DSVMs - this is usually used for creating multiple DSVMs for a group of data scientists. If it is TRUE, the deployment will use the first element (if it consists more than one elements) of the given DSVM names as base, and append serial number to the base to form a DSVM full name, and then use the SAME username and public key across the cluster - this can be used for creating a HPC engine on top of the deployed DSVMs in which parallel computing context which is availed in Microsoft R Server ScaleR package can be applied for embarassing parallelization.
#' @export
deployDSVMCluster <- function(context,
                              resource.group,
                              location,
                              count=1,
                              name,
                              username,
                              size="Standard_D1_v2",
                              pubkey="",
                              dns=name,
                              cluster=FALSE) {

  # do some checks for the input arguments.

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

  # other preconditions.

  # let's limit the number of DSVM deployments to a reasonable number.

  if(count > 10) {
    ans <- readline("More than 10 DSVMs are going to be created and that may take a long time to finish. Continue? (y/n)")
    if(ans == "n" || ans == "N")
      return("The deployment is aborted.")
  }

  # to deploy the cluster of DSVMs.

  if(cluster) {

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
