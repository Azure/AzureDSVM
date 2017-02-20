#' @title This function creates a cluster of Data Science Virtual Machines and enable the DSVMs to communicate across the cluster via public key based credentials for high performance computation. All DSVMs in the cluster are based on Linux OS and use public key cryptogrphy for log in.
#' @param context AzureSMR active context.
#' @param resource.group The Azure resource group where the DSVM is allocated.
#' @param location Location of the data centre to host the DSVM.
#' @param count Number of DSVM instances to be created. Note deploying multiple DSVMs may consume some time.
#' @param name Names of the DSVMs. Lowercase characters or numbers only. Special characters are not permitted.
#' @param username User name of the DSVM. It should be different from name of the DSVM.
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

    HOME_DIR <- ifelse(identical(.Platform$OS.type, "windows"),
                       normalizePath(paste0(Sys.getenv("HOME"), "/../"), winslash = "/"),
                       Sys.getenv("HOME"))

    # generate public key from private key.

    shell(paste0("ssh-keygen -y -f ", HOME_DIR, ".ssh/id_rsa > ./id_rsa.pub"))

    # copy private key into local directory. Note local machine may be either Linux or Windows based so it is treated differently.

    ifelse(identical(.Platform$OS.type, "windows"),
           system(paste0("xcopy /f ", shQuote(paste0(HOME_DIR, ".ssh/id_rsa"), type = "cmd"),
                         " ", shQuote(".", type = "cmd"))),
           system("cp ~/.ssh/id_rsa ."))

    dns_name_list <- paste(names,
                           location,
                           "cloudapp.azure.com",
                           sep=".")

    # Distribute the key pair to all nodes of the cluster.

    for (vm in dns_name_list)
    {
      # add an option to switch off host key checking - for the purpose of avoiding pop up.

      option <- "-q -o StrictHostKeyChecking=no"

      # copy the key pairs onto cluster node.

      system(sprintf("scp %s ./id_rsa %s@%s:.ssh/", option, username, vm))
      system(sprintf("scp %s ./id_rsa.pub %s@%s:.ssh/", option, username, vm))

      # create a config file to switch off strick host key checking to enable node-to-node authentication without pop up.

      sh <- writeChar(c("cat .ssh/id_rsa.pub > .ssh/authorized_keys\n",
                        paste0("echo Host *.", location, ".cloudapp.azure.com >> ~/.ssh/config\n"),
                        paste0("echo StrictHostKeyChecking no >> ~/.ssh/config\n"),
                        paste0("echo UserKnownHostsFile /dev/null >> ~/.ssh/config\n"),
                        "chmod 600 ~/.ssh/config\n"), con = "./shell_script")

      # upload, change mode of, and run the config script.

      system(sprintf("scp %s shell_script %s@%s:~", option, username, vm), show.output.on.console = FALSE)
      system(sprintf("ssh %s -l %s %s 'chmod +x ~/shell_script'", option, username, vm), show.output.on.console = FALSE)
      system(sprintf("ssh %s -l %s %s '~/shell_script'", option, username, vm), show.output.on.console = FALSE)
    }

    # Clean up - if you search "remove password" you will get 284,505 records so the following is to clean up confidential information in the working directory to prevent them from leaking anywhere out of your control.

    file.remove("./id_rsa", "./id_rsa.pub", "./shell_script")
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
