#' Remote execution of R script in an R interface new_interface.
#'
#' @param context AzureSMR context.
#'
#' @param resourceGroup Resource group of Azure resources for computation.
#'
#' @param machines Remote DSVMs that will be used for computation.
#'
#' @param remote IP address or FQDN for a computation engine. For
#'   DSVM, it is either the fully qualified domain name (usually in the format of
#'   <hostname>.<location>.cloudapp.azure.com) or its public IP
#'   address. Note if more than one machines are used for execution,
#'   the remote is used as master node by default.
#'
#' @param user Username for logging into the remote resource.
#'
#' @param script R script to be executed on remote resource(s).
#'
#' @param master IP address or URL of a DSVM which will be used as the
#'   master. By default is remote.
#'
#' @param slaves IP addresses or URLs of slave DSVMs.
#'
#' @param computeContext Computation context of Microsoft R Server
#'   under which the mechanisms of parallelization (e.g., local
#'   parallel, cluster based parallel, or Spark) is
#'   specified. Accepted computing context include "localParallel",
#'   "clusterParallel", "Hadoop", and "Spark".
#'
#' @return Status of scription execution.
#'
#' @details
#'
#' For a localParallel compute context,
#'
#' @export

executeScript <- function(context,
                          resourceGroup,
                          machines,
                          remote,
                          user,
                          script,
                          master,
                          slaves,
                          computeContext)
{

  # Check pre-conditions.

  if(missing(context) | !is.azureActiveContext(context))
    stop("Please provide a valid AzureSMR active context.")

  if(missing(resourceGroup))
    stop("Please specify a resource group.")

  if(missing(machines))
    stop("Please give a list of virtual machines.")

  if(missing(remote))
    stop("Please specify a remote machine.")

  if(missing(user))
    stop("Please give user name for the remote login.")

  if(missing(script))
    stop("Please specify the script to be executed remotely with full path.")

  # Check master and slave only when it is cluster parallel.

  if(computeContext == "clusterParallel")
  {
    if(missing(master))
      stop("Please specify a master node.")

    if(missing(slaves))
      stop("Please specify slaves.")
  }

  # Switch on the machines.
  
  message("The machines will be started sequentially.")

  for (vm in machines)
  {
    # Starting a machine is running in synchronous mode so let's wait
    # for a while patiently until everything is done.

    operateDSVM(context,
                resource.group=resourceGroup,
                name=vm,
                operation="Start")
  }

  # Manage input strings in an interface new_interface.

  new_interface <- createComputeInterface(remote, user, script)

  # set configuration

  new_interface %<>% setConfig(machine_list=machines,
                               master=master,
                               slaves=slaves,
                               dns_list=c(master, slaves),
                               machine_user=user,
                               context=computeContext)

  # print interface contents.

  dumpInterface(new_interface)

  # update script with computing context.

  updateScript(new_interface)

  # execute script on remote machine(s).

  option <- "-q -o StrictHostKeyChecking=no"
  remote_script <- paste0("script_", paste0(sample(letters, 5), collapse=""), ".R")
  
  # to avoid issues with drive letter in Windows system.
  
  script_name <- basename(new_interface$script)
  script_path <- sub(x=new_interface$script, pattern=script_name, replacement="")
  curr_wd <- getwd()
  setwd(script_path)

  exe <- system(sprintf("scp %s %s %s@%s:~/%s",
                        option,
                        # new_interface$script,
                        file.path(".", script_name),
                        new_interface$user,
                        new_interface$remote,
                        remote_script),
                show.output.on.console=FALSE)
  if (is.null(attributes(exe)))
  {
    writeLines(sprintf("File %s is successfully uploaded on %s$%s.",
                       new_interface$script, new_interface$user, new_interface$remote))
  } else {
    writeLines("Something must be wrong....... See warning message.")
  }
  
  setwd(curr_wd)

  # Execute the script.

  exe <- system(sprintf("ssh %s -l %s %s Rscript %s",
                        option,
                        new_interface$user,
                        new_interface$remote,
                        remote_script),
                intern=TRUE,
                show.output.on.console=TRUE)
  if (is.null(attributes(exe)))
  {
    writeLines(sprintf("File %s is successfully executed on %s$%s.",
                       new_interface$script, new_interface$user, new_interface$remote))
  } else {
    writeLines("Something must be wrong....... See warning message.")
  }

  writeLines(exe)

  # need post-execution message...

  # clean up - remove the script.

  system(sprintf("ssh %s -l %s %s rm %s",
                 option,
                 new_interface$user,
                 new_interface$remote,
                 remote_script))
}

#' @title Upload or download files.
#' @param from Source location (path) of file.
#' @param to Target location (path) of file.
#' @param file File name - a character string.
#' @note File transfer is implemented by `scp` with public key based authentication.
#' @export
fileTransfer <- function(from=".",
                         to=".",
                         user,
                         file) {
  if(missing(file)) stop("Please specify a file to transfer.")

  option <- "-q -o StrictHostKeyChecking=no"

  if(stringr::str_detect(from, ":")) {
    cmd <- sprintf("scp %s %s %s",
                   option,
                   file.path(paste0(user, "@", from), file),
                   to)
  } else {
    cmd <- sprintf("scp %s %s %s",
                   option,
                   file.path(from, file),
                   paste0(user, "@", to))
  }

  exe <- system(cmd,
                intern=TRUE,
                show.output.on.console=TRUE)
  if (is.null(attributes(exe)))
  {
    writeLines(sprintf("File %s has been successfully transferred.", file))
  } else {
    writeLines("Something must be wrong....... See warning message.")
  }
}
