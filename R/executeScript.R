#' Remote execution of R script in an R interface new_interface.
#' 
#' @inheritParams deployDSVM
#'
#' @param remote IP address or FQDN for a computation engine. For
#'   DSVM, it is either the fully qualified domain name (usually in 
#'   the format of
#'   <hostname>.<location>.cloudapp.azure.com) or its public IP
#'   address. Note if more than one machines are used for execution,
#'   the remote is used as master node by default.
#'
#' @param script R script to be executed on remote resource(s).
#'
#' @param master IP address or URL of a DSVM which will be used as the
#'   master. By default is remote.
#'
#' @param slaves IP addresses or URLs of slave DSVMs.
#'
#' @param compute.context Computation context of Microsoft R Server
#'   under which the mechanisms of parallelization (e.g., local
#'   parallel, cluster based parallel, etc.) is
#'   specified. Accepted computing context include "localSequential", 
#'   "localParallel", and
#'   "clusterParallel".
#'
#' @return Status of scription execution.
#'
#' @details
#'
#' For a "localSequential" compute context, there is no parallelism and 
#' the analytics run in a sequential manner. In the "localParallel" 
#' compute context, the analytics will be run across available cores 
#' of a computing node. The number of available cores can be checked with
#'  Microsoft RevoScaleR function \code{rxGetOption}. The "clusterParallel" 
#'  compute context enables parallel computation across coputing nodes of
#'   a cluster.
#' 
#' @references Details about distributed computing with Microsoft 
#' RevoScaleR package can be found at 
#' https://msdn.microsoft.com/en-us/microsoft-r/scaler-distributed-computing.
#' 
#' @export
executeScript <- function(context,
                          resource.group,
                          hostname,
                          remote,
                          username,
                          script,
                          master="",
                          slaves="",
                          compute.context=ifelse(master == "" && slaves == "",
                                                 "localParallel",
                                                 "clusterParallel"))
{

  # Check pre-conditions.

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
 
  if(missing(remote))
    stop("Please specify a remote machine.")
  # some check for a valid endpoint.

  if(missing(script))
    stop("Please specify the script to be executed remotely with full path.")
  # some check for a valid file name.

  # Check master and slave only when it is cluster parallel.
  
  compute.context <- match.arg(compute.context, c("localParallel",
                                                  "localSeq", 
                                                  "clusterParallel"))

  if(compute.context == "clusterParallel")
  {
    if(missing(master))
      stop("Please specify a master node.")

    if(missing(slaves))
      stop("Please specify slaves.")
  }

  # Switch on the machines.
  
  message("The machines will be started sequentially.")

  for (vm in hostname)
  {
    # Starting a machine is running in synchronous mode so let's wait
    # for a while patiently until everything is done.

    operateDSVM(context,
                resource.group=resource.group,
                name=vm,
                operation="Start")
  }

  # Manage input strings in an interface new_interface.

  new_interface <- createComputeInterface(remote, username, script)

  # set configuration

  new_interface <- setConfig(newinterface,
                             machine_list=hostname,
                             master=master,
                             slaves=slaves,
                             dns_list=c(master, slaves),
                             machine_user=username,
                             context=compute.context)

  # print interface contents.

  dumpInterface(new_interface)

  # update script with computing context.

  updateScript(new_interface)

  # execute script on remote machine(s).

  option <- "-q -o StrictHostKeyChecking=no"
  remote_script <- paste0("script_", 
                          paste0(sample(letters, 5), collapse=""), 
                          ".R")
  
  # to avoid issues with drive letter in Windows system.
  
  script_name <- basename(new_interface$script)
  script_path <- sub(x=new_interface$script, 
                     pattern=script_name, 
                     replacement="")
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
                       new_interface$script, 
                       new_interface$user,
                       new_interface$remote))
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
                       new_interface$script, 
                       new_interface$user, 
                       new_interface$remote))
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
  
  return(TRUE)
}

#' @title Upload or download files.
#' 
#' @param from Source location (local path or remote FQDN and path) of file.
#' 
#' @param to Target location (local path or remote FQDN and path) of file.
#' 
#' @param file File name - a character string.
#' 
#' @note File transfer is implemented by `scp` with public key based 
#' authentication so it is limited only to Linux based DSVM at the moment. 
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' # copy a file named "script.R" from local current working directory to 
#' remote (e.g., IP address is 192.168.19.1) home directory.
#' fileTransfer(from=".", to="192.168.19.1:~", user="admin", file="script.R")}
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
    if(!file.exists(file.path(from, file))) stop("File does not exist!")
    
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
