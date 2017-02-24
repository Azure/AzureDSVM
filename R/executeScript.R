#' @title Remote execution of R script in an R interface object.
#' @param context AzureSMR context.
#' @param resourceGroup Resource group of Azure resources for computation.
#' @param machines Remote DSVMs that will be used for computation.
#' @param remote Remote URL for the computation engine. For DSVM, it is either DNS (usually in the format of <dsvm name>.<location>.cloudapp.azure.com) or IP address.
#' @param user Username for logging into the remote resource.
#' @param script R script to be executed on remote resource.
#' @param computeContext Computation context of Microsoft R Server under which the mechanisms of parallelization (e.g., local parallel, cluster based parallel, or Spark) is specified. Accepted computing context include "localParallel", "clusterParallel", "Hadoop", and "Spark".
#' @return Status of scription execution.
#' @export
executeScript <- function(context,
                          resourceGroup,
                          machines,
                          remote,
                          user,
                          script,
                          computeContext) {

  # switch on the machines.

  for (vm in machines) {
    # starting a machine is running in synchronous mode so let's wait for a while patiently until everything is done.

    operateDSVM(context,
                resource.group=resourceGroup,
                name=vm,
                operation="Start")
  }

  # manage input strings in an interface object.

  new_interface <- createComputeInterface(remote, user, script)

  # set configuration

  new_interface %<>% setConfig(new_interface,
                               dns_list=remote,
                               machine_user=user,
                               context=computeContext)

  # print interface contents.

  dumpInterface(new_interface)

  # update script with computing context.

  updateScript(new_interface)

  # execute script on remote machine(s).

  option <- "-q -o StrictHostKeyChecking=no"
  remote_script <- paste0("script", as.character(Sys.time()), ".R")

  exe <- system(paste0("scp %s -l %s %s %s:%s",
                       option,
                       object$user,
                       object$script,
                       object$remote,
                       remote_script),
                show.output.on.console=FALSE)
  if (is.null(attributes(exe)))
  {
    writeLines(sprintf("File %s is successfully uploaded on %s$%s.",
                       object$script, object$user, object$remote))
  } else {
    writeLines("Something must be wrong....... See warning message.")
  }

  # Execute the script.

  exe <- system(paste("ssh %s -l %s %s Rscript %s %s",
                      option,
                      object$user,
                      object$remote,
                      roptions,
                      remote_script),
                intern=TRUE,
                show.output.on.console=TRUE)
  if (is.null(attributes(exe)))
  {
    writeLines(sprintf("File %s is successfully executed on %s$%s.",
                       object$script, object$user, object$remote))
  } else {
    writeLines("Something must be wrong....... See warning message.")
  }

  if (!missing(verbose))
  {
    if (verbose == TRUE) writeLines(exe)
  }

  # need post-execution message...
}
