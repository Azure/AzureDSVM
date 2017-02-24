#' @title Remote execution of R script in an R interface new_interface.
#' @param context AzureSMR context.
#' @param resourceGroup Resource group of Azure resources for computation.
#' @param machines Remote DSVMs that will be used for computation.
#' @param remote IP address or URL for a computation engine. For DSVM, it is either DNS (usually in the format of <dsvm name>.<location>.cloudapp.azure.com) or its public IP address. Note if more than one machines are used for execution, the remote is used as master node by default.
#' @param user Username for logging into the remote resource.
#' @param script R script to be executed on remote resource(s).
#' @param master IP address or URL of a DSVM which will be used as the master. By default is remote.
#' @param slaves IP addresses or URLs of slave DSVMs.
#' @param computeContext Computation context of Microsoft R Server under which the mechanisms of parallelization (e.g., local parallel, cluster based parallel, or Spark) is specified. Accepted computing context include "localParallel", "clusterParallel", "Hadoop", and "Spark".
#' @return Status of scription execution.
#' @export
executeScript <- function(context,
                          resourceGroup,
                          machines,
                          remote,
                          user,
                          script,
                          master,
                          slaves,
                          computeContext) {

  # switch on the machines.

  for (vm in machines) {
    # starting a machine is running in synchronous mode so let's wait for a while patiently until everything is done.

    operateDSVM(context,
                resource.group=resourceGroup,
                name=vm,
                operation="Start")
  }

  # manage input strings in an interface new_interface.

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

  exe <- system(sprintf("scp %s %s %s@%s:~/%s",
                        option,
                        new_interface$script,
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
