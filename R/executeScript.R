#' @title Remote execution of R script in an R interface object.
#' @param object R interface object.
#' @param inputs JSON encoded string of R objects that are loaded into the Remote R session's workspace prior to execution. Only R objects of type: primitives, vectors and dataframes are supported via this parameter. Alternatively the putLocalObject can be used, prior to a call to this function, to move any R object from the local workspace into the remote R session.
#' @param outputs Character vector of the names of the objects to retreive. Only primitives, vectors and dataframes can be retrieved using this function. Use getRemoteObject to get any type of R object from the remote session.
#' @param checkLibraries if `TRUE`, check whether libraries used in the R script installed on the remote machine.
#' @param displayPlots If TRUE, plots generated during execution are displayed in the local plot window. **NOTE** This capability requires that the 'png' package is installed on the local machine.
#' @param writePlots If TRUE, plots generated during execution are copied to the working directory of the local session.
#' @return Status of scription execution.
#' @export
executeScript <- function(object,
                          inputs=NULL,
                          outputs=NULL,
                          checkLibraries=FALSE,
                          displayPlots=FALSE,
                          writePlots=FALSE) {

  # add remote spefic header to worker script.

  editScript(object)

  # authenticate the remote server.

  mrsdeploy::remoteLogin(deployr_endpoint=object$remote,
                         session=FALSE,
                         commandline=FALSE,
                         username=object$user)

  # need some exception handling?

  # check libraries in the worker script available on R server. If not, install them. To avoid additional code execution, this operation is invoked only on demand, by argument "checkLibraries".

  if (checkLibraries == TRUE) {
    libs <-
      # str_extract_all(readLines(object$script), "library\\(.*?\\)") %>%
      str_extract_all(x, "library\\(.*?\\)") %>%
      unlist() %>%
      gsub(".*\\((.*)\\).*", "\\1", .) %>%
      str_c()

    codes <- paste(paste0("libs <- c(", paste0("'", libs, "'", collapse=","), ")"),
                   "new.packages <- libs[!(libs %in% installed.packages()[,'Package'])]",
                   "if(length(new.packages)) install.packages(new.packages)",
                   sep=";")

    mrsdeploy::remoteExecute(rcode=codes)
  }

  # remote execution of script.

  resp <- mrsdeploy::remoteScript(name=object$script,
                                  inputs=inputs,
                                  outputs=outputs,
                                  displayPlots=displayPlots,
                                  writePlots=writePlots)

  # need some exception handling?
}
