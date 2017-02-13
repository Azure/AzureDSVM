# Probably directly use remote functions in "msrdeploy" is a good idea...
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
