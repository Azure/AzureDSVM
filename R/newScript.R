#' @title Generate a new worker script which is run on the remote instance with specifications in R interface object configuration.
#' @param path Path to the script.
#' @param title Title of the script.
#' @export
newScript <- function(path=".",
                      title=paste0("worker_new_", Sys.time(), ".R")) {
  notes <-
    sprintf(
      paste(
        "\n# ---------------------------------------------------------------------------",
        "# Your worker script starts from here ... ",
        "# ---------------------------------------------------------------------------\n",
        sep="\n"
      )
    )
  if (missing(path) || missing(title))
  {
    stop(sprintf("A default script named %s located at %s is created.", title, path))
  }

  cat(notes, file=file.path(path, title))
  writeLines(
    sprintf("Worker script %s is created at %s.",
            title, ifelse(path == ".", "work directory", path))
  )
}
