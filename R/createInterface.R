#' @title Create an R interface object that handles interaction with remote instance.
#' @param remote URL of remote instance.
#' @param user User name.
#' @param script R script with full path for execution at remote instance.
#' @param config Configuration for remote execution. Settings include computing context, data reference, etc.
#' @return An S3 R interface object.
#' @export
createRInterface <- function(remote,
                             user,
                             script,
                             config){
  ri_env <- new.env(parent=globalenv())

  # initialize an R interface object.

  if(!missing(remote)) {
    ri_env$remote <- remote
  } else {
    ri_env$remote <- character(0)
  }

  if(!missing(user)) {
    ri_env$user <- user
  } else {
    ri_env$user <- character(0)
  }

  if(!missing(script)) {
    ri_env$script <- script
  } else {
    ri_env$script <- character(0)
  }

  if(!missing(config)) {
    ri_env$config <- config
  } else {
    ri_env$config <- NULL
  }
}
