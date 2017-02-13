#' @title Set values of the R interface object.
#' @param object An S3 R interface object.
#' @param remote URL of remote instance.
#' @param user User name.
#' @param script R script with full path for execution at remote instance.
#' @param config Configuration for remote execution. Settings include computing context, data reference, etc.
#' @return The updated R interface object.
setRInterface <- function(object,
                          remote,
                          user,
                          script,
                          config) {
  if(!missing(remote)) object$remote <- remote
  if(!missing(user)) object$user <- user
  if(!missing(script) && file.exists(script))
  {
    object$script <- script
  }
  if(!missing(config)) object$config <- config

  return(object)
}
