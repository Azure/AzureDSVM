# an R object to handle remote execution and interaction

rInterface <- setClass(

  # Set the name of the class

  "rInterface",

  representation(
    remote = "character",
    user   = "character",
    script = "character",
    config = "list"
  ),

  prototype(
    remote = character(0),
    user   = character(0),
    script = character(0),
    config = NULL
  )
)

#' @title
