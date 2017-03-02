#' @title Check if a resource group exists.
#'
#' @param context Authentication context of AzureSMR encapsulating the
#'   TID, CID, and key obtained from Azure Actrive Directory.
#' @param resource.group The Azure resource group where the DSVM is
#'   created.
#' @param location Location of the data centre to host the DSVM.
#' @export
existsRG <- function(context, resource.group, location, verbose=FALSE)
{
  context %>%
  azureListRG() %>%
  filter(name == RG) %>%
  select(name, location) %T>%
  {if (verbose) print(.)} %>%
  nrow() %>%
  equals(0) %>%
  not()
}
