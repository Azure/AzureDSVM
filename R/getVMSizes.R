#' Get available sizes for data science virtual machines.
#' 
#' @param context An AzureSMR context.
#' 
#' @param location Location of the Azure resources.
#' 
#' @return A data frame that contains basic information
#' about the available DSVM sizes.
#' 
#' @export
getVMSizes <- function(context, location)
{

  AzureSMR::azureCheckToken(context)

  if(missing(context) || !AzureSMR::is.azureActiveContext(context))
    stop("Please provide a valid AzureSMR context.")

  if(missing(location))
    stop("Please provide a location.")

  api_version <- "2016-04-30-preview"

  url <- paste0("https://management.azure.com/subscriptions/",
                context$subscriptionID,
                "/providers/Microsoft.Compute/locations/",
                location,
                "/vmSizes?api-version=",
                api_version)

  headers <- c(Host="management.azure.com",
               Authorization=context$Token,
               `Content-type`="application/json")

  r <- httr::GET(url, add_headers(.headers=headers))

  rl <- content(r, "text", encoding="UTF-8")

  if(! status_code(r) %in% c(200, 201, 202)) AzureSMR:::stopWithAzureError(r)

  jsonlite::fromJSON(rl)$value
}
