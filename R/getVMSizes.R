#' Get available sizes for data science virtual machines.
#' 
#' @param context An AzureSMR context.
#' 
#' @param location Location of the Azure resources.
#' 
#' @return A data frame that contains basic information such as number of cores, disk size, RAM size, etc.,
#' about the available DSVM sizes.
#' 
#' @export
getVMSizes <- function(context, location)
{

  AzureSMR::azureCheckToken(context)

  if(missing(context))
    stop("Please provide a valid AzureSMR context.")
  assert_that(AzureSMR::is.azureActiveContext(context))

  if(missing(location))
    stop("Please provide a location.")

  api_version <- "2016-04-30-preview"

  url <- paste0("https://management.azure.com/subscriptions/",
                context$subscriptionID,
                "/providers/Microsoft.Compute/locations/",
                location,
                "/vmSizes?api-version=",
                api_version)

  # headers <- c(Host="management.azure.com",
  #              Authorization=context$Token,
  #              `Content-type`="application/json")
  # 
  # r <- httr::GET(url, add_headers(.headers=headers))
  
  r <- AzureSMR:::call_azure_sm(asc,
                                uri=url,
                                verb="GET")
  
  AzureSMR:::stopWithAzureError(r)

  rl <- content(r, "text", encoding="UTF-8")
  
  df_size <- jsonlite::fromJSON(rl)$value
  
  # rename the columns for convenient reading.
  
  df_size$Name  <- df_size$name
  df_size$Cores <- df_size$numberOfCores
  df_size$Disk  <- (df_size$resourceDiskSizeInMB) / 1024
  df_size$RAM   <- (df_size$memoryInMB) / 1024
  df_size$Disks <- df_size$maxDataDiskCount
  
  df_size <- df_size[, c("Name", "Cores", "Disk", "RAM", "Disks")]
  
  names(df_size) <- c("VM Size",
                      "Number of cores",
                      "Disk size (GB)",
                      "RAM size (GB)",
                      "Max number of disks")
  
  df_size
}
