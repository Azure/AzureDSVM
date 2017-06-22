#' Get data consumption of an Azure subscription for a time period.

#' Aggregation method can be either daily based or hourly based.
#' 
#' Formats of start time point and end time point follow ISO 8601 standard. For example, if you want to calculate data consumption between Feb 21, 2017 to Feb 25, 2017, with an aggregation granularity of "daily based", the inputs should be "2017-02-21 00:00:00" and "2017-02-25 00:00:00", for start time point and end time point, respectively.
#' If the aggregation granularity is hourly based, the inputs can be "2017-02-21 01:00:00" and "2017-02-21 02:00:00", for start and end time point, respectively. 
#' NOTE by default the Azure data consumption API does not allow an aggregation granularity that is finer than an hour. In the case of "hourly based" granularity, if the time difference between start and end time point is less than an hour, data consumption will still be calculated hourly based with end time postponed.
#' For example, if the start time point and end time point are "2017-02-21 00:00:00" and "2017-02-21 00:45:00", the actual returned results are data consumption in the interval of "2017-02-21 00:00:00" and "2017-02-21 01:00:00". However this calculation is merely for retrieving the information of an existing instance instance (e.g. `meterId`) with which the pricing rate is multiplied by to obtain the overall expense. 
#' Time zone of all time inputs are synchronized to UTC.
#' 
#' @inheritParams deployDSVM
#' 
#' @param instance DSVM instance name that one would like to check expense. 
#' It is by default empty, which returns data consumption for all DSVMs 
#' under subscription.
#' 
#' @param timeStart Start time.
#' @param timeEnd End time.
#' @param granularity Aggregation granularity. Can be either "Daily" or 
#' "Hourly".
#'
#' @family Cost functions
#' @export
dataConsumptionDSVM <- function(context,
                                hostname="",
                                timeStart,
                                timeEnd,
                                granularity="Hourly",
                                verbose=FALSE) {
  
  # check the validity of credentials.
  
  assert_that(is.azureActiveContext(azureActiveContext))
  
  # renew token if it expires.
  
  azureCheckToken(azureActiveContext)
  
  # preconditions here...
  
  if(missing(timeStart))
    stop("Please specify a starting time point in YYYY-MM-DD HH:MM:SS format.")
  
  if(missing(timeEnd))
    stop("Please specify an ending time point in YYYY-MM-DD HH:MM:SS format.")
  
  df_data <- azureDataConsumption(context,
                                  instance=hostname,
                                  timeStart=timeStart,
                                  timeEnd=timeEnd,
                                  granularity=granularity)
  
  return(df_data)
}

#' Calculate cost of using a specific DSVM instance of Azure for certain period.
#' 
#'Note if difference between \code{timeStart} and \code{timeEnd} is less than 
#'the finest granularity, e.g., "Hourly" (we notice this is a usual case when 
#'one needs to be aware of the charges of a job that takes less than an hour),
#' the expense will be estimated based solely on computation hour. That is, 
#' the total expense is the multiplication of computation hour and pricing 
#' rate of the requested instance.
#'
#' @inheritParams deployDSVM
#' @inheritParams dataConsumptionDSVM
#' @inheritParams AzureSMR::azurePricingRates
#' 
#' @return Total cost measured in the given currency of the specified DSVM 
#' instance in the period.
#' 
#' @family Cost functions
#' @export
costDSVM <- function(context,
                     hostname="",
                     timeStart,
                     timeEnd,
                     granularity="Daily",
                     currency="USD",
                     locale="en-US",
                     offerId,
                     region="US",
                     verbose=FALSE) {
  
  if(missing(context)) stop("Please specify an active Azure context")
  assertthat(is.azureActiveContext(context))
  
  if(missing(timeStart)) stop("Please specify a starting time point")
  
  if(missing(timeEnd)) stop("Please specify an ending time point")
  
  if(missing(offerId)) stop("Please specify an Azure subscription offer ID.")
  
  df_cost <- azureExpenseCalculator(context, 
                                    instance=hostname, 
                                    timeStart=timeStart, 
                                    timeEnd=timeEnd, 
                                    granularity=granularity,
                                    currency=currency,
                                    locale=locale,
                                    offerId=offerId,
                                    region=region)
  
  return(df_cost)
}