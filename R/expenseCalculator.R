#' @title Get data consumption of an Azure subscription for a time period between two time points. The granularity of the time can be either daily based or hourly based.
#' @note Formats of start time point and end time point follow ISO 8601 standard. Say if one would like to calculate data consumption between Feb 21, 2017 to Feb 25, 2017, the inputs should be "2017-02-21 00:00:00" and "2017-02-25 00:00:00", for start time point and end time point, respectively. For hourly based calculation, note there should be no minute and second included.
#' @param context AzureSMR context object.
#' @param instance Instance of Azure instance that one would like to check expense. No matter whether resource group is given or not, if a instance of instance is given, data consumption of that instance is returned.
#' @param timeStart Start time.
#' @param timeEnd End time.
#' @param granularity Aggregation granularity. Can be either "Daily" or "Hourly".
#' @export
dataConsumption <- function(context,
                            instance,
                            timeStart,
                            timeEnd,
                            granularity
) {
  # renew token if it expires.

  azureCheckToken(context)

  # preconditions here...

  if(missing(context) || !is.azureActiveContext(context))
    stop("Please specify a valid AzureSMR context.")

  if(missing(instance))
    stop("Please give instance name for retrieving records of data consumption.")

  if(missing(timeStart))
    stop("Please specify a starting time point in YYYY-MM-DD HH:MM:SS format.")

  if(missing(timeEnd))
    stop("Please specify an ending time point in YYYY-MM-DD HH:MM:SS format.")

  if(missing(granularity))
    stop("Please specify the granularity, either 'Daily' or 'Hourly', for daily-based aggregation or hourly aggregation, respectively.")

  # check the validity of input parameters

  if (!length(granularity)) GRA <- "Daily" else GRA <- granularity

  ds <- try(as.POSIXct(timeStart, format= "%Y-%m-%d %H:%M:%S", tz="UTC"))
  de <- try(as.POSIXct(timeEnd, format= "%Y-%m-%d %H:%M:%S", tz="UTC"))

  if(class(ds) == "try-error" || is.na(ds) || class(de) == "try-error" || is.na(de)) stop("Input date format should be YYYY-MM-DD HH:MM:SS.")

  timeStart <- as.POSIXct(timeStart)
  timeEnd <- as.POSIXct(timeEnd)

  if (timeStart >= timeEnd) stop("End time is no later than start time!")

  if (GRA == "Daily") {

    # timeStart and timeEnd should be some day at midnight.

    lubridate::hour(timeStart) <- 0
    lubridate::minute(timeStart) <- 0
    lubridate::second(timeStart) <- 0

    lubridate::hour(timeEnd) <- 0
    lubridate::minute(timeEnd) <- 0
    lubridate::second(timeEnd) <- 0

  } else if (GRA == "Hourly") {

    # Resolution of timeStart and timeEnd should be hour.

    lubridate::minute(timeStart) <- 0
    lubridate::second(timeStart) <- 0

    lubridate::minute(timeEnd) <- 0
    lubridate::second(timeEnd) <- 0

  } else {
    stop("granularity should be either 'Daily' or 'Hourly'.")
  }

  START <- URLencode(paste(as.Date(timeStart, tz=Sys.timezone()), "T",
                           sprintf("%02d", lubridate::hour(timeStart)), ":", sprintf("%02d", lubridate::minute(timeStart)), ":", sprintf("%02d", second(timeStart)), "+",
                           "00:00",
                           sep=""),
                     reserved=TRUE)

  END <- URLencode(paste(as.Date(timeEnd, tz=Sys.timezone()), "T",
                           sprintf("%02d", lubridate::hour(timeEnd)), ":", sprintf("%02d", lubridate::minute(timeEnd)), ":", sprintf("%02d", second(timeEnd)), "+",
                           "00:00",
                           sep=""),
                     reserved=TRUE)

  # END <- URLencode(paste(as.Date(timeEnd, tz=Sys.timezone()), "T",
  #                        hour(timeEnd), hour(timeEnd), ":", lubridate::minute(timeEnd), lubridate::minute(timeEnd), ":", second(timeEnd), second(timeEnd), "+",
  #                        "00:00",
  #                        sep=""),
  #                  reserved=TRUE)

  URL <-
    sprintf("https://management.azure.com/subscriptions/%s/providers/Microsoft.Commerce/UsageAggregates?api-version=%s&reportedStartTime=%s&reportedEndTime=%s&aggregationgranularity=%s&showDetails=%s",
            context$subscriptionID,
            "2015-06-01-preview",
            START,
            END,
            GRA,
            "false"
    )

  r <- GET(URL,
           add_headers(.headers=c("Host"="management.azure.com", "Authorization"=context$Token, "Content-Type"="application/json")))

  if (r$status_code == 200) {
    rl <- content(r,"text",encoding="UTF-8")
    df <- fromJSON(rl)
  } else {

    # for debug use.

    print(content(r, encoding="UTF-8"))

    stop(sprintf("Fail! The return code is %s", r$status_code))
  }

  df_use <-
    df$value$properties %>%
    select(-infoFields)

  inst_data <-
    df$value$properties$instanceData %>%
    lapply(., fromJSON)

  # retrieve results that match instance name.

  instance_detect <- function(inst_data) {
    return(basename(inst_data$Microsoft.Resources$resourceUri) == instance)
  }

  index_instance <- which(unlist(lapply(inst_data, instance_detect)))

  if(!missing(instance)) {
    if(length(index_instance) == 0)
      stop("No data consumption records found for the instance during the given period.")
    df_use <- df_use[index_instance, ]
  } else if(missing(instance)) {
    if(length(index_resource) == 0)
      stop("No data consumption records found for the resource group during the given period.")
    df_use <- df_use[index_resource, ]
  }

  # NOTE the maximum number of records returned from API is limited to 1000.

  if (nrow(df_use) == 1000 && max(as.POSIXct(df_use$usageEndTime)) < as.POSIXct(END)) {
    warning(sprintf("The number of records in the specified time period %s to %s exceeds the limit that can be returned from API call. Consumption information is truncated. Please use a small period instead.", START, END))
  }

  df_use %<>%
    select(usageStartTime,
           usageEndTime,
           meterName,
           meterCategory,
           meterSubCategory,
           unit,
           meterId,
           quantity,
           meterRegion) %>%
    mutate(usageStartTime=lubridate::ymd_h(usageStartTime)) %>%
    mutate(usageEndTime=lubridate::ymd_h(usageEndTime)) %>%

  return(df_use)
}

#' @title Get pricing details of resources under a subscription.
#' @param context - Azure Context Object.
#' @param currency Currency in which price rating is measured.
#' @param locale Locality information of subscription.
#' @param offerId Offer ID of the subscription. Detailed information can be found at https://azure.microsoft.com/en-us/support/legal/offer-details/
#' @param region region information about the subscription.
#' @export
pricingRates <- function(context,
                         currency,
                         locale,
                         offerId,
                         region
) {
  # renew token if it expires.

  azureCheckToken(context)

  # preconditions.

  if(missing(currency))
    stop("Error: please provide currency information.")

  if(missing(locale))
    stop("Error: please provide locale information.")

  if(missing(offerId))
    stop("Error: please provide offer ID.")

  if(missing(region))
    stop("Error: please provide region information.")

  url <- paste(
    "https://management.azure.com/subscriptions/", context$subscriptionID,
    "/providers/Microsoft.Commerce/RateCard?api-version=2016-08-31-preview&$filter=",
    "OfferDurableId eq '", offerId, "'",
    " and Currency eq '", currency, "'",
    " and Locale eq '", locale, "'",
    " and RegionInfo eq '", region, "'",
    sep="")

  url <- URLencode(url)

  # # for debug purpose.
  #
  # cat(url)

  r <- GET(url, add_headers(.headers=c(Authorization=context$Token, "Content-Type"="application/json")))

  rl <- fromJSON(content(r, "text", encoding="UTF-8"), simplifyDataFrame=TRUE)

  df_meter <- rl$Meters
  df_meter$MeterRate <- rl$Meters$MeterRates$`0`

  # an irresponsible drop of MeterRates and MeterTags. Will add them back after having a better handle of them.

  df_meter <- subset(df_meter, select=-MeterRates)
  df_meter <- subset(df_meter, select=-MeterTags)

  return(df_meter)
}

#' @title Calculate cost of using a specific instance of Azure for certain period.
#' @param context AzureSMR context.
#' @param instance Instance of Azure instance that one would like to check expense. No matter whether resource group is given or not, if a instance of instance is given, data consumption of that instance is returned.
#' @param timeStart Start time.
#' @param timeEnd End time.
#' @param granularity Aggregation granularity. Can be either "Daily" or "Hourly".
#' @param currency Currency in which price rating is measured.
#' @param locale Locality information of subscription.
#' @param offerId Offer ID of the subscription. Detailed information can be found at https://azure.microsoft.com/en-us/support/legal/offer-details/
#' @param region region information about the subscription.
#' @return Total cost measured in the given currency of the specified Azure instance in the period.
#' @export
expenseCalculator <- function(context,
                              instance,
                              timeStart,
                              timeEnd,
                              granularity,
                              currency,
                              locale,
                              offerId,
                              region) {
  df_use <-
    dataConsumption(context,
                    instance=instance,
                    timeStart=timeStart,
                    timeEnd=timeEnd,
                    granularity=granularity) %>%
    select(meterId,
           meterSubCategory,
           usageStartTime,
           usageEndTime,
           quantity)

  df_used_data <-
    group_by(df_use, meterId) %>%
    arrange(usageStartTime, usageEndTime) %>%
    summarise(usageStartDate=as.Date(min(usageStartTime), tz=Sys.timezone()),
              usageEndDate=as.Date(max(usageEndTime), tz=Sys.timezone()),
              totalQuantity=sum(quantity)) %>%
    ungroup()

  # use meterId to find pricing rates and then calculate total cost.

  df_rates <- pricingRates(context,
                           currency=currency,
                           locale=locale,
                           region=region,
                           offerId=offerId)

  meter_list <- df_used_data$meterId

  df_used_rates <-
    filter(df_rates, MeterId %in% meter_list) %>%
    rename(meterId=MeterId)

  df_cost <-
    left_join(df_used_data, df_used_rates) %>%
    mutate(Cost=totalQuantity * MeterRate) %>%
    select(-IncludedQuantity, -EffectiveDate, -MeterStatus, -usageStartDate, -usageEndDate) %>%
    na.omit()

  df_cost
}
