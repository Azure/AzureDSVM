#' @name AzureSM: azureConsumption
#' @title Get consumption of a subscription.
#' @param context - Azure Context Object.
#' @param dateStart - Start date timestamp.
#' @param dateEnd - End date timestamp.
#' @param granularity - Aggregation of granularity.
#' @param verbose - print tracing information (default FALSE)
#' @rdname azureConsumption
#' @export

azureConsumption <- function(context,
                             dateStart,
                             dateEnd,
                             granularity,
                             verbose
) {
  require(lubridate)
  require(utils)

  azureCheckToken(context)

  # check the validity of input parameters

  if (!length(granularity)) GRA <- "Daily" else GRA <- granularity

  ds <- try(as.POSIXct(dateStart, format= "%Y-%m-%d %H:%M:%S"))
  de <- try(as.POSIXct(dateEnd, format= "%Y-%m-%d %H:%M:%S"))

  if(class(ds) == "try-error" || is.na(ds) || class(de) == "try-error" || is.na(de)) stop("Input date format should be YYYY-MM-DD HH:MM:SS.")

  dateStart <- as.POSIXct(dateStart)
  dateEnd <- as.POSIXct(dateEnd)

  if (dateStart >= dateEnd) stop("End date is earlier than start date!")

  if (GRA == "Daily") {

    # dateStart and dateEnd should be some day at midnight.

    hour(dateStart) <- 0
    minute(dateStart) <- 0
    second(dateStart) <- 0

    hour(dateEnd) <- 0
    minute(dateEnd) <- 0
    second(dateEnd) <- 0

  } else if (GRA == "Hourly") {

    # Resolution of dateStart and dateEnd should be hour.

    minute(dateStart) <- 0
    second(dateStart) <- 0

    minute(dateEnd) <- 0
    second(dateEnd) <- 0
  } else {
    stop("granularity should be either 'Daily' or 'Hourly'.")
  }

  START <- URLencode(paste(as.Date(dateStart, tz=Sys.timezone()), "T",
                           sprintf("%02d", hour(dateStart)), ":", sprintf("%02d", minute(dateStart)), ":", sprintf("%02d", second(dateStart)), "+",
                           "00:00",
                           sep=""),
                     reserved=TRUE)

  END <- URLencode(paste(as.Date(dateEnd, tz=Sys.timezone()), "T",
                         hour(dateEnd), hour(dateEnd), ":", minute(dateEnd), minute(dateEnd), ":", second(dateEnd), second(dateEnd), "+",
                         "00:00",
                         sep=""),
                   reserved=TRUE)

  URL <-
    sprintf("https://management.azure.com/subscriptions/%s/providers/Microsoft.Commerce/UsageAggregates?api-version=%s&reportedStartTime=%s&reportedEndTime=%s&aggregationgranularity=%s&showDetails=%s",
            SUBIDI,
            "2015-06-01-preview",
            START,
            END,
            GRA,
            "false"
    )

  r <- GET(URL,
           add_headers(.headers=c("Host"="management.azure.com", "Authorization"=sc$Token, "Content-Type"="application/json")),
           verbosity)

  if (r$status_code == 200) {

    # TODO. to check the output value of the df.

    rl <- content(r,"text",encoding="UTF-8")
    df <- fromJSON(rl)
  } else {
    warning(sprintf("Fail! The return code is %s", r$status_code))
  }

  df_use <-
    df$value$properties %>%
    select(-infoFields)

  # # don't know why data one day earlier than dateStart are also returned... let's try drop those results first.
  #
  # df_use %<>%
  #   filter(as.Date(usageStartTime) >= as.Date(START))

  # handle the maximum number of returned records (1000).

  if (max(as.POSIXct(df_use$usageEndTime)) < as.POSIXct(END)) {
    warning(sprintf("The number of records in the specified time period %s to %s exceeds the limit that can be returned from API call. Consumption information is truncated. Please use a small period instead.", START, END))
  }

  return(df_use)
}

#' @name azurePricing
#' @title Get pricing details of resources under a subscription.
#' @param context - Azure Context Object.
#' @param currency Currency in which price rating is measured.
#' @param locale Locality information of subscription.
#' @param offerId Offer ID of the subscription. Detailed information can be found at https://azure.microsoft.com/en-us/support/legal/offer-details/
#' @param region region information about the subscription.
#' @param verbose - print tracing information (default FALSE)
#' @rdname azurePricing
#' @export
azurePricing <- function(context,
                         currency,
                         locale,
                         offerId,
                         region,
                         verbose
) {
  azureCheckToken(context)

  if(missing(currency)) stop("Error: please provide currency information.")
  if(missing(locale)) stop("Error: please provide locale information.")
  if(missing(offerId)) stop("Error: please provide offer ID.")
  if(missing(region)) stop("Error: please provide region information.")
  verbosity <- if(verbose) httr::verbose(TRUE) else NULL


  url <- paste(
    "https://management.azure.com/subscriptions/", SUBIDI,
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

  r <- GET(url, add_headers(.headers=c(Authorization=AT, "Content-Type"="application/json")))

  rl <- fromJSON(content(r, "text", encoding="UTF-8"), simplifyDataFrame=TRUE)

  df_meter <- rl$Meters
  df_meter$MeterRate <- rl$Meters$MeterRates$`0`

  # an irresponsible drop of MeterRates and MeterTags. Will add them back after having a better handle of them.

  df_meter <- subset(df_meter, select=-MeterRates)
  df_meter <- subset(df_meter, select=-MeterTags)

  return(df_meter)
}
