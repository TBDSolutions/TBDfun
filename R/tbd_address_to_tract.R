#' Address to Tract
#'
# \description{
#' Provides access to the US Census Bureau batch endpoints for locations and geographies.
#' The function has been developed specifically with large data sets in mind. only unique
#' addresses are passed to the API for geocoding. If a data set exceeds 1,000 unique
#' addresses, it will be automatically subset into appropriately sized API calls,
#' geocoded, and then put back together so that a single object is returned.
#' The function implements iteration and optional parallelization in order to geocode datasets
#' larger than the API limit of 10,000 and more efficiently than sending 10,000 per request.
#' The function therefore provides an efficient solution to batch geocoding via the Census Bureau’s services.
#' This implementation assumes that your data are contained in a data.frame or tibble, and that address
#' data are split into a number of component variables: street address, city, state, and five digit zip code.
#' file to the census tract API. Please visit the following link for more details:
#'  \href{https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form}{Census Tract Geocoding}
# }
#' @param data A dataframe
#' @param id Unique ID
#' @param street A column that consist of house number and street address
#' @param city City column
#' @param state State column
#' @param zip Zip column
#' @examples
#' \dontrun{
#' tbd_address_to_tract(data = data frame,
#'                      id = id,
#'                      street = street,
#'                      city = city,
#'                      state = state,
#'                      zip = zip_code)
#'   }
#' @import httr
#' @import tidyverse
#' @import devtools
#' @import RCurl
#' @import urltools
#' @import svDialogs
#' @import utils
#' @import parallel
#' @export

tbd_address_to_tract <- function(data, id = NULL, street, city = NULL, state = NULL, zip = NULL){

  parallel = 1

  # Check Specification of Arguments
  if(missing(data) | missing(street)){
    stop('`data` and `street` are required arguments')
  }
  if(!is.null(id) && any(duplicated(data[[id]]))){
    stop('Rows in the `id` column are not unique')
  }

  # Warn for Omission
  if(is.null(city) | is.null(state) | is.null(zip)){
    warning('Omission of `city`, `state` or `zip` greatly reduces the speed and accuracy of the geocoder.')
  }

  # Check Parallel Configuration
  if(parallel > 1){
    # Check Number of Cores
    avail_cores <- parallel::detectCores()
    if(parallel > avail_cores){
      warning('More cores specified than are available, using ', avail_cores, ' cores instead')
      core_count <- avail_cores
    }else{
      core_count <- parallel
    }
  }

  # Handle NA Arguments
  n <- nrow(data)

  if(!is.null(id)){
    if(!id %in% names(data)){
      stop(id, ' is not a defined column name in the data.frame')
    }
    # Need to Sort User Data for Later Column Binding
    data <- data[order(data[[id]]),]
    id <- data[[id]]
  }else{
    id <- seq(n)
  }

  if(!street %in% names(data)){
    stop(street, ' is not a defined column name in the data.frame')
  }

  if(!is.null(city)){
    if(!city %in% names(data)){
      stop(city, ' is not a defined column name in the data.frame')
    }
    city <- data[[city]]
  }else{
    city <- rep_len(NA, n)
  }

  if(!is.null(state)){
    if(!state %in% names(data)){
      stop(state, ' is not a defined column name in the data.frame')
    }
    state <- data[[state]]
  }else{
    state <- rep_len(NA, n)
  }

  if(!is.null(zip)){
    if(!zip %in% names(data)){
      stop(zip, ' is not a defined column name in the data.frame')
    }
    zip <- data[[zip]]
  }else{
    zip <- rep_len(NA, n)
  }

  # Build a Data.frame
  df <- data.frame(
    id = id,
    street = data[[street]],
    city = city,
    state = state,
    zip = zip,
    stringsAsFactors = FALSE
  )

  # Extract unique addresses
  uniq <- df[which(!duplicated(paste(df$street, df$city, df$state, df$zip))),]


  if(parallel > 1){
    # Split by Core Count, Maximizing the Size of Batches
    # Calculate Split Factor (Halve Batch Sizes until appropriately under threshold)
    splt_fac <- core_count
    while (nrow(uniq) / splt_fac > 1000) {
      splt_fac <- 2 * splt_fac
    }

    batches <- split(uniq, rep_len(seq(splt_fac), nrow(uniq)) )

    results <- lapply(batches, tbd_batch_geocoding,
                                  mc.cores = core_count)

  }else{ # Non Parallel
    # Split and Iterate
    batches <- split(uniq, (seq(nrow(uniq))-1) %/% 1000 )
    results <- lapply(batches, tbd_batch_geocoding)

  }

  # Row Bind the List of Responses
  api_output <- do.call(rbind, results)

  # Join Results with unique
  uniq_join <- merge(uniq, api_output, by = 'id' , all.x = TRUE, sort = TRUE)

  # Join Uniq with original df and sort
  all_join <- merge(df, uniq_join, by = c('street', 'city', 'state', 'zip'), all.x = TRUE)
  all_join <- all_join[order(all_join$id.x),]

  # Coerce to Numeric Cooridates
  all_join$lat <- as.numeric(all_join$lat)
  all_join$lon <- as.numeric(all_join$lon)

  all_join <- all_join %>%
    select('id.x', 'input_address', 'status', 'quality', 'matched_address','lat','lon', 'tiger_line_id', 'tiger_side', 'state_id', 'county_id', 'tract_id', 'block_id') %>%
    rename(id = id.x)

  geocoded_address_to_tract <<- all_join


}
