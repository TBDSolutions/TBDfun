#' Address to Tract
#'
# \description{
#' Provides access to the US Census Bureau batch endpoints for locations and geographies.
#' The function implements iteration and optional parallelization in order to geocode datasets larger than the API limit of 10,000 and more efficiently than sending 10,000 per request.
#' The function allows a dataframe as input and returns geocoded information using
#' census tract API. Put each field value in a separate column i.e.. unique ID, House Number and Street Name,
#' city , state, zipcode. City and State, or ZIP Code may be left blank
#' The dataframe may not exceed 10,000 records.
#' It may take several minutes to process this many addresses.
#' The function creates the dataframe as a .csv file in a temp location and passes the .csv
#' file to the census tract API. Please visit the following link for more details:
#'  \href{https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form}{Census Tract Geocoding}
# }
#' @param df A dataframe
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
#' @import rmapshaper
#' @export
# Interal Function for Geocoding with the Batch Endpoint

tbd_batch_geocoder <- function(df, return, timeout){

  if(requireNamespace("magrittr"))
    if(requireNamespace("tidyverse"))
      if(requireNamespace("viridis"))
        if(requireNamespace("maps"))
          if(requireNamespace("tigris"))
            if(requireNamespace("leaflet"))
              if(requireNamespace("tidycensus"))
                if(requireNamespace("stringr"))
                  if(requireNamespace("sf"))
                    if(requireNamespace("dplyr"))
                      if(requireNamespace("htmltools"))
                        if(requireNamespace("rmapshaper"))
                          if(requireNamespace("parallel"))
  # Write a Temporary CSV
  tmp <- tempfile(fileext = '.csv')
  utils::write.table(df, tmp, col.names = FALSE, row.names = FALSE,
                     na = '', sep = ',')
  url <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
  req <-
    POST(
      url,
      body = list(
        addressFile = upload_file(tmp),
        benchmark = "Public_AR_Current", vintage = "Current_Current"
      ),
      encode="multipart"
    )
  cnt <- httr::content(req, as = 'text', encoding = 'UTF-8')

  # Error if Benchmark/Vintage Invalid
  # Not Perfect, Error is Returned as HTML, could be other errors... (BAD API DESIGN!)
  # Could Cause Large Batches to FAIL if API Fails Unexpectedly
  if(grepl('<p>', cnt)){
    stop('API Failed Unexpectedly, Did you supply an Invalid Benchmark or Vintage?')
  }
  df <-
    read.csv(
      file ="output.csv", header = FALSE,
      col.names = c(
        "id","input_address","tiger_address_range_match_indicator"
        ,"tiger_match_type","tiger_output_address","longitude_latitude"
        ,"tigerline_id","tigerline_id_side","state_code","county_code"
        ,"tract_code","block_code"
      ))

  return(df)
}

#############

tbd_address_to_tract <- function(data, id = NULL, street, city = NULL, state = NULL, zip = NULL,timeout = 30, parallel = 1){

  if(requireNamespace("magrittr"))
    if(requireNamespace("tidyverse"))
      if(requireNamespace("viridis"))
        if(requireNamespace("maps"))
          if(requireNamespace("tigris"))
            if(requireNamespace("leaflet"))
              if(requireNamespace("tidycensus"))
                if(requireNamespace("stringr"))
                  if(requireNamespace("sf"))
                    if(requireNamespace("dplyr"))
                      if(requireNamespace("htmltools"))
                        if(requireNamespace("rmapshaper"))
                          if(requireNamespace("parallel"))

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
    # Check OS
    if(.Platform$OS.type != 'unix'){
      stop('Parallelization is only available on Unix Platforms')
    }
    # Check if Available
    if(!requireNamespace('parallel')){
      stop('Please install the `parallel` package to use parallel functionality')
    }
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
    street = street,
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

    results <- parallel::mclapply(batches, tbd_batch_geocoder, timeout,
                                  mc.cores = core_count)

  }else{ # Non Parallel
    # Split and Iterate
    batches <- split(uniq, (seq(nrow(uniq))-1) %/% 1000 )
    results <- lapply(batches, tbd_batch_geocoder, timeout)

  }

  # Row Bind the List of Responses
  api_output <- do.call(rbind, results)

  # Join Results with unique
  uniq_join <- merge(uniq, api_output, by = 'id' , all.x = TRUE, sort = TRUE)

  # Join Uniq with original df and sort
  all_join <- merge(df, uniq_join, by = c('street', 'city', 'state', 'zip'), all.x = TRUE)
  all_join <- all_join[order(all_join$id.x),]

  all_join <- all_join %>%
    select(id.x,input_address,tiger_address_range_match_indicator,
           tiger_match_type,tiger_output_address,longitude_latitude,tigerline_id,
           tigerline_id_side,state_code,county_code,tract_code,block_code) %>%
    rename(id = id.x)
  # Add tbd_ prefix to names
  #names(all_join) <- paste0('tbd_', names(all_join))

  geocoded_address_to_tract <<- all_join

}

