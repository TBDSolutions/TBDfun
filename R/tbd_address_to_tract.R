#' Address to Tract
#'
# \description{
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
#' tbd_address_to_tract(
#'   df = Data frame
#'   )
#'   }
#' @import httr
#' @import tidyverse
#' @import devtools
#' @import RCurl
#' @import urltools
#' @import svDialogs
#' @import utils
#' @export

tbd_address_to_tract <- function(df){

  Census_Tract <- df
  names(Census_Tract) <- NULL

  #store the data in a temp .csv file
  input <- tempfile(fileext = ".csv")
  write.csv(Census_Tract, input, row.names = FALSE)

  #Function to convert the input address to desired output
  apiurl <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

  list_output <-
    POST(
      apiurl,
      body = list(
        addressFile = upload_file(input),
        benchmark = "Public_AR_Current", vintage = "Current_Current"
      ),
      encode="multipart"
    )

  #Storing the Encoded data in a file
  cat(content(list_output, "text", encoding = "UTF-8"), file="output.csv")

  #Converting the output to a data frame
  df_output <-
    read.csv(
      file ="output.csv", header = FALSE,
      col.names = c(
        "record_id_number","input_address","tiger_address_range_match_indicator"
        ,"tiger_match_type","tiger_output_address","longitude_latitude"
        ,"tigerline_id","tigerline_id_side","state_code","county_code"
        ,"tract_code","block_code"
      )
    )
  geocoded_address_to_tract <<- df_output
}
