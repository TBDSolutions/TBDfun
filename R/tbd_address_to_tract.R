#' Census Tract
#'
# \description{
#' df : A dataframe
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
#' @import DBI
#' @import odbc
#' @import RODBC
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
