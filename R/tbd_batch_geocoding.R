#' Internal Function for tbd_address_to_tract
#'
# \description{
#' Interal Function for Geocoding with the Batch Endpoint
#' Please visit the following link for more details:
#'  \href{https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form}{Census Tract Geocoding}
# }
#' @param df A dataframe
#' @examples
#' \dontrun{
#' tbd_batch_geocoding(df = data frame)
#'   }
#' @export
tbd_batch_geocoding <- function(df){
  # Write a Temporary CSV
  tmp <- tempfile(fileext = '.csv')
  utils::write.table(df, tmp, col.names = FALSE, row.names = FALSE,
                     na = '', sep = ',')

  url <- paste0('https://geocoding.geo.census.gov/geocoder/geographies/addressbatch')
  req <-
    httr::POST(url,
               body = list(
                 addressFile = httr::upload_file(tmp),
                 benchmark = "Public_AR_Current",
                 vintage = "Current_Current",
                 format = 'json'
               ),
               encode = 'multipart'
    )
  cnt <- httr::content(req, as = 'text', encoding = 'UTF-8')

  if(grepl('<p>', cnt)){
    stop('API Failed Unexpectedly, Did you supply an Invalid Benchmark or Vintage?')
  }

  cols<- c('id', 'input_address', 'status', 'quality', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side', 'state_id', 'county_id', 'tract_id', 'block_id')
  df <- utils::read.csv(text = cnt, header = FALSE,
                        col.names = cols,
                        fill = TRUE, stringsAsFactors = FALSE,
                        na.strings = '')

  # Split Lon/Lat
  df$coords <- as.character(df$coords)
  lonlat <- strsplit(df$coords, split = ',')

  df$lon <- vapply(lonlat,function(x){x[1]}, 'numeric')
  df$lat <- vapply(lonlat,function(x){x[2]}, 'numeric')

  return(df)
}
