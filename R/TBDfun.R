library(magrittr);library(tidyverse);library(viridis); library(maps); library(tigris); library(leaflet)
library(tidycensus);library(stringr);library(sf)

#Basic map with county or PIHP or CMHSP boundaries is drawn based on the map_type argument, Valid values are county, pihp, cmhsp, tract.
dynamic_map <- function(map_type,
                        df,
                        pihp_filter,
                        cmh_filter,
                        col_pallet = "viridis",
                        addtiles = "Stamen.TonerLite",
                        border_col = "white",
                        bins = c(0,1,3,5,10,15,20,25,30,35,45,50),
                        legend_label = "range") {

  mi_master_polygons <- st_read(system.file("/data//mi_master_polygons.shp", package = "TBDfun"))

  ###county shape file ##
  county <- mi_master_polygons %>%
    group_by(county) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  ###pihp shape file ##
  pihp <- mi_master_polygons %>%
    group_by(PIHP) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  ###cmhsp shape file ##
  cmhsp <- mi_master_polygons %>%
    group_by(CMHSP) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  ###tract shape file ##
  tract <- mi_master_polygons %>%
    group_by(NAME) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  pihp_fil <- pihp %>% filter(PIHP == pihp_filter$name)

  cmh_fil <- cmhsp %>% filter(CMHSP == cmh_filter$name)

  county_reference<-mi_master_polygons[,c(1,4,6)]
  county_reference$GEOID<-substr(county_reference$GEOID, 1, 5)
  county_reference<-county_reference %>%
    group_by(GEOID,county)%>%
    summarize(
      population = sum(estimate, na.rm = TRUE))
  county_reference$geometry<-NULL
  county_reference$population<-NULL
  tract_reference<-mi_master_polygons[,c(1,2)]
  tract_reference$geometry<-NULL

  if(names(df) == "countyid"){
    df<-df %>%
      inner_join(county_reference, by = c("countyid" = "GEOID"))%>%
      rename(name = county)
  }

  if(names(df) == "tractid"){
    df<-df %>%
      inner_join(tract_reference, by = c("tractid" = "GEOID"))%>%
      rename(name = county)
  }

  if(grepl("county", map_type, ignore.case = T)){

    df <- county_reference %>%
      left_join(df, by = c("county" = "name")) %>%
      rename(name = county)
    df$summary <- as.numeric(df$summary)
  }

  county_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s county</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)

  pihp_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s PIHP</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)

  cmhsp_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s CMHSP</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)

  tract_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s tract</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)

  pihp_label_filt <- sprintf(
    "<strong>%g </strong><br/><strong>%s PIHP</strong><br/>",
    pihp_filter$summary,pihp_filter$name) %>%
    lapply(htmltools::HTML)

  cmh_label_filt <- sprintf(
    "<strong>%g </strong><br/><strong>%s CMHSP</strong><br/>",
    cmh_filter$summary,cmh_filter$name) %>%
    lapply(htmltools::HTML)


  col_dist <- colorBin(c("viridis"),bins = bins,reverse = T, na.color = "grey")

  map <- if(grepl("County", map_type, ignore.case = T)){

    map <- leaflet(county) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = county_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)

  }

  else if(grepl("PIHP", map_type, ignore.case = T)){

    map <- leaflet(pihp) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = F),
        label = pihp_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addPolygons(
        data = pihp_fil,
        fillColor = ~col_dist(pihp_filter$summary),
        weight = 2,
        opacity = 1,
        color = "red",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = F),
        label = pihp_label_filt,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addPolygons(
        data = cmh_fil,
        fillColor = ~col_dist(cmh_filter$summary),
        weight = 2,
        opacity = 0.5,
        color = "blue",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = F),
        label = cmh_label_filt,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)
  }
  else if(grepl("CMHSP", map_type, ignore.case = T)){

    map <- leaflet(cmhsp) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = F),
        label = cmhsp_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addPolygons(
        data=cmh_fil,
        fillColor = ~col_dist(cmh_filter$summary),
        weight = 2,
        opacity = 1,
        color = "red",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = cmh_label_filt,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)
  }
  else if(grepl("tract", map_type, ignore.case = T)){

    map <- leaflet(tract) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = F),
        label = tract_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)
  }

  return(map)
}

## Static Function
#Basic map with county or PIHP or CMHSP boundaries is drawn based on the map_type argument, Valid values are county, pihp, cmhsp, tract.
static_map <- function(map_type, df,
                       col_pallet = "viridis",
                       addtiles = "Stamen.TonerLite",
                       border_col = "white",
                       legend_label = "range") {

  mi_master_polygons <- st_read(system.file("/data//mi_master_polygons.shp", package = "TBDfun"))

  ###county shape file ##
  county <- mi_master_polygons %>%
    group_by(county) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  ###pihp shape file ##
  pihp <- mi_master_polygons %>%
    group_by(PIHP) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  ###cmhsp shape file ##
  cmhsp <- mi_master_polygons %>%
    group_by(CMHSP) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  ###tract shape file ##
  tract <- mi_master_polygons %>%
    group_by(NAME) %>%
    summarize(
      population = sum(estimate, na.rm = TRUE))

  county_reference<-mi_master_polygons[,c(1,4,6)]
  county_reference$GEOID<-substr(county_reference$GEOID, 1, 5)
  county_reference<-county_reference %>%
    group_by(GEOID,county)%>%
    summarize(
      population = sum(estimate, na.rm = TRUE))
  county_reference$geometry<-NULL
  county_reference$population<-NULL
  tract_reference<-mi_master_polygons[,c(1,2)]
  tract_reference$geometry<-NULL

  if(names(df) == "countyid"){
    df<-df %>%
      inner_join(county_reference, by = c("countyid" = "GEOID"))%>%
      rename(name = county)
  }

  if(names(df) == "tractid"){
    df<-df %>%
      inner_join(tract_reference, by = c("tractid" = "GEOID"))%>%
      rename(name = NAME)
  }

  if(grepl("county", map_type, ignore.case = T)){

  df <- county_reference %>%
    left_join(df, by = c("county" = "name")) %>%
    rename(name = county)
    df$summary <- as.numeric(df$summary)
  }

  county_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s county</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)

  pihp_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s PIHP</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)

  cmhsp_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s CMHSP</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)

  tract_label <- sprintf(
    "<strong>%g </strong><br/><strong>%s tract</strong><br/>",
    df$summary,df$name) %>%
    lapply(htmltools::HTML)


  col_dist <- colorBin(c(col_pallet),df$summary, reverse = T, na.color = "grey")

  map <- if(grepl("county", map_type, ignore.case = T)){

    map <- leaflet(county) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = county_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)

  }
  else if(grepl("pihp", map_type, ignore.case = T)){

    map <- leaflet(pihp) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = F),
        label = pihp_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)
  }
  else if(grepl("cmhsp", map_type, ignore.case = T)){

    map <- leaflet(cmhsp) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = F),
        label = cmhsp_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)
  }
  else if(grepl("tract", map_type, ignore.case = T)){

    map <- leaflet(tract) %>%
      addTiles() %>%
      addProviderTiles(addtiles) %>%
      addPolygons(
        fillColor = ~col_dist(df$summary),
        weight = 2,
        opacity = 0.6,
        color = border_col,
        dashArray = "3",
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF5500",
          dashArray = "0.3",
          fillOpacity = 0.7,
          bringToFront = F),
        label = tract_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      leaflet::addLegend(pal = col_dist,
                         values = ~df$summary,
                         opacity = 0.7,
                         title = legend_label,
                         position = "bottomright",
                         na.label = "NA")

    print(map)
  }

  return(map)
}

census <- function(read_database,sql_query,write_database){
  library(httr)
  library(tidyverse)
  library(devtools)
  library(RCurl)
  library(urltools)
  library(DBI)
  library(odbc)
  library(RODBC)
  library(svDialogs)

  #Establish database connection
  connection <- DBI::dbConnect(odbc::odbc(),
                               Driver = "SQL Server",
                               Server = Sys.getenv("tbd_server_address"),
                               Database =  paste(read_database),
                               UID = Sys.getenv("tbd_server_uid"),
                               PWD = Sys.getenv("tbd_server_pw"),
                               Port = 1433)

  #Query to fetch the data from SQL
  Census_Tract<- DBI::dbGetQuery(connection, paste(sql_query))

  names(Census_Tract) <- NULL

  #store the data in a temp .csv file
  input <- tempfile(fileext = ".csv")
  write.csv(Census_Tract, input, row.names = FALSE)

  #Function to convert the input address to desiered output
  apiurl <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
  list_output <- POST(apiurl, body= list(addressFile = upload_file(input),
                                         benchmark = "Public_AR_Current",
                                         vintage = "Current_Current"
  ),
  encode="multipart"
  )

  cat(content(list_output, "text", encoding = "UTF-8"), "\n")

  #Storing the Encoded data in a file
  cat(content(list_output, "text", encoding = "UTF-8"), file="output.csv")

  #Converting the output to a data frame
  df_output<-read.csv(file ="output.csv",header = FALSE,
                      col.names = c("record_id_number"
                                    ,"input_address"
                                    ,"tiger_address_range_match_indicator"
                                    ,"tiger_match_type"
                                    ,"tiger_output_address"
                                    ,"longitude_latitude"
                                    ,"tigerline_id"
                                    ,"tigerline_id_side"
                                    ,"state_code"
                                    ,"county_code"
                                    ,"tract_code"
                                    ,"block_code"))

  #Removing the column headers from the output
  #df_output=subset(df_output,df_output$input_address!="street_address, city, state, zip")

  #store the output in a temp file
  output <- tempfile(fileext = ".csv")
  write.csv(df_output, output, row.names = FALSE)

  #Storing the formated output in the database
  dbWriteTable(conn = connection,
               name = paste(write_database),
               value = df_output,
               overwrite = T)
}
