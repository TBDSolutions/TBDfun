#' Static Map Function
#'
# \description{
#
#' df : The data frame must contain a name column (county or PIHP or CMHSP names) and summary column
#' i.e... summarized number. \cr \cr
#
#' map_type : Basic map with county or PIHP or CMHSP boundaries is drawn based on the map_type argument,
#'  Valid values are county, pihp, cmhsp, tract. \cr \cr
#
#' col_palette : Choose a color palette of any choice to populate the summarized data. \cr \cr
#
#' add tiles : Add a tile layer from a known map provider. \cr \cr
#
#' border_col : Fill border color. \cr \cr
#
#' legend_label : Label of the legend. \cr \cr
# }
#' @param df A dataframe
#' @param map_type A map type (Available map types are tract, county, cmhsp, pihp)
#' @param col_pallet A color pelette
#' @param addtiles A provider tile
#' @param border_col Prefered border color
#' @param legend_label Prefered label of the legend
#' @examples
#' \dontrun{
#' tbd_static_map_mi(
#'    df = dataframe,
#'    map_type = county,
#'    col_pallet = "viridis",
#'    addtiles = "Stamen.TonerLite",
#'    border_col = "white",
#'    legend_label = "range")
#' }
#' @import magrittr
#' @import tidyverse
#' @import viridis
#' @import maps
#' @import tigris
#' @import leaflet
#' @import tidycensus
#' @import stringr
#' @import sf
#' @import dplyr
#' @import htmltools
#' @export
tbd_static_map_mi <- function(df,
                       map_type,
                       col_pallet = "viridis",
                       addtiles = "Stamen.TonerLite",
                       border_col = "white",
                       legend_label = "range") {

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

    mi_master_polygons <- st_read(system.file("extdata","mi_master_polygons.shp", package = "TBDfun"))

    col_dist <- colorBin(c(col_pallet),df$summary, reverse = T, na.color = "grey")

  map <- if(grepl("county", map_type, ignore.case = T)){

    county_reference<-mi_master_polygons[,c(1,4,6)]
    county_reference$GEOID<-substr(county_reference$GEOID, 1, 5)
    county_reference<-county_reference %>%
      group_by(GEOID,county)%>%
      summarize(
        population = sum(estimate, na.rm = TRUE))
    county_reference$geometry<-NULL
    county_reference$population<-NULL

    df <- county_reference %>%
      left_join(df, by = c("county" = "name")) %>%
      rename(name = county)
    df$summary <- as.numeric(df$summary)

    if(names(df) == "countyid"){
      df<-df %>%
        inner_join(county_reference, by = c("countyid" = "GEOID"))%>%
        rename(name = county)
    }

    ###county shape file ##
    county <- mi_master_polygons %>%
      group_by(county) %>%
      summarize(
        population = sum(estimate, na.rm = TRUE))
    county <- ms_simplify(county)

    county_label <- sprintf(
      "<strong>%g </strong><br/><strong>%s county</strong><br/>",
      df$summary,df$name) %>%
      lapply(htmltools::HTML)


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

    ###pihp shape file ##
    pihp <- mi_master_polygons %>%
      group_by(PIHP) %>%
      summarize(
        population = sum(estimate, na.rm = TRUE))
    pihp <- ms_simplify(pihp)

    pihp_label <- sprintf(
      "<strong>%g </strong><br/><strong>%s PIHP</strong><br/>",
      df$summary,df$name) %>%
      lapply(htmltools::HTML)

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

    ###cmhsp shape file ##
    cmhsp <- mi_master_polygons %>%
      group_by(CMHSP) %>%
      summarize(
        population = sum(estimate, na.rm = TRUE))
    cmhsp <- ms_simplify(cmhsp)

    cmhsp_label <- sprintf(
      "<strong>%g </strong><br/><strong>%s CMHSP</strong><br/>",
      df$summary,df$name) %>%
      lapply(htmltools::HTML)

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

    tract_reference<-mi_master_polygons[,c(1,2)]
    tract_reference$geometry<-NULL

    if(names(df) == "tractid"){
      df<-df %>%
        inner_join(tract_reference, by = c("tractid" = "GEOID"))%>%
        rename(name = NAME)
    }
    ###tract shape file ##
    tract <- mi_master_polygons %>%
      group_by(NAME) %>%
      summarize(
        population = sum(estimate, na.rm = TRUE))

    tract_label <- sprintf(
      "<strong>%g </strong><br/><strong>%s tract</strong><br/>",
      df$summary,df$name) %>%
      lapply(htmltools::HTML)

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

  else if(grepl("agency", map_type, ignore.case = T)){

    ###agency shape file ##
    agency <- mi_master_polygons %>%
      group_by(agency) %>%
      summarize(
        population = sum(estimate, na.rm = TRUE))
    agency <- ms_simplify(agency)

    agency_label <- sprintf(
      "<strong>%g </strong><br/><strong>%s agency</strong><br/>",
      df$summary,df$name) %>%
      lapply(htmltools::HTML)

    map <- leaflet(agency) %>%
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
        label = agency_label,
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
