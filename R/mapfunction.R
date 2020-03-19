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

  county <- st_read("data/mi_polygons.shp")
  tract <- st_read("data/tract.shp")

  pihp = aggregate(x = county[, "estimate"],
                   by = list(county$PIHP),
                   FUN = sum, na.rm = TRUE)

  pihp_fil <- pihp %>% filter(Group.1 == pihp_filter$name)

  cmhsp = aggregate(x = county[, "estimate"],
                    by = list(county$CMHSP),
                    FUN = sum, na.rm = TRUE)

  cmh_fil <- cmhsp %>% filter(Group.1 == cmh_filter$name)

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
                         na.label = "red")

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
                         na.label = "red")

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
                         na.label = "red")

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
                         na.label = "red")

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

  county_reference<-read.csv("data/county_reference.csv")
  tract_reference<-read.csv("data/tract_reference.csv")

  if(names(df) == "countyid"){
    df<-df %>%
      inner_join(county_reference, by = c("countyid" = "GEOID"))%>%
      rename(name = NAME)
  }

  if(names(df) == "tractid"){
    df<-df %>%
      inner_join(tract_reference, by = c("tractid" = "GEOID"))%>%
      rename(name = NAME)
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

  county <- st_read("data/mi_polygons.shp")
  tract <- st_read("data/tract.shp")

  pihp = aggregate(x = county[, "estimate"],
                   by = list(county$PIHP),
                   FUN = sum, na.rm = TRUE)

  cmhsp = aggregate(x = county[, "estimate"],
                    by = list(county$CMHSP),
                    FUN = sum, na.rm = TRUE)

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
                         na.label = "red")

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
                         na.label = "red")

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
                         na.label = "red")

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
                         na.label = "red")

    print(map)
  }

  return(map)
}
