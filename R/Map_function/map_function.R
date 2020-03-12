## Function

pro_map <- function(map_type, df, col_pallet, addtiles, border_col, legend_label) {
  
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
  
  county <- st_read("data/county.shp")
  tract <- st_read("data/tract.shp")
  
  pihp = aggregate(x = county[, "estimate"], 
                   by = list(county$PIHP),
                   FUN = sum, na.rm = TRUE)
  
  cmhsp = aggregate(x = county[, "estimate"], 
                    by = list(county$CMHSP),
                    FUN = sum, na.rm = TRUE)
  
  col_dist <- colorBin(c(col_pallet),df$summary, reverse = T, na.color = "grey")
  
  map <- if(map_type == "county"){
    
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
    
  } else if(map_type == "pihp"){
    
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
  else if(map_type == "cmhsp"){
    
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
  else if(map_type == "tract"){
    
    map <- leaflet(tract_acs) %>% 
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

 # 1. The first argument is  the type of the map for example (county, pihp, cmhsp, tract), should be given in all lower case. 
 # 2. The second argument is the dataset, the dataset should consist of the name column i.e.. county, pihp, cmhsp or census tract name and a summary column to fill the color over the map. 
 # 3. The 3rd argument is the color pellet name. 
 # 4. The 4th argument is the tiles name. 
 # 5. The 5th argument is the color of the boarder color
 # 6. The 6th argument is the legend label to display over the legend.
 

pro_map(map_type = "county", 
        df = county_deaths, 
        col_pallet = "viridis", 
        addtiles = "Stamen.TonerLite", 
        border_col = "red", 
        legend_label ="range")