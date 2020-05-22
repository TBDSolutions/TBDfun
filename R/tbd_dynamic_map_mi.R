#' Dynamic Map Function
#'
# \description{
#
#' df : The data frame must contain a name column (county or PIHP or CMHSP names) and summary column
#' i.e... summarized number. \cr \cr
#
#' map_type : Basic map with county or PIHP or CMHSP boundaries is drawn based on the map_type argument,
#'  Valid values are county, pihp, cmhsp, tract. \cr \cr
#
#' pihp_fill : A PIHP filtered dataset must be provided to filter the selected PIHP and highlight the
#' boundaries of the selected PIHP. \cr \cr
#
#' cmh_fill : A CMHSP filtered dataset must be provided to filter the selected CMHSP and highlight the
#' boundaries of the selected CMHSP. \cr \cr
#
#' col_palette : Choose a color palette of any choice to populate the summarized data. \cr \cr
#
#' add tiles : Add a tile layer from a known map provider. \cr \cr
#
#' border_col : Fill border color. \cr \cr
#
#' bins : Give the bins range in a collection. \cr \cr
#
#' legend_label : Label of the legend. \cr \cr
# }
#' @param df A dataframe
#' @param map_type A map type (Available map types are tract, county, cmhsp, pihp)
#' @param pihp_filter A PIHP filtered dataframe
#' @param cmh_filter A CMHSP filtered dataframe
#' @param col_pallet A color pelette
#' @param addtiles A provider tile
#' @param border_col Prefered border color
#' @param bins Prefered bin distribution
#' @param legend_label Prefered label of the legend
#' @examples
#' \dontrun{
#' tbd_dynamic_map_mi(
#'    df = select_df,
#'    map_type = input$group,
#'    pihp_filter = pihp_deaths_filt,
#'    cmh_fillter = cmh_deaths_filt,
#'    col_pallet = "viridis",
#'    addtiles = "Stamen.TonerLite",
#'    border_col = "white",
#'    bins = c(0,1,3,5,10,15,20,25,30,35,45,55),
#'    legend_label ="range")
#'   }
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

tbd_dynamic_map_mi <- function(df,
                        map_type,
                        pihp_filter,
                        cmh_filter,
                        col_pallet = "viridis",
                        addtiles = "Stamen.TonerLite",
                        border_col = "white",
                        bins = c(0,1,3,5,10,15,20,25,30,35,45,50),
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


  col_dist <- colorBin(c("viridis"),bins = bins,reverse = T, na.color = "grey")

      map <- if(grepl("County", map_type, ignore.case = T)){

          ###county shape file ##
             county <- mi_master_polygons %>%
               group_by(county) %>%
               summarize(
                 population = sum(estimate, na.rm = TRUE))
                 #county <- ms_simplify(county)

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

      else if(grepl("PIHP", map_type, ignore.case = T)){

          ###pihp shape file ##
              pihp <- mi_master_polygons %>%
                group_by(PIHP) %>%
                summarize(
                  population = sum(estimate, na.rm = TRUE))
              #pihp <- ms_simplify(pihp)

          ###cmhsp shape file ##
              cmhsp <- mi_master_polygons %>%
                group_by(CMHSP) %>%
                summarize(
                  population = sum(estimate, na.rm = TRUE))
                  #cmhsp <- ms_simplify(cmhsp)

          pihp_fil <- pihp %>% filter(PIHP == pihp_filter$name)

          cmh_fil <- cmhsp %>% filter(CMHSP == cmh_filter$name)

          pihp_label <- sprintf(
            "<strong>%g </strong><br/><strong>%s PIHP</strong><br/>",
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

        ###cmhsp shape file ##
        cmhsp <- mi_master_polygons %>%
          group_by(CMHSP) %>%
          summarize(
            population = sum(estimate, na.rm = TRUE))

        cmh_fil <- cmhsp %>% filter(CMHSP == cmh_filter$name)

        cmhsp_label <- sprintf(
          "<strong>%g </strong><br/><strong>%s CMHSP</strong><br/>",
          df$summary,df$name) %>%
          lapply(htmltools::HTML)

        cmh_label_filt <- sprintf(
          "<strong>%g </strong><br/><strong>%s CMHSP</strong><br/>",
          cmh_filter$summary,cmh_filter$name) %>%
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

        ###tract shape file ##
        tract <- mi_master_polygons %>%
          group_by(NAME) %>%
          summarize(
            population = sum(estimate, na.rm = TRUE))
        #tract <- ms_simplify(tract)

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

      return(map)

      }
