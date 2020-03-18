\name{static}
\alias{static}
\title{Map Function}
\usage{
static_map(map_type = " ",
           df = data frame,
           col_pallet = " ",
           addtiles = " ",
           border_col = " ",
           legend_label =" ")
}
\description{
map_type : Basic map with county or PIHP or CMHSP boundaries is drawn based on the map_type argument, Valid values are county, pihp, cmhsp, tract.

df : The data frame must contain a name column (county or PIHP or CMHSP names) and summary column i.e... summarized number.

col_palette : Choose a color palette of any choice to populate the summarized data.

add tiles : Add a tile layer from a known map provide, it will give the bacground map.

border_col : Fill border color.

legend_label : Label of the legend.
}
\examples{
static_map(map_type = "county",
           df = county_deaths,
           col_pallet = "viridis",
           addtiles = "Stamen.TonerLite",
           border_col = "white",
           legend_label ="range")
}
