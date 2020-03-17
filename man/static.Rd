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
map_type: Basic map with county or pihp or cmhsp boundaries is drawn based on the map_type argument, it should be given in double cotations.

df: The data frame must contain a name column (county or pihp or cmhsp names) and summary column i.e.. summarized number.

col_pallet: Choose a color pellet of any choise to populate the summarized data, It should be given in double cotations.

addtiles: Add a tile layer from a known map provider, it should be given in double cotations.

border_col: Fill border color, it should be given in double cotations.

legend_label: Label of the ledgend, it should be given in double cotations.
}
\examples{
static_map(map_type = "county",
           df = county_deaths,
           col_pallet = "viridis",
           addtiles = "Stamen.TonerLite",
           border_col = "white",
           legend_label ="range")
}
