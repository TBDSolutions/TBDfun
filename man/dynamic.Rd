\name{dynamic}
\alias{dynamic}
\title{Map Function}
\usage{
dynamic_map(map_type = "",
        df = data frame,
        pihp_fill = filtered pihp dataset,
        cmh_fill = filtered cmhsp dataset,
        col_pallet = " ",
        addtiles = " ",
        border_col = " ",
        legend_label =" ")
}
\description{
map_type: Basic map with county or pihp or cmhsp boundaries is drawn based on the map_type argument, it should be given in double cotations.

df: The data frame must contain a name column (county or pihp or cmhsp names) and summary column i.e.. summarized number.

pihp_fill: A pihp filtered dataset must be provided to filter the selected PIHP and highlight the boundaries of the selected PIHP.

cmh_fill: A cmhsp filtered dataset must be provided to filter the selected CMHSP and highlight the boundaries of the selected CMHSP.

col_pallet: Choose a color pellet of any choise to populate the summarized data, It should be given in double cotations.

addtiles: Add a tile layer from a known map provider, it should be given in double cotations.

border_col: Fill border color, it should be given in double cotations.

legend_label: Label of the ledgend, it should be given in double cotations.
}
\examples{
dynamic_map(map_type = input$group,
            df = select_df,
            pihp_fill = pihp_deaths_filt,
            cmh_fill = cmh_deaths_filt,
            col_pallet = "viridis",
            addtiles = "Stamen.TonerLite",
            border_col = "white",
            legend_label ="range")
}
