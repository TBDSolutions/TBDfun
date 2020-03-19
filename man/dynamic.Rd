\name{dynamic}
\alias{dynamic}
\title{Map Function}
\usage{
dynamic_map(map_type = "",
        df = data frame,
        pihp_filter = filtered pihp dataset,
        cmh_filter = filtered cmhsp dataset,
        col_pallet = " ",
        addtiles = " ",
        border_col = " ",
        bins = bins,
        legend_label =" ")
}
\description{
map_type : Basic map with county or PIHP or CMHSP boundaries is drawn based on the map_type argument, Valid values are county, pihp, cmhsp, tract.

df : The data frame must contain a name column (county or PIHP or CMHSP names) and summary column i.e... summarized number.

pihp_fill : A PIHP filtered dataset must be provided to filter the selected PIHP and highlight the boundaries of the selected PIHP.

cmh_fill : A CMHSP filtered dataset must be provided to filter the selected CMHSP and highlight the boundaries of the selected CMHSP.

col_palette : Choose a color palette of any choice to populate the summarized data.

add tiles : Add a tile layer from a known map provider.

border_col : Fill border color.

bins : Give the bins range in a collection.

legend_label : Label of the legend.
}
\examples{
dynamic_map(map_type = input$group,
            df = select_df,
            pihp_filter = pihp_deaths_filt,
            cmh_fillter = cmh_deaths_filt,
            col_pallet = "viridis",
            addtiles = "Stamen.TonerLite",
            border_col = "white",
            bins = c(0,1,3,5,10,15,20,25,30,35,45,55)
            legend_label ="range")
}
