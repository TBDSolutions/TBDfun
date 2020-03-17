library(magrittr);library(tidyverse)
library(xts);library(car)
library(DT);library(scales);library(plotly);library(dygraphs)
library(RColorBrewer)
library(viridis); library(maps); library(tigris); library(leaflet)
library(tidycensus)
library(stringr)
library(sf)

# #Download the MI geography dataset 
# county <- get_acs(geography = "county",
#                        variables = "B01003_001",
#                        state = "MI",
#                        geometry = TRUE)
# 
# st_write(county, "data/county.shp")

#Download the MI census tract geography dataset 
tract <- get_acs(geography = "tract",
                       variables = "B01003_001",
                       state = "MI",
                       geometry = TRUE)

st_write(tract, "data/tract.shp")
