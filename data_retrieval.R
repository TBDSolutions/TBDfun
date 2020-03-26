library(magrittr);library(tidyverse)
library(xts);library(car)
library(DT);library(scales);library(plotly);library(dygraphs)
library(RColorBrewer)
library(viridis); library(maps); library(tigris); library(leaflet)
library(tidycensus)
library(stringr)
library(sf)

#Download the MI geography dataset
mi_polygons <- get_acs(geography = "county",
                       variables = "B01003_001",
                       state = "MI",
                       geometry = TRUE)
mi_polygons$NAME<-gsub(" County, Michigan","",mi_polygons$NAME)

mi_polygons$PIHP<-NA
mi_polygons[mi_polygons$NAME %in% c("Lenawee","Livingston","Monroe","Washtenaw"),]$PIHP <- 'CMHPSM'
mi_polygons[mi_polygons$NAME %in% c("Wayne"),]$PIHP <- 'DWMHA'
mi_polygons[mi_polygons$NAME %in% c("Oakland"),]$PIHP <- 'OCCMHA'
mi_polygons[mi_polygons$NAME %in% c("Allegan", "Kent","Lake", "Mason","Muskegon", "Oceana","Ottawa"),]$PIHP <- 'LRE'
mi_polygons[mi_polygons$NAME %in% c("Macomb"),]$PIHP <- 'MCMHS'
mi_polygons[mi_polygons$NAME %in% c("Arenac","Bay", "Clare","Clinton","Eaton","Gladwin", "Gratiot","Hillsdale",
                                    "Huron","Ingham", "Ionia","Isabella","Jackson","Mecosta", "Midland","Montcalm",
                                    "Newaygo","Osceola","Saginaw","Shiawassee","Tuscola"),]$PIHP <- 'MSHN'
mi_polygons[mi_polygons$NAME %in% c("Alcona","Alpena", "Antrim","Benzie", "Charlevoix","Cheboygan","Crawford","Emmet",
                                    "Grand Traverse","Iosco","Kalkaska","Leelanau","Manistee","Missaukee","Montmorency","Ogemaw",
                                    "Oscoda","Otsego","Presque Isle","Roscommon","Wexford"),]$PIHP <- 'NMRE'
mi_polygons[mi_polygons$NAME %in% c("Alger","Baraga","Chippewa","Delta", "Dickinson","Gogebic","Houghton","Iron",
                                    "Keweenaw","Luce","Mackinac","Marquette","Menominee","Ontonagon","Schoolcraft"),]$PIHP <- 'Northcare'
mi_polygons[mi_polygons$NAME %in% c("Genesee","Lapeer","Sanilac","St. Clair"),]$PIHP <- 'Region10'
mi_polygons[mi_polygons$NAME %in% c("Barry","Berrien","Branch","Calhoun","Cass","Kalamazoo","Van Buren","St. Joseph"),]$PIHP <- 'SWMBH'

mi_polygons$CMHSP<-NA
mi_polygons[mi_polygons$NAME %in% c("Allegan"),]$CMHSP <- 'Allegan'
mi_polygons[mi_polygons$NAME %in% c("Iosco","Ogemaw","Oscoda"),]$CMHSP <- 'AuSable Valley'
mi_polygons[mi_polygons$NAME %in% c("Barry"),]$CMHSP <- 'Barry'
mi_polygons[mi_polygons$NAME %in% c("Oakland"),]$CMHSP <- 'Oakland'
mi_polygons[mi_polygons$NAME %in% c("St. Clair"),]$CMHSP <- 'Saint Clair'
mi_polygons[mi_polygons$NAME %in% c("St. Joseph"),]$CMHSP <- 'Saint Joseph'
mi_polygons[mi_polygons$NAME %in% c("Arenac","Bay"),]$CMHSP <- 'Bay-Arenac'
mi_polygons[mi_polygons$NAME %in% c("Berrien"),]$CMHSP <- 'Berrien'
mi_polygons[mi_polygons$NAME %in% c("Clinton","Eaton","Ingham"),]$CMHSP <- 'CEI'
mi_polygons[mi_polygons$NAME %in% c("Clare","Gladwin", "Isabella","Mecosta","Midland","Osceola"),]$CMHSP <- 'Central Michigan'
mi_polygons[mi_polygons$NAME %in% c("Baraga","Houghton","Keweenaw","Ontonagon"),]$CMHSP <- 'Copper Country'
mi_polygons[mi_polygons$NAME %in% c("Wayne"),]$CMHSP <- 'Detroit-Wayne'
mi_polygons[mi_polygons$NAME %in% c("Genesee"),]$CMHSP <- 'Genesee'
mi_polygons[mi_polygons$NAME %in% c("Gogebic"),]$CMHSP <- 'Gogebic'
mi_polygons[mi_polygons$NAME %in% c("Gratiot"),]$CMHSP <- 'Gratiot'
mi_polygons[mi_polygons$NAME %in% c("Chippewa","Mackinac","Schoolcraft"),]$CMHSP <- 'Hiawatha'
mi_polygons[mi_polygons$NAME %in% c("Huron"),]$CMHSP <- 'Huron'
mi_polygons[mi_polygons$NAME %in% c("Ionia"),]$CMHSP <- 'Ionia'
mi_polygons[mi_polygons$NAME %in% c("Kalamazoo"),]$CMHSP <- 'Kalamazoo'
mi_polygons[mi_polygons$NAME %in% c("Lapeer"),]$CMHSP <- 'Lapeer'
mi_polygons[mi_polygons$NAME %in% c("Lenawee"),]$CMHSP <- 'Lenawee'
mi_polygons[mi_polygons$NAME %in% c("Hillsdale","Jackson"),]$CMHSP <- 'Lifeways'
mi_polygons[mi_polygons$NAME %in% c("Livingston"),]$CMHSP <- 'Livingston'
mi_polygons[mi_polygons$NAME %in% c("Macomb"),]$CMHSP <- 'Macomb'
mi_polygons[mi_polygons$NAME %in% c("Benzie","Manistee"),]$CMHSP <- 'Manistee-Benzie'
mi_polygons[mi_polygons$NAME %in% c("Monroe"),]$CMHSP <- 'Monroe'
mi_polygons[mi_polygons$NAME %in% c("Montcalm"),]$CMHSP <- 'Montcalm'
mi_polygons[mi_polygons$NAME %in% c("Muskegon"),]$CMHSP <- 'Muskegon'
mi_polygons[mi_polygons$NAME %in% c("Kent"),]$CMHSP <- 'Network180'
mi_polygons[mi_polygons$NAME %in% c("Newaygo"),]$CMHSP <- 'Newaygo'
mi_polygons[mi_polygons$NAME %in% c("Antrim","Charlevoix","Cheboygan","Emmet","Kalkaska","Otsego"),]$CMHSP <- 'North Country'
mi_polygons[mi_polygons$NAME %in% c("Alcona","Alpena","Montmorency","Presque Isle"),]$CMHSP <- 'Northeast Michigan'
mi_polygons[mi_polygons$NAME %in% c("Crawford","Grand Traverse","Leelanau", "Missaukee","Roscommon","Wexford"),]$CMHSP <- 'Northern Lakes'
mi_polygons[mi_polygons$NAME %in% c("Dickinson","Iron","Menominee"),]$CMHSP <- 'Northpointe'
mi_polygons[mi_polygons$NAME %in% c("Ottawa"),]$CMHSP <- 'Ottawa'
mi_polygons[mi_polygons$NAME %in% c("Alger","Delta","Luce","Marquette"),]$CMHSP <- 'Pathways'
mi_polygons[mi_polygons$NAME %in% c("Branch"),]$CMHSP <- 'Pines'
mi_polygons[mi_polygons$NAME %in% c("Saginaw"),]$CMHSP <- 'Saginaw'
mi_polygons[mi_polygons$NAME %in% c("Sanilac"),]$CMHSP <- 'Sanilac'
mi_polygons[mi_polygons$NAME %in% c("Shiawassee"),]$CMHSP <- 'Shiawassee'
mi_polygons[mi_polygons$NAME %in% c("Calhoun"),]$CMHSP <- 'Summit Pointe'
mi_polygons[mi_polygons$NAME %in% c("Tuscola"),]$CMHSP <- 'Tuscola'
mi_polygons[mi_polygons$NAME %in% c("Van Buren"),]$CMHSP <- 'Van Buren'
mi_polygons[mi_polygons$NAME %in% c("Cass"),]$CMHSP <- 'Woodlands'
mi_polygons[mi_polygons$NAME %in% c("Lake","Mason","Oceana"),]$CMHSP <- 'West Michigan'
mi_polygons[mi_polygons$NAME %in% c("Washtenaw"),]$CMHSP <- 'Washtenaw'

st_write(mi_polygons, "data/mi_polygons.shp")

#Download the MI census tract geography dataset
tract <- get_acs(geography = "tract",
                       variables = "B01003_001",
                       state = "MI",
                       geometry = TRUE)

st_write(tract, "data/tract.shp")
