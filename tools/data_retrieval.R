library(magrittr);library(tidyverse)
library(xts);library(car)
library(DT);library(scales);library(plotly);library(dygraphs)
library(RColorBrewer)
library(viridis); library(maps); library(tigris); library(leaflet)
library(tidycensus)
library(stringr)
library(sf)
library(rmapshaper)

#Download the MI geography dataset
mi_master_polygons <- get_acs(geography = "tract",
                       variables = "B01003_001",
                       state = "MI",
                       geometry = TRUE)

mi_master_polygons$county<-str_extract(mi_master_polygons$NAME, "(?<=,)[^,]*(?=,)")
mi_master_polygons$county<-gsub(" County","",mi_master_polygons$county)
mi_master_polygons$county<-trimws(mi_master_polygons$county)
mi_master_polygons$NAME<-gsub("St. Clair","Saint Clair",mi_master_polygons$NAME)
mi_master_polygons$NAME<-gsub("St. Joseph","Saint Joseph",mi_master_polygons$NAME)
mi_master_polygons$county<-gsub("St. Clair","Saint Clair",mi_master_polygons$county)
mi_master_polygons$county<-gsub("St. Joseph","Saint Joseph",mi_master_polygons$county)
#######
mi_master_polygons$PIHP<-NA
mi_master_polygons[mi_master_polygons$county %in% c("Lenawee","Livingston","Monroe","Washtenaw"),]$PIHP <- 'CMHPSM'
mi_master_polygons[mi_master_polygons$county %in% c("Wayne"),]$PIHP <- 'DWMHA'
mi_master_polygons[mi_master_polygons$county %in% c("Oakland"),]$PIHP <- 'OCCMHA'
mi_master_polygons[mi_master_polygons$county %in% c("Allegan", "Kent","Lake", "Mason","Muskegon", "Oceana","Ottawa"),]$PIHP <- 'LRE'
mi_master_polygons[mi_master_polygons$county %in% c("Macomb"),]$PIHP <- 'MCMHS'
mi_master_polygons[mi_master_polygons$county %in% c("Arenac","Bay", "Clare","Clinton","Eaton","Gladwin", "Gratiot","Hillsdale",
                                      "Huron","Ingham", "Ionia","Isabella","Jackson","Mecosta", "Midland","Montcalm",
                                      "Newaygo","Osceola","Saginaw","Shiawassee","Tuscola"),]$PIHP <- 'MSHN'
mi_master_polygons[mi_master_polygons$county %in% c("Alcona","Alpena", "Antrim","Benzie", "Charlevoix","Cheboygan","Crawford","Emmet",
                                      "Grand Traverse","Iosco","Kalkaska","Leelanau","Manistee","Missaukee","Montmorency","Ogemaw",
                                      "Oscoda","Otsego","Presque Isle","Roscommon","Wexford"),]$PIHP <- 'NMRE'
mi_master_polygons[mi_master_polygons$county %in% c("Alger","Baraga","Chippewa","Delta", "Dickinson","Gogebic","Houghton","Iron",
                                      "Keweenaw","Luce","Mackinac","Marquette","Menominee","Ontonagon","Schoolcraft"),]$PIHP <- 'Northcare'
mi_master_polygons[mi_master_polygons$county %in% c("Genesee","Lapeer","Sanilac","Saint Clair"),]$PIHP <- 'Region10'
mi_master_polygons[mi_master_polygons$county %in% c("Barry","Berrien","Branch","Calhoun","Cass","Kalamazoo","Van Buren","Saint Joseph"),]$PIHP <- 'SWMBH'

mi_master_polygons$CMHSP<-NA
mi_master_polygons[mi_master_polygons$county %in% c("Allegan"),]$CMHSP <- 'Allegan'
mi_master_polygons[mi_master_polygons$county %in% c("Iosco","Ogemaw","Oscoda"),]$CMHSP <- 'AuSable Valley'
mi_master_polygons[mi_master_polygons$county %in% c("Barry"),]$CMHSP <- 'Barry'
mi_master_polygons[mi_master_polygons$county %in% c("Oakland"),]$CMHSP <- 'Oakland'
mi_master_polygons[mi_master_polygons$county %in% c("Saint Clair"),]$CMHSP <- 'Saint Clair'
mi_master_polygons[mi_master_polygons$county %in% c("Saint Joseph"),]$CMHSP <- 'Saint Joseph'
mi_master_polygons[mi_master_polygons$county %in% c("Arenac","Bay"),]$CMHSP <- 'Bay-Arenac'
mi_master_polygons[mi_master_polygons$county %in% c("Berrien"),]$CMHSP <- 'Berrien'
mi_master_polygons[mi_master_polygons$county %in% c("Clinton","Eaton","Ingham"),]$CMHSP <- 'CEI'
mi_master_polygons[mi_master_polygons$county %in% c("Clare","Gladwin", "Isabella","Mecosta","Midland","Osceola"),]$CMHSP <- 'Central Michigan'
mi_master_polygons[mi_master_polygons$county %in% c("Baraga","Houghton","Keweenaw","Ontonagon"),]$CMHSP <- 'Copper Country'
mi_master_polygons[mi_master_polygons$county %in% c("Wayne"),]$CMHSP <- 'Detroit-Wayne'
mi_master_polygons[mi_master_polygons$county %in% c("Genesee"),]$CMHSP <- 'Genesee'
mi_master_polygons[mi_master_polygons$county %in% c("Gogebic"),]$CMHSP <- 'Gogebic'
mi_master_polygons[mi_master_polygons$county %in% c("Gratiot"),]$CMHSP <- 'Gratiot'
mi_master_polygons[mi_master_polygons$county %in% c("Chippewa","Mackinac","Schoolcraft"),]$CMHSP <- 'Hiawatha'
mi_master_polygons[mi_master_polygons$county %in% c("Huron"),]$CMHSP <- 'Huron'
mi_master_polygons[mi_master_polygons$county %in% c("Ionia"),]$CMHSP <- 'Ionia'
mi_master_polygons[mi_master_polygons$county %in% c("Kalamazoo"),]$CMHSP <- 'Kalamazoo'
mi_master_polygons[mi_master_polygons$county %in% c("Lapeer"),]$CMHSP <- 'Lapeer'
mi_master_polygons[mi_master_polygons$county %in% c("Lenawee"),]$CMHSP <- 'Lenawee'
mi_master_polygons[mi_master_polygons$county %in% c("Hillsdale","Jackson"),]$CMHSP <- 'Lifeways'
mi_master_polygons[mi_master_polygons$county %in% c("Livingston"),]$CMHSP <- 'Livingston'
mi_master_polygons[mi_master_polygons$county %in% c("Macomb"),]$CMHSP <- 'Macomb'
mi_master_polygons[mi_master_polygons$county %in% c("Benzie","Manistee"),]$CMHSP <- 'Manistee-Benzie'
mi_master_polygons[mi_master_polygons$county %in% c("Monroe"),]$CMHSP <- 'Monroe'
mi_master_polygons[mi_master_polygons$county %in% c("Montcalm"),]$CMHSP <- 'Montcalm'
mi_master_polygons[mi_master_polygons$county %in% c("Muskegon"),]$CMHSP <- 'Muskegon'
mi_master_polygons[mi_master_polygons$county %in% c("Kent"),]$CMHSP <- 'Network180'
mi_master_polygons[mi_master_polygons$county %in% c("Newaygo"),]$CMHSP <- 'Newaygo'
mi_master_polygons[mi_master_polygons$county %in% c("Antrim","Charlevoix","Cheboygan","Emmet","Kalkaska","Otsego"),]$CMHSP <- 'North Country'
mi_master_polygons[mi_master_polygons$county %in% c("Alcona","Alpena","Montmorency","Presque Isle"),]$CMHSP <- 'Northeast Michigan'
mi_master_polygons[mi_master_polygons$county %in% c("Crawford","Grand Traverse","Leelanau", "Missaukee","Roscommon","Wexford"),]$CMHSP <- 'Northern Lakes'
mi_master_polygons[mi_master_polygons$county %in% c("Dickinson","Iron","Menominee"),]$CMHSP <- 'Northpointe'
mi_master_polygons[mi_master_polygons$county %in% c("Ottawa"),]$CMHSP <- 'Ottawa'
mi_master_polygons[mi_master_polygons$county %in% c("Alger","Delta","Luce","Marquette"),]$CMHSP <- 'Pathways'
mi_master_polygons[mi_master_polygons$county %in% c("Branch"),]$CMHSP <- 'Pines'
mi_master_polygons[mi_master_polygons$county %in% c("Saginaw"),]$CMHSP <- 'Saginaw'
mi_master_polygons[mi_master_polygons$county %in% c("Sanilac"),]$CMHSP <- 'Sanilac'
mi_master_polygons[mi_master_polygons$county %in% c("Shiawassee"),]$CMHSP <- 'Shiawassee'
mi_master_polygons[mi_master_polygons$county %in% c("Calhoun"),]$CMHSP <- 'Summit Pointe'
mi_master_polygons[mi_master_polygons$county %in% c("Tuscola"),]$CMHSP <- 'Tuscola'
mi_master_polygons[mi_master_polygons$county %in% c("Van Buren"),]$CMHSP <- 'Van Buren'
mi_master_polygons[mi_master_polygons$county %in% c("Cass"),]$CMHSP <- 'Woodlands'
mi_master_polygons[mi_master_polygons$county %in% c("Lake","Mason","Oceana"),]$CMHSP <- 'West Michigan'
mi_master_polygons[mi_master_polygons$county %in% c("Washtenaw"),]$CMHSP <- 'Washtenaw'

mi_master_polygons$agency<-NA
mi_master_polygons[mi_master_polygons$county %in% c("Wayne"),]$agency <- 'Region 1A & 1C'
mi_master_polygons[mi_master_polygons$county %in% c("Livingston","Macomb","Monroe","Oakland","Saint Clair","Washtenaw"),]$agency <- 'Region 1B'
mi_master_polygons[mi_master_polygons$county %in% c("Jackson", "Hillsdale", "Lenawee"),]$agency <- 'Region 2'
mi_master_polygons[mi_master_polygons$county %in% c("Barry", "Branch", "Calhoun", "Kalamazoo", "Saint Joseph"),]$agency <- 'Region 3'
mi_master_polygons[mi_master_polygons$county %in% c("Berrien", "Cass", "Van Buren"),]$agency <- 'Region 4'
mi_master_polygons[mi_master_polygons$county %in% c("Genesee", "Lapeer", "Shiawassee"),]$agency <- 'Region 5'
mi_master_polygons[mi_master_polygons$county %in% c("Clinton", "Eaton", "Ingham"),]$agency <- 'Region 6'
mi_master_polygons[mi_master_polygons$county %in% c("Bay", "Clare", "Gladwin", "Gratiot", "Huron", "Isabella", "Midland", "Saginaw", "Sanilac", "Tuscola"),]$agency <- 'Region 7'
mi_master_polygons[mi_master_polygons$county %in% c("Allegan", "Ionia", "Kent", "Lake", "Mason", "Mecosta", "Montcalm", "Newaygo", "Osceola"),]$agency <- 'Region 8'
mi_master_polygons[mi_master_polygons$county %in% c("Alcona", "Arenac", "Alpena", "Cheboygan", "Crawford", "Iosco", "Montmorency", "Ogemaw", "Oscoda", "Otsego", "Presque Isle", "Roscommon"),]$agency <- 'Region 9'
mi_master_polygons[mi_master_polygons$county %in% c("Antrim", "Benzie", "Charlevoix", "Emmet", "Grand Traverse", "Kalkaska", "Leelanau", "Manistee", "Missaukee", "Wexford" ),]$agency <- 'Region 10'
mi_master_polygons[mi_master_polygons$county %in% c("Alger", "Baraga", "Chippewa", "Delta", "Dickinson", "Gogebic", "Houghton", "Iron", "Keweenaw", "Luce", "Mackinac", "Marquette", "Menominee", "Ontonagon", "Schoolcraft"),]$agency <- 'Region 11'
mi_master_polygons[mi_master_polygons$county %in% c("Muskegon", "Oceana", "Ottawa"),]$agency <- 'Region 14'

###county shape file ##
county <- mi_master_polygons %>%
  group_by(county) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE))
county <- ms_simplify(county)

###pihp shape file ##
pihp <- mi_master_polygons %>%
  group_by(PIHP) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE))
pihp <- ms_simplify(pihp)

###cmhsp shape file ##
cmhsp <- mi_master_polygons %>%
  group_by(CMHSP) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE))
cmhsp <- ms_simplify(cmhsp)

###tract shape file ##
tract <- mi_master_polygons %>%
  group_by(NAME) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE))

###agency shape file ##
agency <- mi_master_polygons %>%
  group_by(agency) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE))
agency <- ms_simplify(agency)

st_write(mi_master_polygons,"../mi_master_polygons.shp")
