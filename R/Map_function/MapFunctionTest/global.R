library(tidyverse)
library(sf)
install.packages("data/mapfun_0.2.0.tar.gz", repos = NULL, type = "source")
library(mapfun)
data404<-read_csv("data/404Data.csv")%>%
                       rename(
                         pih_numb = pihp,
                         pihp = pihp_name)

