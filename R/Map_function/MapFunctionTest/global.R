
data404<-read_csv("data/404Data.csv")%>%
                       rename(
                         pih_numb = pihp,
                         pihp = pihp_name)

