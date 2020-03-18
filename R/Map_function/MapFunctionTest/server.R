#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
### Reactive Input 
org_input<-reactive(input$org)
metric_input<-reactive(input$metric)

    
### Reactive Dataset
data<-reactive({
    
    data404<-data404%>%
        group_by(!!as.symbol(org_input()))%>%
        summarise(summary = sum(!!as.symbol(metric_input()),na.rm = T))%>%
        ungroup()%>%
        rename(name = org_input())

})
    
   
    
### UI Components 

output$CMH_PIHP<-renderUI({
    
selectInput(inputId = "org",
            label = "Org",
            choices = c("pihp","cmhsp"),
            selected = 'pihp')

})



output$metric<-renderUI({
    
    selectInput(inputId = "metric",
                label = "metric",
                choices = c("cost","units"),
                selected = 'pihp')
    
})



### Map

output$map<-renderLeaflet({
  #  req(data())
    ds<-as.data.frame(data())
    
    static_map(map_type = org_input(), 
            df = ds, 
            col_pallet = "viridis", 
            addtiles = "Stamen.TonerLite", 
            border_col = "red", 
            legend_label ="range")
    

    
    
})
    
    

})
