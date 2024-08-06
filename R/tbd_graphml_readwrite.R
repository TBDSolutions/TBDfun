library(tidyverse)
library(xml2)
library(igraph)
library(tidygraph)
library(ggraph)

set_graph_style()

## creating a function to read in graphml and create graph object

read_graphml <- function(path, print = F, graph = FALSE) {

### create function to deal with xml words
  
  xml_values_to_character <- function(x){library(tidyverse)
    library(xml2)
    library(igraph)
    library(tidygraph)
    library(ggraph)
    
    set_graph_style()
    
    ## creating a function to read in graphml and create graph object
    
    read_graphml <- function(path, print = F, graph = FALSE) {
      
      ### create function to deal with xml words
      
      xml_values_to_character <- function(x){
        
        df = c(x %>%
                 as_list() %>%
                 data.frame() %>%
                 t())
        
        return(df)
        
      }
      
      xml_yed <- read_xml(path)
      
      x_graph <- xml_yed %>%
        xml_ns_strip() %>%
        xml_find_all(xpath = "//graph")
      
      edge_list <- data.frame(
        
        id = x_graph %>%
          xml_find_all(xpath = './/edge//@id') %>%
          xml_values_to_character(),
        
        from = x_graph %>%
          xml_find_all(xpath = './edge/@source') %>%
          xml_values_to_character(),
        
        to = x_graph %>%
          xml_find_all(xpath = './edge/@target') %>%
          xml_values_to_character(),
        
        labels = x_graph %>%
          xml_find_all(xpath = './/edge//data[@key="d10"]//text()') %>%
          xml_values_to_character(),
        
        color = x_graph %>%
          xml_find_all(xpath = './/edge//y:LineStyle//@color') %>%
          xml_values_to_character()
        
      )
      
      node_list <- data.frame(
        
        id = x_graph %>%
          xml_find_all(xpath = './/node[not(@yfiles.foldertype="group")]//@id') %>%
          xml_values_to_character(),
        
        labels = x_graph %>%
          xml_find_all(xpath = './/node[not(@yfiles.foldertype="group")]//data[@key="d6"]//text()') %>%
          xml_values_to_character(),
        
        fill = x_graph %>%
          xml_find_all(xpath = './/node[not(@yfiles.foldertype="group")]//y:Fill//@color') %>%
          xml_values_to_character()
        
      )
      
      container_list <- data.frame(
        
        id = x_graph %>%
          xml_find_all(xpath = './/node[@yfiles.foldertype="group"]//@id') %>%
          xml_values_to_character(),
        
        labels = x_graph %>%
          xml_find_all(xpath = './/node[@yfiles.foldertype="group"]//data[@key="d6"]//text()') %>%
          xml_values_to_character(),
        
        fill = x_graph %>%
          xml_find_all(xpath = './/node[@yfiles.foldertype="group"]//y:Fill//@color') %>%
          xml_values_to_character()
      )
      
      #graph_obj <- tbl_graph(nodes = node_list, edges = edge_list)
      
      # Filter out nodes within containers from container list (with ":" in their IDs)
      container_list <- container_list[!grepl(":", container_list$id), ]
      
      return(list(edge_list = edge_list, node_list = node_list, container_list = container_list,
                  graph_obj = graph_obj))
      
    }
    # 
    # df <- read_graphml("graphs/yed_output.graphml")
    
    
    ## OUTPUT 
    # 
    # edge_list = df$edge_list
    # node_list = df$node_list
    # graph_obj = df$graph_obj
    
    ## Create basic graphml file from igraph object 
    ### so we can load back into yeD
    
    # write_graph(graph_obj, file = "graphs/test.graphml", format = "graphml")
    
    df = c(x %>%
             as_list() %>%
             data.frame() %>%
             t())
    
    return(df)
    
  }

xml_yed <- read_xml(path)

x_graph <- xml_yed %>%
  xml_ns_strip() %>%
  xml_find_all(xpath = "//graph")

edge_list <- data.frame(
    
    id = x_graph %>%
      xml_find_all(xpath = './/edge//@id') %>%
      xml_values_to_character(),
    
    from = x_graph %>%
      xml_find_all(xpath = './edge/@source') %>%
      xml_values_to_character(),
    
    to = x_graph %>%
      xml_find_all(xpath = './edge/@target') %>%
      xml_values_to_character(),
    
    labels = x_graph %>%
      xml_find_all(xpath = './/edge//data[@key="d10"]//text()') %>%
      xml_values_to_character(),
    
    color = x_graph %>%
      xml_find_all(xpath = './/edge//y:LineStyle//@color') %>%
      xml_values_to_character()
    
  )

node_list <- data.frame(
  
  id = x_graph %>%
    xml_find_all(xpath = './/node//@id') %>%
    xml_values_to_character(),
  
  labels = x_graph %>%
    xml_find_all(xpath = './/node//data[@key="d6"]//text()') %>%
    xml_values_to_character(),
  
  fill = x_graph %>%
    xml_find_all(xpath = './/node//y:Fill//@color') %>%
    xml_values_to_character()
  
  
)

graph_obj <- tbl_graph(nodes = node_list, edges = edge_list)

return(list(edge_list = edge_list, node_list = node_list,
            graph_obj = graph_obj))

}
# 
# df <- read_graphml("graphs/yed_output.graphml")


## OUTPUT 
# 
# edge_list = df$edge_list
# node_list = df$node_list
# graph_obj = df$graph_obj

## Create basic graphml file from igraph object 
### so we can load back into yeD

# write_graph(graph_obj, file = "graphs/test.graphml", format = "graphml")


