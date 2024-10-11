library(tidyverse)
library(xml2)
library(igraph)
library(tidygraph)
library(ggraph)

set_graph_style()

# Creating a function to read in graphml and create graph object
read_graphml <- function(path, print = F, graph = FALSE) {
  
  # Create function to deal with xml values
  xml_values_to_character <- function(x) {
    df <- c(x %>%
              as_list() %>%
              data.frame(stringsAsFactors = FALSE) %>%
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
    
    labels = if(length(x_graph %>%
                       xml_find_all(xpath = './/edge//data[@key="d10"]//text()') %>%
                       xml_values_to_character()) == 0) {NA} else {
                         x_graph %>%
                           xml_find_all(xpath = './/edge//data[@key="d10"]//text()') %>%
                           xml_values_to_character()
                       },
    
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
      xml_values_to_character(),
    
    description = sapply(x_graph %>%
                           xml_find_all(xpath = './/node[not(@yfiles.foldertype="group")]'), function(node) {
                             desc <- xml_find_first(node, './/data[@key="d5"]//text()')
                             if (is.na(desc) || xml_text(desc) == "") {
                               return(NA)  # or return("") for an empty string
                             } else {
                               return(xml_text(desc, trim = TRUE))
                             }
                           })
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
  
  # Filter out nodes within containers from container list (with ":" in their IDs)
  container_list <- container_list[!grepl(":", container_list$id), ]
  
  return(list(edge_list = edge_list,
              node_list = node_list,
              container_list = container_list))
}