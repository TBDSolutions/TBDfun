#---------------------- Description ----------------------

# Author:       Samantha Kohtz
# Date:         9-17-24
# Description:  Imports an Excel file and exports a graphml file.

#---------------------- Setup ----------------------
 library(writexl); library(tidyverse); library(purrr);
library(readxl);library(tidygraph);
library(igraph);library(ggraph);

#---------------------- Import and Reformat Excel ----------------------

import <- function(file_path, format = 'Standard') {
  if(!dir.exists(dirname(file_path))) {
    print("The file path does not exist.")
    break
  }
  else if(format == 'Format 7') {
    ex_in <- read_excel(file_path, sheet = 'Sheet1') 
    
    edge_list <- read_excel(file_path, sheet = 'Sheet2') %>%
      select(from, to, id, -labels, -color) %>%
      na.omit() %>%
      distinct()

    node_functions <- ex_in %>%
      select(function_id, function_label) %>%
      na.omit() %>%
      rowwise() %>%
      mutate(
        fill = '#339966',
        labels = function_label,
        id = function_id,
        container_id = case_when(
          length(str_subset(function_id, '::')) == 0 ~ 'no container',
          length(str_subset(function_id, '::')) > 0 ~ strsplit(function_id, "::", fixed = TRUE)[[1]][1]
          )
        ) %>%
      select(-function_id, -function_label) %>%
      distinct()
    node_entities <- ex_in %>%
      select(entity_id, entity_label) %>%
      na.omit() %>%
      mutate(
        fill = '#FFCC00',
        labels = entity_label,
        id = entity_id,
        container_id = case_when(
          length(str_subset(entity_id, '::')) == 0 ~ 'no container',
          length(str_subset(entity_id, '::')) > 0 ~ strsplit(entity_id, "::", fixed = TRUE)[[1]][1]
        )
        ) %>%
      select(-entity_id, -entity_label) %>%
      distinct()
    node_list <- rbind(node_entities, node_functions)
    
    container_list <- ex_in %>%
      select(container_id, container_label) %>%
      na.omit() %>%
      mutate(
        fill = '#F5F5F5',
        id = container_id,
        labels = container_label,
        container_id = 'no container'
      ) %>%
      select(-container_label) %>%
      distinct()
    
    node_list <- rbind(node_list, container_list) %>% distinct() %>% arrange(id) %>% select(id, labels, fill, container_id)
    
  } else if(format == 'Standard') {
    edge_list <- read_excel(file_path, sheet = 'Edges') %>%
      select(from, to, id, -labels, -color) %>%
      na.omit() %>%
      distinct()
    
    node_list <- read_excel(file_path, sheet = 'Nodes') %>%
      select(id, labels, fill, -parent_id, -parent_labels) %>%
      na.omit() %>%
      mutate(
        container_id = case_when(
          length(str_subset(id, '::')) == 0 ~ 'no container',
          length(str_subset(id, '::')) > 0 ~ strsplit(id, "::", fixed = TRUE)[[1]][1]
        )
      ) %>%
      distinct()
    
    container_list <- read_excel(file_path, sheet = 'Containers') %>%
      na.omit() %>%
      mutate(container_id = 'no container') %>%
      distinct()
    
    node_list <- rbind(node_list, container_list) %>% distinct() %>% arrange(id) %>% select(id, labels, fill, container_id)
  }
  list(edge_list, node_list, container_list)
}


# # Testing for issues with the lists for creating a graph
# # Ensure all IDs are positive integers
# node_neg <- node_list %>% filter(id <= 0)
# edge_neg <- edge_list %>% filter(from <= 0 | to <= 0)
# 
# # Check for matching IDs
# valid_ids <- node_list$id
# `%nin%` = Negate(`%in%`)
# edge_test <- edge_list %>% filter(from %nin% valid_ids | to %nin% valid_ids)


#---------------------- Create graph from Excel ----------------------
  
file_path <- 'C:\\Users\\samanthak\\OneDrive - TBD Solutions Inc\\Documents\\Sam Files\\MDHHS\\Graphml Project\\Graphml Function-Entity.xlsx'
ex <- import(file_path, format = 'Standard')
edge_list <- data.frame(ex[1])
node_list <- data.frame(ex[2])
container_list <- data.frame(ex[3])

# create graph object:
graph_obj <- tbl_graph(nodes = node_list, edges = edge_list, node_key = 'id')

v(graph_obj)$width = 150
v(graph_obj)$height = 100

# create graphml file:
write_graph(graph_obj, file = "C:\\Users\\samanthak\\OneDrive - TBD Solutions Inc\\Documents\\Sam Files\\MDHHS\\Graphml Project\\tst.graphml", format = "graphml")
  
# Manual changes to be made in yEd:
#   * Go to "Edit -> Properties Mapper" and run the "Import from Excel" configuration.
#     * This maps fill = Fill color, labels = Label Text, height = Height, width = Width
#   * Select "Layout -> One-Click Layout" to reformat the nodes
#   * Manually add groupings (containers):
#     * Go to "Tools -> Select Elements -> Nodes" and enter "Select: Data Property", "Data Property: ", and "Text: *container_id*"
#     * Then go to "Grouping -> Group" to group those nodes
#     * Rename the container with the container label; Find this in the container_list data frame (along with all container ids)
#     * Do this for each container

