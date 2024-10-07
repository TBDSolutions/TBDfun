#----------------------Description----------------------

# Author:       Samantha Kohtz
# Date:         8-5-24
# Description:  Imports a graphml file and exports an Excel file containing 
#               lists of edges, nodes, and containers.

#----------------------Setup----------------------
library(igraph); library(writexl); library(tidyverse); library(purrr);

#----------------------Translate Graphml----------------------

# Translate Graphml file to edge list and node list tables

# Function that you'll use is in the TBDFun repo. 
# https://github.com/TBDSolutions/TBDfun/blob/master/R/tbd_graphml_readwrite.R
source("C:\\Users\\samanthak\\Documents\\GitHub\\TBDfun\\R\\tbd_graphml_readwrite.R")

# Save the file path of the graphml file to be analyzed
path = "C:\\Users\\samanthak\\OneDrive - TBD Solutions Inc\\Documents\\Sam Files\\MDHHS\\Graphml Project\\CFA&P_Roles.v5.graphml"

# Read in the Graphml file (attributes such as color and text cannot be blank)
graph <- read_graphml(path)

#---------Edges---------

edge_import <- function(graph, color = NA, label = NA) {
  if(is.na(color) & is.na(label)) {
    e <- graph$edge_list
  } else if(!is.na(color) & is.na(label)) {
    e <- graph$edge_list %>%
      filter(color == color) # Filter edges by color
  } else if(is.na(color) & !is.na(label)) {
    e <- graph$edge_list %>%
      filter(labels == label) # Filter edges by label
  } else {
    e <- graph$edge_list %>%
      filter(color == color & labels == label) # Filter edges by label and color
  }
  e
}

#---------Containers---------

container_import <- function(graph, color = NA, label = NA) {
  if(is.na(color) & is.na(label)) {
    c <- graph$container_list
  } else if(!is.na(color) & is.na(label)) {
    c <- graph$container_list %>%
      filter(fill == color) # Filter containers by color
  } else if(is.na(color) & !is.na(label)) {
    c <- graph$container_list %>%
      filter(labels == label) # Filter containers by label
  } else {
    c <- graph$container_list %>%
      filter(fill == color & labels == label) # Filter containers by label and color
  }
  c
}

#---------Nodes---------

node_import <- function(graph, color = NA, label = NA, container_label = NA) {
  if(is.na(color) & is.na(label) & is.na(container_label)) {
    n <- graph$node_list
  } else if(!is.na(color) & is.na(label) & is.na(container_label)) {
    n <- graph$node_list %>%
      filter(fill == color) # Filter nodes by color
  } else if(is.na(color) & !is.na(label) & is.na(container_label)) {
    n <- graph$node_list %>%
      filter(labels == label) # Filter nodes by label
  } else if(is.na(color) & is.na(label) & !is.na(container_label)) {
    n <- graph$node_list %>%
      filter(grepl((container_list %>% filter(labels == container_label))$id, id)) # Filter nodes by container labels
  } else if(is.na(color) & !is.na(label) & !is.na(container_label)) {
    n <- graph$node_list %>%
      filter(grepl((container_list %>% filter(labels == container_label))$id, id) & labels == label) # Filter nodes by label and by container labels
  } else if(!is.na(color) & is.na(label) & !is.na(container_label)) {
    n <- graph$node_list %>%
      filter(grepl((container_list %>% filter(labels == container_label))$id, id) & fill == color) # Filter nodes by color and by container labels
  } else if(!is.na(color) & !is.na(label) & is.na(container_label)) {
    n <- graph$node_list %>%
      filter(labels == label & fill == color) # Filter nodes by color and by label
  } else {
    n <- graph$node_list %>%
      filter(fill == color & labels == label & grepl((container_list %>% filter(labels == container_label))$id, id)) # Filter nodes by label, color, and container labels
  }
  n
}

  # rowwise %>% # Add id to the label
  # mutate(labels = paste(as.numeric(substring(id, 2, coalesce(str_locate(id, ':')-1, nchar(id)))[1]) + # Add id to the label
  #                         coalesce(as.numeric(substring(id, str_locate(id, '::')[2]+2, nchar(id)))*.1, 0), labels)) # Add id to the label
  
#---------Format Export---------

export <- function(edge_list = NA, node_list = NA, container_list = NA, format = 'Standard') {
  if(format == 'Standard' & 
     (!(exists("edge_list") && is.data.frame(get("edge_list"))) | 
      !(exists("node_list") && is.data.frame(get("node_list"))) | 
      !(exists("container_list") && is.data.frame(get("container_list"))))) {
    ex <- "Error: Not all necessary components supplied"
  } else if(format == 'Standard' & 
            (exists("edge_list") && is.data.frame(get("edge_list"))) &
            (exists("node_list") && is.data.frame(get("node_list"))) &
            (exists("container_list") && is.data.frame(get("container_list")))) {
    ex <- list(Edges = edge_list
               , Nodes = node_list
               , Containers = container_list)
  } else if(format == 'Format 7' & 
            (!(exists("edge_list") && is.data.frame(get("edge_list"))) | 
             !(exists("node_list") && is.data.frame(get("node_list"))) | 
             !(exists("container_list") && is.data.frame(get("container_list"))))) {
    ex <- "Error: Not all necessary components supplied"
  } else if(format == 'Format 7' & 
            (exists("edge_list") && is.data.frame(get("edge_list"))) &
            (exists("node_list") && is.data.frame(get("node_list"))) &
            (exists("container_list") && is.data.frame(get("container_list")))) {
    # Entity nodes
    entities <- node_list %>%
      filter(fill == '#FFCC00') # Filter nodes by color
    # Function nodes
    functions <- node_list %>%
      filter(fill == '#339966') # Filter nodes by color
    # How nodes
    hows <- node_list %>%
      filter(fill == '#FF9900') # Filter nodes by color
    # Relevant edges
    edges <- edge_list %>%
      filter((to %in% functions$id & from %in% entities$id) | (to %in% hows$id))
    
    container_id <- character()
    container_label <- character()
    function_id <- character()
    function_label <- character()
    entity_id <- character()
    entity_label <- character()
    hows_id <- character()
    hows_label <- character()
    
    for(f in 1:nrow(functions)) {
      # Map functions to their entities based on the linking edges
      for(e in 1:nrow(entities)) {
        for(ed in 1:nrow(edges)) {
          if(edges$to[ed] == functions$id[f] & edges$from[ed] == entities$id[e]) {
            function_id[f] <- functions$id[f]
            function_label[f] <- functions$labels[f]
            entity_id[f] <- entities$id[e]
            entity_label[f] <- entities$labels[e]
            break
          } else if(ed == nrow(edges) & e == nrow(entities) & f > coalesce(length(function_id), 0)) {
            function_id[f] <- functions$id[f]
            function_label[f] <- functions$labels[f]
            entity_id[f] <- NA
            entity_label[f] <- NA
            break
          } 
        }
      }
      # Map functions to their containers based on ID
      for(c in 1:nrow(container_list)) {
        if(length(str_subset(functions$id[f], container_list$id[c])) > 0) {
          container_id[f] <- container_list$id[c]
          container_label[f] <- container_list$labels[c]
          # Map containers to their hows
          for(h in 1:nrow(hows)) {
            for(ed in 1:nrow(edges)) {
              if(edges$from[ed] == container_list$id[c] & edges$to[ed] == hows$id[h] & 
                 length(str_subset(hows_id[f], paste(hows$id[h], ';'))) < 1) {
                hows_id[f] <- if(is.na(hows_id[f])) {paste(hows$id[h], ';')} else{paste(hows_id[f], hows$id[h], ';')}
                hows_label[f] <- if(is.na(hows_label[f])) {paste(hows$labels[h], ';')} else{paste(hows_label[f], hows$labels[h], ';')}
                break
              }
            }
          }
          break
        } else if(c == nrow(container_list)){
          container_id[f] <- NA
          container_label[f] <- NA
        }
      }
      # Map functions to their hows
      for(h in 1:nrow(hows)) {
        for(ed in 1:nrow(edges)) {
          if(edges$from[ed] == functions$id[f] & edges$to[ed] == hows$id[h] & 
             length(str_subset(hows_id[f], paste(hows$id[h], ';'))) < 1) {
            hows_id[f] <- if(is.na(hows_id[f])) {paste(hows$id[h], ';')} else{paste(hows_id[f], hows$id[h], ';')}
            hows_label[f] <- if(is.na(hows_label[f])) {paste(hows$labels[h], ';')} else{paste(hows_label[f], hows$labels[h], ';')}
            break
          } 
        }
      }
    }
    
    ex <- list(
      data.frame(cbind(entity_id, entity_label, container_id, container_label, function_id, function_label, hows_id, hows_label)),
      edge_list
    )
  } else {
    ex <- 'Error: data entered incorrectly'
  }
  ex
}
  
#---------------------- Collect and Export Data ----------------------
  
edge_list = edge_import(graph, label = NA, color = NA)
container_list <- container_import(graph, label = NA, color = NA)
node_list = node_import(graph, label = NA, color = NA, container_label = NA)

# Format options: "Standard" or "Format 7"
e <- export(edge_list = edge_list, node_list = node_list, container_list = container_list, format = 'Format 7')
export_path <- 'C:\\Users\\samanthak\\OneDrive - TBD Solutions Inc\\Documents\\Sam Files\\MDHHS\\Graphml Project\\Graphml Test Export.xlsx'
write_xlsx(e, export_path)



