library(tidyverse)
library(vroom)
library(igraph)
library(rlist)
library(tidygraph)
library(ggraph)
library(data.table)


load_data <- function(path){
  # Input path to folder with data being read
  # Loads data and converts date column to type "date"
  # Sorts df by date
  # Returns sorted df
  
  
  clean <- function (file) {
    # Input filename
    # Loads the large dataset
    # Formats dates to the type date
    # Returns formated df
    df_new <- read_csv(file)
    
    df_new <- df_new %>%
      mutate(date = as.Date(date, format = "%b %d, %Y")) %>%
      lapply(as.character)
    
    return(df_new)
  }
  
  data <- list.files(path="Data", full.names = TRUE) %>%
    lapply(clean) %>%
    bind_rows
  
  tweets_all <- data
  
  dates_ord <- tweets_all %>%
    arrange(date)
  
  return(dates_ord)
}



remove_singles <- function(relations){
  # Input edge list as a df type
  # Removes the conversations with only one participant from the list of edges
  # Returns new df
  
  convo_list <- relations$conversation_id
  
  t <- data.frame(table(convo_list))
  t <- t[t$Freq==1,]
  
  relations <- relations[!(relations$conversation_id %in% t$convo_list),]
  return(relations)
}


setwd("~/Work Stuff/Gulio Summer Project")

dates_ord <- load_data("Data")

start_date <- as.Date(dates_ord$date[1])
end_date <- as.Date(tail(dates_ord$date, 1))

week_dif <- difftime(end_date,
                     start_date,
                     units="weeks")





exactdegree_randomized_edges <- function(data){
  random_order_convos <- sample(data$conversation_id)
  return(data.frame(username = data$username, conversation_id = random_order_convos))
}

nondegree_randomized_edges <- function(data){
  random_order_convos <- sample(unique(data$conversation_id),size = length(data$conversation_id), replace = T)
  random_order_user <- sample(unique(data$username),size = length(data$username), replace = T)
  return(data.frame(username = random_order_user, conversation_id = random_order_convos))
}

expecteddegree_randomized_edges <- function(data){
  random_order_convos <- sample(unique(data$conversation_id),size = length(data$conversation_id), 
                                prob = table(data$conversation_id) / length(data$conversation_id), replace=T)
  
  random_order_users <- sample(unique(data$username),size = length(data$username), 
                                prob = table(data$username) / length(data$username), replace=T)
  return(data.frame(username = random_order_users, conversation_id = random_order_convos))
}

make_graph_projection <- function(data){
  G <- graph.data.frame(data)
  V(G)$type <- V(G)$name %in% data$username
  projection <- bipartite.projection(G, V(G)$type, which = T, multiplicity = TRUE)
  
  return(projection)
}

run_unip_for_loop <- function(num_randomizations=NULL, randomized_edges=NULL){
  # Input the number of randomly generated null models (int)
  # if "num_randomizations is NULL, the function will give observed statistics
  # else it
  # Iterates over each week and generates the stats for the null model (average of the random models)
  # Writes csv for each week containing the stats of each generated graph
  # Returns the null model stats
  
  cur_start <- start_date
  cur_end <- start_date + 7
  
  
  stats <- tibble(Week_start = character(),
                  Week_end = character(),
                  Week = numeric(),
                  Net_size = numeric(),
                  Density = numeric(),
                  Diameter = numeric(),
                  Avg_distance = numeric(),
                  Global_clustering = numeric(),
                  Average_clustering = numeric())
  
  distinct_and_single <- function(data){
    d <- data %>%
      distinct %>%
      remove_singles
    return(d)
  }
  
  set.seed(1234)
  for (i in 1:week_dif) {
    print(paste(as.character(i),"of",as.character(week_dif)))
    graph_data <- dates_ord %>%
      filter(date >= cur_start & date < cur_end) %>%
      select(username, conversation_id) %>%
      na.omit()
    
    if(is.null(num_randomizations)){
      # Calculates the statistics of the observed graph
      graph_data <- distinct_and_single(graph_data)
      
      projection <- make_graph_projection(graph_data)
      
      stats <- stats %>% add_row(Week_start = as.character(cur_start),
                                 Week_end = as.character(cur_end),
                                 Week = i,
                                 Net_size = gsize(projection),
                                 Density = edge_density(projection),
                                 Diameter = diameter(projection, weights = NA),
                                 Avg_distance = mean_distance(projection),
                                 Global_clustering = transitivity(projection),
                                 Average_clustering = transitivity(projection, type="average"))
    }
    else{
      
      sample_tibble <- tibble(Density = numeric(),
                              Diameter = numeric(),
                              Avg_distance = numeric(),
                              Global_clustering = numeric(),
                              Average_clustering = numeric(),
                              Week = numeric())
      
      
      for (j in 1:num_randomizations){
        print(j)
        null <- randomized_edges(graph_data)
        null <- distinct_and_single(null)
        
        null_projection <- make_graph_projection(null)
        
        
        
        sample_tibble <- sample_tibble %>% add_row(Density = edge_density(null_projection),
                                                   Diameter = diameter(null_projection, weights = NA),
                                                   Avg_distance = mean_distance(null_projection),
                                                   Global_clustering = transitivity(null_projection),
                                                   Average_clustering = transitivity(null_projection, type="average"),
                                                   Week = i)
        
      }
      
      write_csv(sample_tibble, sprintf("%s.csv", i))
      
      stats <- stats %>% add_row(Week_start = as.character(cur_start),
                                 Week_end = as.character(cur_end),
                                 Week = i,
                                 Net_size = gsize(null_projection),
                                 Density = mean(sample_tibble$Density),
                                 Diameter = mean(sample_tibble$Diameter),
                                 Avg_distance = mean(sample_tibble$Avg_distance),
                                 Global_clustering = mean(sample_tibble$Global_clustering),
                                 Average_clustering = mean(sample_tibble$Average_clustering))
    }
    
    
    
    
    cur_start <- cur_start + 7
    cur_end <- cur_end + 7
  }
  return(stats)
}

